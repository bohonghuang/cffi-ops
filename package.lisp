(defpackage cffi-ops
  (:use #:cl #:alexandria #:arrow-macros #:cffi #:trivial-macroexpand-all)
  (:import-from #:cffi #:ctype))

(in-package #:cffi-ops)

(defun ctypes-slots (types)
  (let ((type-table (make-hash-table)))
    (labels ((ctype-slots (ctype)
               (setf ctype (cffi::ensure-parsed-base-type ctype))
               (unless (gethash ctype type-table)
                 (typecase ctype
                   (cffi::foreign-struct-type
                    (loop :for slot-name :in (cffi:foreign-slot-names ctype)
                          :for slot-type := (cffi:foreign-slot-type ctype slot-name)
                          :do (setf (gethash slot-type type-table) t)
                          :nconc (cons slot-name (ctype-slots slot-type))))
                   (cffi::foreign-pointer-type
                    (ctype-slots (cffi::pointer-type ctype)))))))
      (remove-duplicates (mapcan #'ctype-slots types)))))

(defvar *type-dictionary* nil)

(defvar *bound-macros* nil)

(defvar *value-required* t)

(declaim (inline cthe))
(defun cthe (ctype form)
  (declare (ignore ctype))
  (values form))

(define-setf-expander cthe (ctype form &environment env)
  (multiple-value-bind (dummies vals newval setter getter) (get-setf-expansion form env)
    (declare (ignore newval setter))
    (with-gensyms (store)
      (values dummies vals `(,store) `(setf ,getter ,store) `(cthe ,ctype ,getter)))))

(defun form-type (form)
  (etypecase form
    (symbol (values form (assoc-value *type-dictionary* form)))
    (list (destructuring-ecase form
            ((cthe self-type self-form) (values self-form (eval self-type)))))))

(defun slot-access-form (slot form &optional env &aux type)
  (setf (values form type) (let ((*value-required* nil)) (form-type (macroexpand form env))))
  (loop :for parsed-type := (cffi::ensure-parsed-base-type type)
        :for expansions :from 0
        :while (typep parsed-type 'cffi::foreign-pointer-type)
        :when (plusp expansions)
          :do (setf form `(mem-ref ,form ',(cffi::unparse-type type)))
        :do (setf type (cffi::pointer-type parsed-type)))
  (let ((rtype (foreign-slot-type type slot)))
    (if *value-required*
        `(cthe ',rtype (foreign-slot-value ,form ',(cffi::unparse-type type) ',slot))
        `(cthe '(:pointer ,rtype) (foreign-slot-pointer ,form ',(cffi::unparse-type type) ',slot)))))

(defun array-access-form (pointer index &optional env &aux type)
  (setf (values pointer type) (let ((*value-required* t)) (form-type (macroexpand pointer env)))
        type (cffi::ensure-parsed-base-type type))
  (let ((rtype (cffi::pointer-type (cffi::ensure-parsed-base-type type))))
    (if *value-required*
        `(cthe ',(cffi::unparse-type rtype) (mem-aref ,pointer ',(cffi::unparse-type rtype) ,index))
        `(cthe ',(cffi::unparse-type type) (mem-aptr ,pointer ',(cffi::unparse-type rtype) ,index)))))

(defun ref-form (form &optional env &aux type)
  (setf (values form type) (let ((*value-required* nil)) (form-type (macroexpand form env))))
  `(cthe ',type ,form))

(defun deref-form (form &optional env)
  (let ((*value-required* t))
    (array-access-form form 0 env)))

(defmacro clocally (&body body &environment env)
  (let* ((declarations (loop :for (declaration . rest) :on body
                             :while (eq (car declaration) 'declare)
                             :do (setf body rest)
                             :append (cdr declaration))))
    (multiple-value-bind (ctypes declarations)
        (loop :for declaration :in declarations
              :if (eq (car declaration) 'ctype)
                :collect (cdr declaration) :into ctypes
              :else
                :collect declaration :into declarations
              :finally (return (values ctypes declarations)))
      (let* ((types (mapcan (lambda (type-vars)
                              (mapcar (rcurry #'cons (car type-vars)) (cdr type-vars)))
                            ctypes))
             (slots (ctypes-slots (mapcar #'cdr types))))
        (let ((macros (loop :for slot :in (set-difference slots *bound-macros*)
                            :collect `(,slot (form &environment env) (slot-access-form ',slot form env)))))
          (let ((*bound-macros* (nconc slots *bound-macros*))
                (*type-dictionary* (nconc types *type-dictionary*)))
            (macroexpand-all
             `(locally (declare . ,declarations)
                (macrolet (([] (pointer index &environment env) (array-access-form pointer index env))
                           (& (form &environment env) (ref-form form env))
                           ($ (form &environment env) (deref-form form env))
                           . ,macros)
                  . ,body))
             (when (consp (second env)) env))))))))
