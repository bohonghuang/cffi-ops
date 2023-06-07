(defpackage cffi-ops
  (:use #:cl #:alexandria #:arrow-macros #:cffi #:trivial-macroexpand-all)
  (:import-from #:cffi #:ctype)
  (:export #:cthe #:clocally #:clet #:clet* #:& #:-> #:[]))

(in-package #:cffi-ops)

(defvar *value-required* t)

(defvar *type-dictionary* nil)

(defvar *struct-slots* nil)

(declaim (inline memcpy))
(defcfun "memcpy" :void
  (dest :pointer)
  (src :pointer)
  (n :size))

(declaim (inline free))
(defcfun "free" :void
  (ptr :pointer))

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

(defun form-type (form)
  (etypecase form
    (symbol (values (assoc-value *type-dictionary* form) form))
    (list (destructuring-ecase form
            ((cthe ctype cform) (values (eval ctype) cform))))))

(declaim (inline cthe))
(defun cthe (ctype form)
  (declare (ignore ctype))
  (values form))

(define-setf-expander cthe (ctype form &environment env)
  (multiple-value-bind (dummies vals newval setter getter) (get-setf-expansion form env)
    (declare (ignore newval setter))
    (with-gensyms (store)
      (values dummies vals `(,store) `(setf ,getter ,store) `(cthe ,ctype ,getter)))))

(define-setf-expander -> (init &rest exps &environment env)
  (multiple-value-bind (dummies vals newval setter getter) (get-setf-expansion (walk-form `(-> ,init . ,exps)) env)
    (declare (ignore newval setter))
    (with-gensyms (store)
      (values dummies vals `(,store) `(setf ,getter ,store) `(-> ,getter . ,exps)))))

(define-setf-expander & (form &environment env)
  (multiple-value-bind (dummies vals newval setter getter) (get-setf-expansion (let ((*value-required* nil)) (walk-form form)) env)
    (declare (ignore newval setter))
    (with-gensyms (store)
      (values dummies vals `(,store)
              `(memcpy ,getter ,store
                       ,(foreign-type-size
                         (cffi::unparse-type
                          (cffi::pointer-type
                           (cffi::ensure-parsed-base-type
                            (form-type getter))))))
              `(& ,getter)))))

(defun expand-slot (slot form)
  (multiple-value-bind (type form)
      (form-type (let ((*value-required* nil)) (walk-form form)))
    (loop :for parsed-type := (cffi::ensure-parsed-base-type type)
          :for expansions :from 0
          :while (typep parsed-type 'cffi::foreign-pointer-type)
          :when (plusp expansions)
            :do (setf form `(mem-ref ,form ',(cffi::unparse-type type)))
          :do (setf type (cffi::unparse-type (cffi::pointer-type parsed-type))))
    (let ((rtype (foreign-slot-type type slot)))
      (if *value-required*
          `(cthe ',rtype (foreign-slot-value ,form ',type ',slot))
          `(cthe '(:pointer ,rtype) (foreign-slot-pointer ,form ',type ',slot))))))

(defun expand-aref (pointer index)
  (multiple-value-bind (type pointer)
      (form-type (let ((*value-required* t)) (walk-form pointer)))
    (let ((index (let ((*value-required* t)) (walk-form index)))
          (rtype (cffi::unparse-type (cffi::pointer-type (cffi::ensure-parsed-base-type type)))))
      (if *value-required*
          `(cthe ',rtype (mem-aref ,pointer ',rtype ,index))
          `(cthe ',type (mem-aptr ,pointer ',rtype ,index))))))

(defun expand-ref (form)
  (multiple-value-bind (type form) (form-type (let ((*value-required* nil)) (walk-form form)))
    `(cthe ',type ,form)))

(defun walk-form (form)
  (typecase form
    (cons
     (destructuring-case form
       (((declare quote) &rest args) (declare (ignore args)) form)
       (((let let*) bindings &rest body)
        (list* (car form)
               (mapcar (lambda (binding)
                         (typecase binding
                           (symbol binding)
                           (list (list (first binding) (walk-form (second binding))))))
                       bindings)
               (mapcar #'walk-form body)))
       ((cthe ctype form) (declare (ignore ctype)) form)
       ((-> &rest args) (declare (ignore args)) (walk-form (macroexpand form)))
       (([] pointer &optional (index 0)) (expand-aref pointer index))
       ((& form) (expand-ref form))
       ((t &rest args)
        (if (find (car form) *struct-slots*)
            (expand-slot (car form) (first args))
            (cons (car form) (let ((*value-required* t)) (mapcar #'walk-form args)))))))
    (t form)))

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
        (let ((*type-dictionary* (nconc types *type-dictionary*))
              (*struct-slots* (nconc slots *struct-slots*)))
          `(cthe nil ,(let ((*value-required* t))
                        (walk-form (macroexpand-all `(locally (declare . ,declarations) . ,body) #-ecl env)))))))))

(defmacro clet (bindings &body body)
  (loop :with type :and form
        :for (name var) :in bindings
        :if (not (and (listp var) (keywordp (first var))))
          :do (setf (values type form) (form-type (walk-form var)))
          :and :collect `(let ((,name ,form))) :into forms
          :and :collect (cons name type) :into types
        :else :if (eq (first var) :pointer)
          :collect `(let ((,name (foreign-alloc ',(second var))))) :into forms
          :and :collect (cons name var) :into types
        :else
          :collect `(with-foreign-object (,name ',var)) :into forms
          :and :collect (cons name `(:pointer ,var)) :into types
        :finally
           (return
             (reduce (lambda (x acc) (nconc x (list acc))) forms
                     :from-end t
                     :initial-value `(clocally
                                       (declare
                                        . ,(mapcar (lambda (type)
                                                     `(ctype ,(cdr type) ,(car type)))
                                                   types))
                                       . ,body)))))

(defmacro clet* (bindings &body body)
  (if bindings
      `(clet (,(car bindings))
         (clet* ,(cdr bindings)
           . ,body))
      `(progn . ,body)))
