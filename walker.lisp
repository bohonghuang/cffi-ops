(in-package #:cffi-ops)

(defvar *value-required* t)

(defvar *type-dictionary* nil)

(defvar *struct-slots* nil)

(defvar *macro-environment* nil)

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
  (setf form (macroexpand form *macro-environment*))
  (etypecase form
    (symbol (values (assoc-value *type-dictionary* form) form))
    (list (destructuring-ecase form
            ((the type tform)
             (declare (ignore type))
             (values (form-type tform) form))
            ((%cthe ctype cform)
             (assert (constantp ctype *macro-environment*))
             (values (eval ctype) cform))
            ((foreign-alloc ctype &rest args)
             (declare (ignore args))
             (assert (constantp ctype *macro-environment*))
             (values (eval ctype) form))))))

(declaim (inline %cthe))
(defun %cthe (ctype form)
  (declare (ignore ctype))
  (values form))

(defun expand-slot (slot form)
  (multiple-value-bind (type form)
      (form-type (let ((*value-required* nil)) (expand-form form)))
    (loop :for parsed-type := (cffi::ensure-parsed-base-type type)
          :for expansions :from 0
          :while (typep parsed-type 'cffi::foreign-pointer-type)
          :when (plusp expansions)
            :do (setf form `(mem-ref ,form ',(cffi::unparse-type type)))
          :do (setf type (cffi::unparse-type (cffi::pointer-type parsed-type))))
    (let ((rtype (foreign-slot-type type slot)))
      (if *value-required*
          `(%cthe ',rtype (foreign-slot-value ,form ',type ',slot))
          `(%cthe '(:pointer ,rtype) (foreign-slot-pointer ,form ',type ',slot))))))

(defun expand-aref (pointer index)
  (multiple-value-bind (type pointer)
      (form-type (let ((*value-required* t)) (expand-form pointer)))
    (let ((index (let ((*value-required* t)) (expand-form index)))
          (rtype (cffi::unparse-type (cffi::pointer-type (cffi::ensure-parsed-base-type type)))))
      (if *value-required*
          `(%cthe ',rtype (mem-aref ,pointer ',rtype ,index))
          `(%cthe ',type (mem-aptr ,pointer ',rtype ,index))))))

(defun expand-ref (form)
  (multiple-value-bind (type form) (form-type (let ((*value-required* nil)) (expand-form form)))
    `(%cthe ',type ,form)))

(defun expand-form (form)
  (typecase form
    (cons
     (destructuring-case form
       (((declare quote %cthe) &rest args) (declare (ignore args)) form)
       (((let let*) bindings &rest body)
        (list* (car form)
               (mapcar (lambda (binding)
                         (typecase binding
                           (symbol binding)
                           (list (list (first binding) (expand-form (second binding))))))
                       bindings)
               (mapcar #'expand-form body)))
       ((-> &rest args) (declare (ignore args)) (expand-form (macroexpand form *macro-environment*)))
       (([] pointer &optional (index 0)) (expand-aref pointer index))
       ((& form) (expand-ref form))
       ((t &rest args)
        (if (find (car form) *struct-slots*)
            (expand-slot (car form) (first args))
            (cons (car form) (let ((*value-required* t)) (mapcar #'expand-form args)))))))
    (t form)))
