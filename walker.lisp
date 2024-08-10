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
                    (ctype-slots (cffi::pointer-type ctype)))
                   (cffi::foreign-array-type
                    (ctype-slots (cffi::element-type ctype)))))))
      (remove-duplicates (mapcan #'ctype-slots types)))))

(defun pointer-type-p (type)
  (typep (cffi::ensure-parsed-base-type type) 'cffi::foreign-pointer-type))

(defun ensure-pointer-type (type)
  (setf type (cffi::ensure-parsed-base-type type)
        type (typecase type
               (cffi::foreign-pointer-type (cffi::pointer-type type))
               (t type)))
  `(:pointer
    ,(typecase type
       (cffi::foreign-array-type (cffi::element-type type))
       (cffi::foreign-type (cffi::unparse-type type))
       (t type))))

(defgeneric funcall-form-type (function args)
  (:method (function args) (declare (ignore function args)))
  (:method ((function (eql 'foreign-alloc)) args)
    (destructuring-bind (type) args
      (assert (constantp type *macro-environment*))
      `(:pointer ,(eval type)))))

(defun form-type (form)
  (setf form (macroexpand form *macro-environment*))
  (etypecase form
    (symbol (values (assoc-value *type-dictionary* form) form))
    (list (destructuring-case form
            ((the type tform)
             (declare (ignore type))
             (values (form-type tform) form))
            ((%cthe ctype cform)
             (assert (constantp ctype *macro-environment*))
             (values (eval ctype) cform))
            ((t &rest args)
             (declare (ignore args))
             (values (funcall-form-type (car form) (cdr form)) form))))))

(declaim (inline %cthe))
(defun %cthe (ctype form)
  (declare (ignore ctype))
  (values form))

(define-compiler-macro %cthe (ctype form)
  (declare (ignore ctype)) form)

(defun expand-slot (slot form)
  (multiple-value-bind (type form)
      (let ((*value-required* nil)) (form-type (expand-form form)))
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
      (multiple-value-bind (value-type value-form)
          (let ((*value-required* t))
            (form-type (expand-form pointer)))
        (when (typep (cffi::ensure-parsed-base-type value-type) 'cffi::foreign-array-type)
          (setf (values value-type value-form) (let ((*value-required* nil)) (form-type (expand-form pointer)))
                value-type (cadr value-type))) ; (:pointer (:array ...)) -> (:array ...)
        (values value-type value-form))
    (let ((index (let ((*value-required* t)) (expand-form index))))
      (multiple-value-bind (type rtype)
          (let ((rtype (cffi::ensure-parsed-base-type type)))
            (etypecase rtype
              (cffi::foreign-pointer-type
               (values type (cffi::unparse-type (cffi::pointer-type rtype))))
              (cffi::foreign-array-type
               (values `(:pointer ,(cffi::element-type rtype)) ; (:array ...) -> (:pointer ...)
                       (cffi::element-type rtype)))))
        (if *value-required*
            `(%cthe ',rtype (mem-aref ,pointer ',rtype ,index))
            `(%cthe ',type (mem-aptr ,pointer ',rtype ,index)))))))

(defun expand-ref (form)
  (multiple-value-bind (type form)
      (let ((*value-required* nil))
        (form-type (expand-form form)))
    `(%cthe ',type ,form)))

(defun expand-form (form)
  (typecase form
    (cons
     (destructuring-case form
       (((declare quote) &rest args) (declare (ignore args)) form)
       (((let let*) bindings &rest body)
        (list* (car form)
               (mapcar (lambda (binding)
                         (typecase binding
                           (symbol binding)
                           (list (list (first binding) (expand-form (second binding))))))
                       bindings)
               (mapcar #'expand-form body)))
       (((flet labels) bindings &rest body)
        (list* (car form)
               (mapcar
                (lambda (binding)
                  (destructuring-bind (name lambda-list &rest body) binding
                    `(,name ,lambda-list . ,(mapcar #'expand-form body))))
                bindings)
               (mapcar #'expand-form body)))
       ((%cthe type form) `(%cthe ,type ,(expand-form form)))
       ((-> init &rest args) (declare (ignore args))
        (let ((*struct-slots* (if-let ((type (form-type (expand-form init))))
                                (nconc (ctypes-slots (list type)) *struct-slots*)
                                *struct-slots*)))
          (expand-form (macroexpand form *macro-environment*))))
       (([] pointer &optional (index 0)) (expand-aref pointer index))
       ((& form) (expand-ref form))
       ((t &rest args)
        (cond
          ((find (car form) *struct-slots*) (expand-slot (car form) (first args)))
          ((proper-list-p args) (cons (car form) (let ((*value-required* t)) (mapcar #'expand-form args))))
          (t form)))))
    (t form)))

(defgeneric funcall-dynamic-extent-form (function args)
  (:method (function args) (declare (ignore function args)))
  (:method ((function (eql 'foreign-alloc)) args)
    (destructuring-bind (type) args
      (lambda (var body)
        (assert (constantp type *macro-environment*))
        `(with-foreign-object (,var ',(eval type)) . ,body)))))
