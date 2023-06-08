(in-package #:cffi-ops)

(declaim (inline memcpy))
(defcfun "memcpy" :void
  (dest :pointer)
  (src :pointer)
  (n :size))

(defmacro clocally (&body body &environment env)
  "Similar to LOCALLY but allows using CTYPE to declare CFFI types for variables."
  (let* ((declarations (loop :for (declaration . rest) :on body
                             :while (and (listp declaration) (eq (car declaration) 'declare))
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
                        (expand-form (macroexpand-all `(locally (declare . ,declarations) . ,body) #-ecl env)))))))))

(defmacro clet (bindings &body body &environment env)
  "Equivalent to variable definition and initialization statements in C, but with type inference. For each element (NAME FORM) of BINDINGS, NAME is always bound to a CFFI pointer, with the following cases for different FORMs:
- A type of CFFI: An uninitialized variable of that type is created on the stack, and the bound pointer is pointed to it.
- A variable with pointer type: NAME is directly bound to this pointer variable.
- A variable with non-pointer type: The variable is copied onto the stack, and the bound pointer is pointed to it."
  (flet ((pointer-type-p (type)
           (typep (cffi::ensure-parsed-base-type type) 'cffi::foreign-pointer-type))
         (ensure-pointer-type (type)
           (setf type (cffi::ensure-parsed-base-type type)
                 type (typecase type
                        (cffi::foreign-pointer-type (cffi::pointer-type type))
                        (cffi::foreign-array-type (cffi::element-type type))
                        (t type)))
           `(:pointer
             ,(typecase type
                (cffi::foreign-type (cffi::unparse-type type))
                (t type)))))
    (loop :with type :and form
          :for (name var) :in bindings
          :if (or (keywordp var) (and (listp var) (or (keywordp (first var)) (eq (first var) 'foreign-alloc))))
            :if (and (listp var) (eq (first var) 'foreign-alloc))
              :collect `(let ((,name ,var))) :into forms
              :and :when (constantp (second var) env)
                :collect (cons name (ensure-pointer-type (eval (second var)))) :into types
              :end
            :else
              :collect `(with-foreign-object (,name ',var)) :into forms
              :and :collect (cons name (ensure-pointer-type var)) :into types
            :end
          :else
            :do (setf (values type form) (form-type (expand-form var)))
            :and :collect `(,@(if (pointer-type-p type)
                                  `(let ((,name ,var)))
                                  `(with-foreign-object (,name ',type)))
                            ,@(unless (pointer-type-p type)
                                `((csetf (cthe '(:pointer ,type) ,name) ,var))))
                   :into forms
            :and :collect (cons name (ensure-pointer-type type)) :into types
          :finally
             (return
               (reduce (lambda (x acc) (nconc x (list acc))) forms
                       :from-end t
                       :initial-value `(clocally
                                         (declare
                                          . ,(mapcar (lambda (type)
                                                       `(ctype ,(cdr type) ,(car type)))
                                                     types))
                                         . ,body))))))

(defmacro clet* (bindings &body body)
  "Similar to CLET, but the initialization FORM of the variable can use variables defined earlier."
  (if bindings
      `(clet (,(car bindings))
         (clet* ,(cdr bindings)
           . ,body))
      `(clocally . ,body)))

(defmacro csetf (&rest args)
  "Equivalent to assignment statements in C, which assign the rvalue of each pair to the lvalue. Note that both the lvalues and rvalues are represented as CFFI pointers here, and the assignment operation is actually a memory copy."
  (when args
    (destructuring-bind (var val &rest args) args
      (multiple-value-bind (ltype lform) (form-type (let ((*value-required* nil)) (expand-form var)))
        (multiple-value-bind (rtype rform) (form-type (let ((*value-required* nil)) (expand-form val)))
          (assert (eql (cffi::ensure-parsed-base-type (cffi::pointer-type (cffi::ensure-parsed-base-type ltype)))
                       (cffi::ensure-parsed-base-type (cffi::pointer-type (cffi::ensure-parsed-base-type rtype)))))
          `(progn
             (memcpy ,lform ,rform ,(foreign-type-size (cffi::unparse-type (cffi::pointer-type (cffi::ensure-parsed-base-type ltype)))))
             (csetf . ,args)))))))
