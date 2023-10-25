(in-package #:cffi-ops)

(declaim (inline memcpy))
(defcfun "memcpy" :void
  (dest :pointer)
  (src :pointer)
  (n :size))

(defmacro cthe (ctype form)
  "Similar to THE, but declares the CFFI type for FORM."
  `(%cthe ',ctype ,form))

(defun body-declarations (body)
  (values
   (loop :for (declaration . rest) :on body
         :while (and (listp declaration) (eq (car declaration) 'declare))
         :do (setf body rest)
         :append (cdr declaration))
   body))

(defmacro clocally (&body body &environment *macro-environment*)
  "Similar to LOCALLY but allows using CTYPE to declare CFFI types for variables."
  (multiple-value-bind (body-declarations body) (body-declarations body)
    (multiple-value-bind (ctypes declarations)
        (loop :for declaration :in body-declarations
              :if (eq (car declaration) 'ctype)
                :collect (cdr declaration) :into ctypes
              :else :if (eq (car declaration) 'dynamic-extent)
                :do (progn)
              :else
                :collect declaration :into declarations
              :finally (return (values ctypes declarations)))
      (let* ((types (mapcan (lambda (type-vars)
                              (mapcar (rcurry #'cons (car type-vars)) (cdr type-vars)))
                            ctypes))
             (slots (ctypes-slots (mapcar #'cdr types))))
        (let ((*type-dictionary* (nconc types *type-dictionary*))
              (*struct-slots* (nconc slots *struct-slots*)))
          `(%cthe nil ,(let ((*value-required* t))
                         (expand-form (macroexpand-all `(locally (declare . ,declarations) . ,body) #-ecl *macro-environment*)))))))))

(defmacro clet (bindings &body body &environment *macro-environment*)
  "Equivalent to variable definition and initialization statements in C, but with type inference. For each element (NAME FORM) of BINDINGS, NAME is always bound to a CFFI pointer, with the following cases for different FORMs:
- A variable with pointer type: NAME is directly bound to this pointer variable.
- A variable with non-pointer type: The variable is copied onto the stack semantically, and the bound pointer is pointed to it."
  #+sbcl (declare (sb-ext:muffle-conditions warning))
  (multiple-value-bind (body-declarations body)
      (body-declarations body)
    (loop :with type :and form
          :with dynamic-extent-vars := (mapcan (lambda (declaration)
                                                 (when (eq (car declaration) 'dynamic-extent)
                                                   (cdr declaration)))
                                               body-declarations)
          :for (name val) :in bindings
          :when (consp val)
            :do (destructuring-case val
                  ((foreign-alloca type)
                   (push name dynamic-extent-vars)
                   (setf val `(foreign-alloc ,type))))
          :do (setf (values type form) (form-type (expand-form val)))
          :collect (cons name (ensure-pointer-type type)) :into types
          :collect (let ((name name) (val val) (type type))
                     (compose
                      (cond
                        ((member name dynamic-extent-vars)
                         (lambda (body)
                           (with-gensyms (var)
                             (funcall (funcall-dynamic-extent-form (car val) (cdr val)) var `((let ((,name ,var)) ,@body))))))
                        ((pointer-type-p type)  (lambda (body) `(let ((,name ,val)) . ,body)))
                        (t (lambda (body) `(with-foreign-object (,name ',type) (csetf (%cthe '(:pointer ,type) ,name) ,val) . ,body))))
                      #'list))
            :into forms
          :finally
             (return
               (reduce #'funcall forms
                       :from-end t
                       :initial-value `(clocally
                                         (declare
                                          . ,(mapcar (lambda (type)
                                                       `(ctype ,(cdr type) ,(car type)))
                                                     types))
                                         (declare . ,(remove 'dynamic-extent body-declarations :key #'car))
                                         . ,body))))))

(defmacro clet* (bindings &body body)
  "Similar to CLET, but the initialization FORM of the variable can use variables defined earlier."
  (if bindings
      `(clet (,(car bindings))
         (declare . ,(remove (caar bindings) (body-declarations body) :key #'cdr :test-not #'member))
         (clet* ,(cdr bindings)
           . ,body))
      `(clocally . ,body)))

(defmacro csetf (&rest args &environment *macro-environment*)
  "Equivalent to assignment statements in C, which assign the rvalue of each pair to the lvalue. Note that both the lvalues and rvalues are represented as CFFI pointers here, and the assignment operation is actually a memory copy."
  (when args
    (destructuring-bind (var val &rest args) args
      (multiple-value-bind (ltype lform) (form-type (let ((*value-required* nil)) (expand-form var)))
        (multiple-value-bind (rtype rform) (form-type (let ((*value-required* nil)) (expand-form val)))
          (assert (eq (cffi::ensure-parsed-base-type (cffi::pointer-type (cffi::ensure-parsed-base-type ltype)))
                      (cffi::ensure-parsed-base-type (cffi::pointer-type (cffi::ensure-parsed-base-type rtype)))))
          `(progn
             (memcpy ,lform ,rform ,(foreign-type-size (cffi::unparse-type (cffi::pointer-type (cffi::ensure-parsed-base-type ltype)))))
             (csetf . ,args)))))))
