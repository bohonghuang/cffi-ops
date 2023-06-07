(defpackage cffi-ops
  (:use #:cl #:alexandria #:arrow-macros #:cffi #:trivial-macroexpand-all)
  (:import-from #:cffi #:ctype)
  (:export #:ctype #:cthe #:clocally #:clet #:clet* #:csetf #:& #:-> #:[]))

(in-package #:cffi-ops)

(defvar *value-required* t)

(defvar *type-dictionary* nil)

(defvar *struct-slots* nil)

(declaim (inline memcpy))
(defcfun "memcpy" :void
  (dest :pointer)
  (src :pointer)
  (n :size))

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
  (multiple-value-bind (vars vals newval setter getter) (get-setf-expansion form env)
    (declare (ignore newval setter))
    (with-gensyms (store)
      (values vars vals `(,store) `(setf ,getter ,store) `(cthe ,ctype ,form)))))

(define-setf-expander -> (init &rest exps &environment env)
  (multiple-value-bind (vars vals newval setter getter) (get-setf-expansion (expand-form `(-> ,init . ,exps)) env)
    (declare (ignore newval setter))
    (with-gensyms (store)
      (values vars vals `(,store) `(setf ,getter ,store) getter))))

(define-setf-expander [] (pointer &optional (index 0) &environment env)
  (multiple-value-bind (vars vals newval setter getter) (get-setf-expansion (expand-form `([] ,pointer ,index)) env)
    (declare (ignore newval setter))
    (with-gensyms (store)
      (values vars vals `(,store) `(setf ,getter ,store) getter))))

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
          `(cthe ',rtype (foreign-slot-value ,form ',type ',slot))
          `(cthe '(:pointer ,rtype) (foreign-slot-pointer ,form ',type ',slot))))))

(defun expand-aref (pointer index)
  (multiple-value-bind (type pointer)
      (form-type (let ((*value-required* t)) (expand-form pointer)))
    (let ((index (let ((*value-required* t)) (expand-form index)))
          (rtype (cffi::unparse-type (cffi::pointer-type (cffi::ensure-parsed-base-type type)))))
      (if *value-required*
          `(cthe ',rtype (mem-aref ,pointer ',rtype ,index))
          `(cthe ',type (mem-aptr ,pointer ',rtype ,index))))))

(defun expand-ref (form)
  (multiple-value-bind (type form) (form-type (let ((*value-required* nil)) (expand-form form)))
    `(cthe ',type ,form)))

(defun expand-form (form)
  (typecase form
    (cons
     (destructuring-case form
       (((declare quote cthe) &rest args) (declare (ignore args)) form)
       (((let let*) bindings &rest body)
        (list* (car form)
               (mapcar (lambda (binding)
                         (typecase binding
                           (symbol binding)
                           (list (list (first binding) (expand-form (second binding))))))
                       bindings)
               (mapcar #'expand-form body)))
       ((-> &rest args) (declare (ignore args)) (expand-form (macroexpand form)))
       (([] pointer &optional (index 0)) (expand-aref pointer index))
       ((& form) (expand-ref form))
       ((t &rest args)
        (if (find (car form) *struct-slots*)
            (expand-slot (car form) (first args))
            (cons (car form) (let ((*value-required* t)) (mapcar #'expand-form args)))))))
    (t form)))

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

(defmacro clet (bindings &body body)
  "Equivalent to variable definition and initialization statements in C, but with type inference. For each element (NAME FORM) of BINDINGS, NAME is always bound to a CFFI pointer, with the following cases for different FORMs:
- A pointer type of CFFI: An uninitialized variable of the pointer target type is created on the heap, and the bound pointer is pointed to it. Note that FOREIGN-FREE needs to be used to manually release memory.
- A non-pointer types of CFFI: An uninitialized variable of that type is created on the stack, and the bound pointer is pointed to it.
- A variable with pointer type: NAME is directly bound to this pointer variable.
- A variables with non-pointer type: The variable is copied onto the stack, and the bound pointer is pointed to it."
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
          :if (or (keywordp var) (and (listp var) (keywordp (first var))))
            :if (and (listp var) (eq (first var) :pointer))
              :collect `(let ((,name (foreign-alloc ',(second var))))) :into forms
              :and :collect (cons name (ensure-pointer-type var)) :into types
            :else
              :collect `(with-foreign-object (,name ',var)) :into forms
              :and :collect (cons name (ensure-pointer-type var)) :into types
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
