(in-package #:cffi-ops)

(define-setf-expander %cthe (ctype form &environment env)
  (declare (ignore ctype))
  (get-setf-expansion (expand-form form) env))

(define-setf-expander -> (init &rest exps &environment env)
  (get-setf-expansion (expand-form `(-> ,init . ,exps)) env))

(define-setf-expander [] (pointer &optional (index 0) &environment env)
  (get-setf-expansion (expand-form `([] ,pointer ,index)) env))
