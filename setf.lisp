(in-package #:cffi-ops)

(define-setf-expander %cthe (ctype form &environment env)
  (multiple-value-bind (vars vals newval setter getter) (get-setf-expansion form env)
    (declare (ignore newval setter))
    (with-gensyms (store)
      (values vars vals `(,store) `(setf ,getter ,store) `(%cthe ,ctype ,form)))))

(define-setf-expander -> (init &rest exps &environment env)
  (multiple-value-bind (vars vals newval setter getter) (get-setf-expansion (expand-form `(-> ,init . ,exps)) env)
    (declare (ignore vars vals newval setter))
    (with-gensyms (store)
      (values nil nil `(,store) `(setf ,getter ,store) getter))))

(define-setf-expander [] (pointer &optional (index 0) &environment env)
  (multiple-value-bind (vars vals newval setter getter) (get-setf-expansion (expand-form `([] ,pointer ,index)) env)
    (declare (ignore newval setter))
    (with-gensyms (store)
      (values vars vals `(,store) `(setf ,getter ,store) getter))))
