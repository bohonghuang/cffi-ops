(defpackage cffi-ops.test
  (:use #:cl #:parachute #:cffi #:cffi-ops))

(in-package #:cffi-ops.test)

(defcstruct vector3
  (x :float)
  (y :float)
  (z :float))

(defcstruct matrix3
  (v1 (:struct vector3))
  (v2 (:struct vector3))
  (v3 (:struct vector3)))

(defun vector3-add (output v1 v2)
  (clocally
    (declare (ctype (:pointer (:struct vector3)) v1 v2))
    (setf (-> (cthe (:pointer (:struct vector3)) output) x) (+ (-> v1 x) (-> v2 x))
          (-> (cthe (:pointer (:struct vector3)) output) y) (+ (-> v1 y) (-> v2 y))
          (-> (cthe (:pointer (:struct vector3)) output) z) (+ (-> v1 z) (-> v2 z)))))

(define-test suite)

(define-test simple :parent suite
  (clet ((m1 (foreign-alloc '(:array (:struct matrix3) 3))))
    (declare (dynamic-extent m1))
    (setf (-> ([] m1 0) v1 x) 1.0
          (-> ([] m1 0) v1 y) 2.0
          (-> ([] m1 0) v1 z) 3.0)
    (clet* ((m2 ([] m1))
            (v1 (& (-> m2 v1)))
            (v2 (foreign-alloc '(:struct vector3))))
      (csetf ([] v2) ([] v1))
      (setf (-> v2 x) 3.0
            (-> v2 z) 1.0)
      (vector3-add v1 v1 v2)
      (is = (-> v1 x) 4.0)
      (is = (-> v1 y) 4.0)
      (is = (-> v1 z) 4.0)
      (foreign-free v2))))

(define-test nested-array :parent suite
  (clet ((m (foreign-alloc '(:array (:array :float 3) 3))))
    (declare (dynamic-extent m))
    (loop :for row :below 3
          :do (loop :for col :below 3
                    :do (setf ([] ([] m row) col) (+ (* row 3.0) col))))
    (clet ((m1 ([] (cthe (:pointer (:struct matrix3)) m))))
      (is = 0.0 (-> (-> m1 v1) x))
      (is = 1.0 (-> (-> m1 v1) y))
      (is = 2.0 (-> (-> m1 v1) z))
      (is = 3.0 (-> (-> m1 v2) x))
      (is = 4.0 (-> (-> m1 v2) y))
      (is = 5.0 (-> (-> m1 v2) z))
      (is = 6.0 (-> (-> m1 v3) x))
      (is = 7.0 (-> (-> m1 v3) y))
      (is = 8.0 (-> (-> m1 v3) z)))))
