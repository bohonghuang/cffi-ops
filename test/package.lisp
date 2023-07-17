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

(define-test suite
  (clet ((m1 (:array (:struct matrix3) 3)))
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
