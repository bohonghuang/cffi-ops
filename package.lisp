(defpackage cffi-ops
  (:use #:cl #:alexandria #:arrow-macros #:cffi #:trivial-macroexpand-all)
  (:import-from #:cffi #:ctype)
  (:export #:ctype #:cthe #:clocally #:clet #:clet* #:csetf #:& #:-> #:[] #:foreign-alloca)
  (:nicknames #:cops))

(in-package #:cffi-ops)
