(defsystem cffi-ops
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "Apache-2.0"
  :description "A library that helps write concise CFFI-related code. "
  :homepage "https://github.com/bohonghuang/cffi-ops"
  :bug-tracker "https://github.com/bohonghuang/cffi-ops/issues"
  :source-control (:git "https://github.com/bohonghuang/cffi-ops.git")
  :components ((:file "package")
               (:file "walker" :depends-on ("package"))
               (:file "setf" :depends-on ("package" "walker"))
               (:file "macros" :depends-on ("package" "walker")))
  :depends-on (#:alexandria #:cffi #:arrow-macros #:trivial-macroexpand-all)
  :in-order-to ((test-op (test-op #:cffi-ops/test))))

(defsystem cffi-ops/test
  :depends-on (#:cffi-ops #:parachute)
  :pathname "./test/"
  :components ((:file "package"))
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:suite) '#:cffi-ops.test))))
