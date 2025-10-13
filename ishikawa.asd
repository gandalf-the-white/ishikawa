(in-package :asdf-user)

(defsystem "ishikawa"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on (eazy-gnuplot)
  :components ((:module "src"
                :components
                ((:file "algebra")
                 (:file "perceptron" :depends-on ("algebra"))
                 (:file "blobs")
                 (:file "gnuplot")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "ishikawa/tests"))))

(defsystem "ishikawa/tests"
  :author ""
  :license ""
  :depends-on ("ishikawa"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "algebra")
                 (:file "perceptron" :depends-on ("algebra"))
                 (:file "blobs")
                 (:file "gnuplot")
                 (:file "main"))))
  :description "Test system for ishikawa"
  :perform (test-op (op c) (symbol-call :rove :run c)))
