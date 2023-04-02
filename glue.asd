(defsystem "glue"
  :version "0.1.0"
  :author "remimimimimi"
  :license "AGPL"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "glue/tests"))))

(defsystem "glue/tests"
  :author "remimimimimi"
  :license "AGPL"
  :depends-on ("glue"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for glue"
  :perform (test-op (op c) (symbol-call :rove :run c)))
