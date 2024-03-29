(defsystem "clos-shape"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "clos-shape/tests"))))

(defsystem "clos-shape/tests"
  :author ""
  :license ""
  :depends-on ("clos-shape"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for clos-shape"
  :perform (test-op (op c) (symbol-call :rove :run c)))
