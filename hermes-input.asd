(defsystem "hermes-input"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:ciel
               :cl-dates
               :flexi-streams
               :drakma
               :postmodern

               :hermes-common)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config"))
                 (:file "api" :depends-on ("rates"))
                 (:file "rates")
                 (:file "config"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "hermes-input/tests"))))

(defsystem "hermes-input/tests"
  :author ""
  :license ""
  :depends-on ("hermes-input"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for hermes-input"

  :perform (test-op (op c) (symbol-call :rove :run c)))
