(in-package :cl-user)
(defpackage hermes-input.init
  (:use :cl)
  (:import-from :hscom.config
                #:cfg<)
  (:nicknames #:hsinp.init))
(in-package :hermes-input.init)

;; Loading configuration for Hermes Input.
(cfg< :hsinp)
