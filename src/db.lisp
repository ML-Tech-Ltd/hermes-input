(defpackage hermes-input.db
  (:use :cl)
  (:export #:conn)
  (:nicknames :hsinp.db))
(in-package :hermes-input.db)

(defmacro conn (&rest body)
  `(postmodern:with-connection (list ,hscom.db:*db-name* ,hscom.db:*db-user* ,hscom.db:*db-pass* ,hscom.db:*db-hostname*)
     ,@body))
