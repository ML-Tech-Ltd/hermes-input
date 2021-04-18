(defpackage hermes-input.db
  (:use :cl)
  (:export #:conn)
  (:nicknames :ominp.db))
(in-package :hermes-input.db)

(defmacro conn (&rest body)
  `(postmodern:with-connection (list ,omcom.db:*db-name* ,omcom.db:*db-user* ,omcom.db:*db-pass* ,omcom.db:*db-hostname*)
     ,@body))
