(defpackage hermes-input.api
  (:use #:cl #:postmodern)
  (:import-from #:hsinp.db
		#:conn)
  (:import-from #:hsinp.rates
		#:get-rates-count-from-big
		#:get-rates-count-big
		#:get-rates-random-count-big
		#:get-rates-range-big)
  (:export #:get-rates)
  (:nicknames #:hsinp.api))
(in-package :hermes-input.api)

(defun whent (condition then)
  "WHENT behaves like `cl:when` but returns `(sql t)` if the condition is
  not met instead of `nil`."
  (if condition
      then
      (sql t)))

(defun -get-rates (&key instrument timeframe from to)
  (sql (:select '* :from 'rates
		:where (:and
			(:raw (whent instrument (sql (:= 'instrument instrument))))
			(:raw (whent timeframe (sql (:= 'timeframe timeframe))))
			(:raw (whent from (sql (:>= 'time from))))
			(:raw (whent to (sql (:<= 'time to))))))))

(defun get-rates (&key instrument timeframe from to limit)
  (conn (query (:raw (if limit
			 (sql (:limit (:raw (-get-rates :instrument instrument :timeframe timeframe :from from :to to)) '$1))
			 (-get-rates :instrument instrument :timeframe timeframe :from from :to to)
			 ))
	       limit :alists
	       )))
