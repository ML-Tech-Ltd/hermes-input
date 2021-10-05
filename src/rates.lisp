(defpackage hermes-input.rates
  (:use #:cl #:alexandria #:postmodern)
  (:import-from #:hscom.utils
		#:format-table
		#:assoccess
		#:comment
		#:random-int)
  (:import-from #:hsinp.config
		#:*tiingo-token*)
  (:import-from #:hscom.db
		#:conn)
  (:export #:->close
	   #:->high
	   #:->low
	   #:->open
	   #:->close-frac
	   #:->high-frac
	   #:->low-frac
	   #:->close-bid
	   #:->close-ask
	   #:->low-bid
	   #:->low-ask
	   #:->high-bid
	   #:->high-ask
	   #:->open-bid
	   #:->open-ask
	   #:init
	   #:to-pips
	   #:from-pips
	   #:fracdiff
	   #:unix-from-nano
	   #:unix-to-nano
	   #:insert-rates
	   #:get-rates-count-from-big
	   #:get-rates-count-big
	   #:get-rates-random-count-big
	   #:get-rates-range-big
	   #:update-creation-training-dataset
	   #:sync-datasets-to-database
	   #:sync-datasets-from-database
	   #:get-datasets
	   #:format-datasets
	   #:score-rates
	   #:get-types-score
	   #:get-rates-chunk-of-types
	   #:winning-type-output-dataset
	   #:get-input-dataset
	   #:get-output-dataset
	   #:get-rates-batches
	   #:get-rates-range
	   #:get-rates-count
	   #:get-rates-count-from
	   #:get-random-rates-count
	   #:get-tp-sl
	   #:*creation-training-datasets*)
  (:nicknames #:hsinp.rates))
(in-package :hermes-input.rates)

(defparameter *creation-training-datasets* (make-hash-table :test 'equal :synchronized t))

(defun unix-from-nano (unix-nano &optional (is-string? nil))
  (if is-string?
      (/ (read-from-string unix-nano) 1000000)
      (/ unix-nano 1000000)))

(defun unix-to-nano (unix &optional (is-string? nil))
  (if is-string?
      (* (read-from-string unix) 1000000)
      (* unix 1000000)))

(defclass dataset ()
  ((id :col-type string :initarg :id)
   (begin-time :col-type int8 :initarg :begin-time)
   (end-time :col-type int8 :initarg :end-time))
  (:metaclass postmodern:dao-class)
  (:table-name datasets)
  (:keys id))

(defclass rate ()
  ((time :col-type string :initarg :time)
   (instrument :col-type string :initarg :instrument)
   (timeframe :col-type string :initarg :timeframe)
   (open-bid :col-type double-float :initarg :open-bid)
   (open-ask :col-type double-float :initarg :open-ask)
   (high-bid :col-type double-float :initarg :high-bid)
   (high-ask :col-type double-float :initarg :high-ask)
   (low-bid :col-type double-float :initarg :low-bid)
   (low-ask :col-type double-float :initarg :low-ask)
   (close-bid :col-type double-float :initarg :close-bid)
   (close-ask :col-type double-float :initarg :close-ask)
   (volume :col-type int :initarg :volume))
  (:metaclass postmodern:dao-class)
  (:table-name rates)
  (:keys time instrument timeframe))

(defun ->close (rate)
  (/ (+ (assoccess rate :close-bid)
	(assoccess rate :close-ask))
     2))

(defun ->high (rate)
  (/ (+ (assoccess rate :high-bid)
	(assoccess rate :high-ask))
     2))

(defun ->low (rate)
  (/ (+ (assoccess rate :low-bid)
	(assoccess rate :low-ask))
     2))

(defun ->open (rate)
  (/ (+ (assoccess rate :open-bid)
	(assoccess rate :open-ask))
     2))

(defun ->close-frac (rate)
  "Returns the fractionally differentiatied rate-close."
  (assoccess rate :close-frac))

(defun ->high-frac (rate)
  "Returns the fractionally differentiatied rate-high."
  (assoccess rate :high-frac))

(defun ->low-frac (rate)
  "Returns the fractionally differentiatied rate-low."
  (assoccess rate :low-frac))

(defun ->close-bid (rate)
  (assoccess rate :close-bid))

(defun ->close-ask (rate)
  (assoccess rate :close-ask))

(defun ->low-bid (rate)
  (assoccess rate :low-bid))

(defun ->low-ask (rate)
  (assoccess rate :low-ask))

(defun ->high-bid (rate)
  (assoccess rate :high-bid))

(defun ->high-ask (rate)
  (assoccess rate :high-ask))

(defun ->open-bid (rate)
  (assoccess rate :open-bid))

(defun ->open-ask (rate)
  (assoccess rate :open-ask))

(defun init (howmany-batches &key (timeframes '(:H1 :M1)))
  "INIT populates the `rates` table with batches of rates (a batch is
5000 rates) for each instrument in `hscom.hsage:*forex*` for each timeframe
in `timeframes`."
  (let ((instruments hscom.hsage:*forex*))
    (loop for instrument in instruments
	  do (loop for timeframe in timeframes
		   do (let ((rates (get-rates-batches instrument timeframe howmany-batches)))
			(insert-rates instrument timeframe rates))))))
;; (init 1 :timeframes '(:M1))

(defun to-pips (instrument quantity)
  (let ((str-instrument (format nil "~a" instrument)))
    (cond ((or (cl-ppcre:scan "JPY" str-instrument)
	       (cl-ppcre:scan "HUF" str-instrument)
	       (cl-ppcre:scan "KRW" str-instrument)
	       (cl-ppcre:scan "THB" str-instrument))
	   (* quantity 100))
	  ((or (cl-ppcre:scan "CZK" str-instrument)
	       (cl-ppcre:scan "CNY" str-instrument)
	       (cl-ppcre:scan "INR" str-instrument))
	   (* quantity 1000))
	  (t (* quantity 10000)))))
;; (to-pips :USD_CNH 0.0001)

(defun from-pips (instrument quantity)
  (let ((str-instrument (format nil "~a" instrument)))
    (cond ((or (cl-ppcre:scan "JPY" str-instrument)
	       (cl-ppcre:scan "HUF" str-instrument)
	       (cl-ppcre:scan "KRW" str-instrument)
	       (cl-ppcre:scan "THB" str-instrument))
	   (/ quantity 100))
	  ((or (cl-ppcre:scan "CZK" str-instrument)
	       (cl-ppcre:scan "CNY" str-instrument)
	       (cl-ppcre:scan "INR" str-instrument))
	   (/ quantity 1000))
	  (t (/ quantity 10000)))))
;; (float (from-pips :EUR_JPY 100))

(defun -get-weight-ffd (d thresh lim)
  "Used by fracdiff."
  (let ((w '(1.0)))
    (loop for k from 1
	  repeat (1- lim)
	  do (let ((-w (* (/ (- (first w)) k)
			  (+ d (- k) 1))))
	       (when (< (abs -w) thresh)
	       	 (return))
	       (push -w w)
	       ))
    w))
;; (-get-weight-ffd hscom.hsage:*fracdiff-d* hscom.hsage:*fracdiff-threshold* 10)

(defun -dot-product (a b)
  (apply #'+ (mapcar #'* (coerce a 'list) (coerce b 'list))))

(defun fracdiff (rates)
  (let* ((w (-get-weight-ffd hscom.hsage:*fracdiff-d* hscom.hsage:*fracdiff-threshold* (length rates)))
	 (width (1- (length w)))
	 ;; (output (make-list width :initial-element 0.0))
	 )
    (loop for i in (iota (- (length rates) width) :start width)
    	  do (let ((closes (mapcar #'->close (subseq rates (- i width) (1+ i))))
		   (highs (mapcar #'->high (subseq rates (- i width) (1+ i))))
		   (lows (mapcar #'->low (subseq rates (- i width) (1+ i)))))
	       ;; (push (-dot-product w x) output)
	       (push
	       	`(:close-frac . ,(-dot-product w closes))
	       	(nth i rates))
	       (push
	       	`(:high-frac . ,(-dot-product w highs))
	       	(nth i rates))
	       (push
	       	`(:low-frac . ,(-dot-product w lows))
	       	(nth i rates))
	       ))
    (subseq rates width)))
;; (fracdiff hsper::*rates*)

;; (defparameter *rates* (hsinp.rates::fracdiff (hsinp.rates::get-rates-random-count-big :AUD_USD :M15 10000)))

(comment
 (loop for rate in (fracdiff *rates*)
       do (print (assoccess rate :high-bid))))

(defun insert-rates (instrument timeframe rates)
  (conn
   (loop for rate in rates
	 ;; Inserting only if complete.
	 do (when (assoccess rate :complete)
	      (let ((time (assoccess rate :time))
		    (instrument (format nil "~a" instrument))
		    (timeframe (format nil "~a" timeframe)))
		(unless (get-dao 'rate time instrument timeframe)
		  (make-dao 'rate
			    :time time
			    :instrument instrument
			    :timeframe timeframe
			    :open-bid (assoccess rate :open-bid)
			    :open-ask (assoccess rate :open-ask)
			    :high-bid (assoccess rate :high-bid)
			    :high-ask (assoccess rate :high-ask)
			    :low-bid (assoccess rate :low-bid)
			    :low-ask (assoccess rate :low-ask)
			    :close-bid (assoccess rate :close-bid)
			    :close-ask (assoccess rate :close-ask)
			    :volume (assoccess rate :volume)
			    )))))))

(defun pips (n &optional (jpy? nil) (decimal? nil))
  (if decimal?
      (if jpy?
          (/ n 1000)
          (/ n 100000))
      (if jpy?
          (/ n 100)
          (/ n 10000))))

(defun print-rate (rate)
  (format t "~5$~%" rate))

(defun round-to (number precision &optional (what #'round))
  (float (let ((div (expt 10 precision)))
           (/ (funcall what (* number div)) div))))

(defun start-end (howmany)
  (let* ((pre-result (iota (round (/ (+ howmany 5000) 5000)) :start 0 :step 5000))
         (result (if (> (last pre-result) howmany)
                     (concatenate 'list (butlast pre-result) (list howmany))
                     (if (= (last pre-result) howmany)
                         pre-result
                         (concatenate 'list pre-result (list howmany))))))
    result))

(defun timeframe-for-tiingo (timeframe)
  "TODO: Adapt code for Tiingo instead of Oanda."
  (cond
    ((eq timeframe :M5) "5min")
    ((eq timeframe :H1) "1hour")
    ((eq timeframe :H2) "2hour")
    ((eq timeframe :H3) "3hour")
    ((eq timeframe :H4) "4hour")
    ((eq timeframe :D) "24hour")
    ))

(defun instrument-for-tiingo (instrument)
  "TODO: Adapt code for Tiingo instead of Oanda."
  (string-downcase (cl-ppcre:regex-replace-all "_" (format nil "~a" instrument) "")))

(defun get-rates-count-from-big (instrument timeframe count from)
  (let* ((instrument (format nil "~a" instrument))
	 (timeframe (format nil "~a" timeframe))
	 (from-str (format nil "~a" from))
	 (try-db (reverse (conn (query (:limit (:order-by (:select '* :from 'rates :where (:and (:= 'instrument instrument)
												(:= 'timeframe timeframe)
												(:>= 'time from-str)))
							  (:desc 'rates.time))
					       '$1)
				       count
				       :alists)))))
    ;; Checking if we have all the necessary rates on the database already.
    (if (= (length try-db) count)
	try-db
	;; We need to query from broker then.
	(let ((recent (get-rates-count-from instrument timeframe 5000 from :provider :oanda :type :fx))
	      (prev (reverse (conn (query (:limit (:order-by (:select '* :from 'rates :where (:and (:= 'instrument instrument)
												   (:= 'timeframe timeframe)
												   (:>= 'time from-str)))
							     (:desc 'rates.time))
						  '$1)
					  (1- count)
					  :alists)))))
	  ;; Updating DB.
	  (insert-rates instrument timeframe recent)
	  (if (string= (assoccess (last-elt prev) :time)
		       (assoccess (last-elt recent) :time))
	      prev
	      (append prev
		      (last recent)))))))
;; (length (get-rates-count-from-big :EUR_USD :M1 20000 (unix-to-nano (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 40 :day)))))
;; (loop for rate in (get-rates-count-from-big :EUR_USD :M1 20000 (unix-to-nano (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 40 :day))))
;;       do (print (local-time:unix-to-timestamp (/ (read-from-string (assoccess rate :time)) 1000000))))

(defun get-rates-count-big (instrument timeframe count)
  (let ((recent (get-rates-count instrument timeframe 100)))
    ;; Updating DB.
    (insert-rates instrument timeframe recent)
    ;; Getting COUNT - 1 rates, and then appending last rate from RECENT.
    (let ((prev (reverse (conn (query (:limit (:order-by (:select '* :from 'rates :where (:and (:= 'instrument (format nil "~a" instrument))
											       (:= 'timeframe (format nil "~a" timeframe))))
							 ;; TODO: It's not a good idea to sort by time, considering it's a string. The good news is that we don't have to worry about this until year ~2200.
							 (:desc 'rates.time))
					      '$1)
				      (1- count)
				      :alists)))))
      (if (string= (assoccess (last-elt prev) :time)
		   (assoccess (last-elt recent) :time))
	  prev
	  (append prev
		  (last recent))))))
;; (get-rates-count-big :EUR_USD hscom.hsage:*train-tf* 10)

(defun get-rates-random-count-big (instrument timeframe count)
  "Assumes a minimum of 50K rates"
  (let* ((offset (random-int 0 (- 50000 count))))
    (reverse (conn (query (:limit (:order-by (:select '* :from 'rates :where (:and (:= 'instrument (format nil "~a" instrument))
										   (:= 'timeframe (format nil "~a" timeframe))))
					     ;; TODO: It's not a good idea to sort by time, considering it's a string. The good news is that we don't have to worry about this until year ~2200.
					     (:desc 'rates.time))
				  '$1 '$2)
			  count offset
			  :alists)))))
;; (get-rates-random-count-big :EUR_USD hscom.hsage:*train-tf* 10)

(defun get-rates-range-big (instrument timeframe from to)
  (let ((recent (get-rates-count instrument timeframe 100)))
    ;; Updating DB.
    (insert-rates instrument timeframe recent)
    (let ((instrument (format nil "~a" instrument))
	  (timeframe (format nil "~a" timeframe))
	  (from (format nil "~a" from))
	  (to (format nil "~a" to)))
      (reverse (conn (query (:order-by (:select '* :from 'rates :where (:and (:= 'instrument instrument)
									     (:= 'timeframe timeframe)
									     (:>= 'time from)
									     (:<= 'time to)))
				       (:desc 'rates.time))
			    :alists))))))

(defun update-creation-training-dataset (type pattern instrument timeframe start end)
  (setf (gethash (list instrument timeframe pattern type) *creation-training-datasets*)
	(list start end))
  "")

(defun sync-datasets-to-database ()
  (conn
   (loop for id being each hash-key of *creation-training-datasets*
	 for begin-end being each hash-value of *creation-training-datasets*
	 do (let* ((id (format nil "~s" id))
		   (begin-time (first begin-end))
		   (end-time (second begin-end))
		   (dataset (get-dao 'dataset id)))
	      (if dataset
		  ;; Checking if we need to update the dataset.
		  ;; Reading the DB is fast, but writing to it is a costly operation, so it's worth checking.
		  (when (and (/= (slot-value dataset 'begin-time) begin-time)
			     (/= (slot-value dataset 'end-time) end-time))
		    (setf (slot-value dataset 'begin-time) begin-time)
		    (setf (slot-value dataset 'end-time) end-time)
		    (update-dao dataset))
		  ;; It doesn't exist. Let's initialize it.
		  (make-dao 'dataset
			    :id id
			    :begin-time begin-time
			    :end-time end-time))))))

(defun sync-datasets-from-database ()
  (loop for dataset in (conn (query (:select '* :from 'datasets) :alists))
	do (setf (gethash (read-from-string (assoccess dataset :id)) *creation-training-datasets*)
		 (list (assoccess dataset :begin-time)
		       (assoccess dataset :end-time)))))

(defun get-datasets ()
  (let ((datasets (alexandria:hash-table-alist *creation-training-datasets*)))
    (if datasets
	(loop for dataset in datasets
	      collect `((:segment . ,(first dataset))
			(:from . ,(local-time:unix-to-timestamp (unix-from-nano (second dataset))))
			(:to . ,(local-time:unix-to-timestamp (unix-from-nano (third dataset))))))
	nil)))
;; (get-datasets)

(defun format-datasets ()
  (let ((data (get-datasets)))
    (when data
      (let ((datasets (sort data ;; TODO: Sorting in a very, very naughty way.
			    #'string< :key (lambda (elt) (format nil "~a" (assoccess elt :segment)))))
	    (table-labels '("FROM" "TO")))
	(with-open-stream (s (make-string-output-stream))
	  (loop for dataset in datasets
		do (let ((segment (assoccess dataset :segment))
			 (from (assoccess dataset :from))
			 (to (assoccess dataset :to)))
		     (format s "~%~{~a~^, ~}~%" segment)
		     (format s "------------------------~%")
		     (format-table s `((,from
					,to
					))
				   :column-label table-labels)
		     ;; (format s "</pre><hr/>")
		     ))
	  (get-output-stream-string s)
	  )))))
;; (format-datasets)

(defun score-rates (rates &key (lookahead 10) (stagnation-threshold 0.5))
  (let* ((results (loop for i from 0 below (- (length rates) lookahead)
			collect (get-tp-sl (get-output-dataset rates i) lookahead)))
	 (rrs (remove nil (loop for result in results collect (if (= (assoccess result :tp) 0)
								  nil
								  (* (/ (assoccess result :sl)
									(assoccess result :tp))
								     (if (plusp (assoccess result :tp))
									 -1
									 1))))))
	 (stagnated (count-if (lambda (elt) (>= (abs elt) stagnation-threshold)) rrs))
	 (uptrend (count-if (lambda (elt) (and (< (abs elt) stagnation-threshold)
					       (plusp elt)))
			    rrs))
	 (downtrend (count-if (lambda (elt) (and (< (abs elt) stagnation-threshold)
						 (minusp elt)))
			      rrs)))
    (let ((lrates (length rates)))
      `((:bullish . ,(/ uptrend lrates))
    	(:bearish . ,(/ downtrend lrates))
    	(:stagnated . ,(/ stagnated lrates))))))
;; (time (score-rates (get-input-dataset *rates* 20)))

(defun get-types-score (scored-rates types)
  (loop for type in types sum (assoccess scored-rates type)))

(defun get-rates-chunk-of-types (rates types &key (min-chunk-size 100) (max-chunk-size 500)
					       (chunk-step 20) (slide-step 20) (lookahead 10) (stagnation-threshold 0.5))
  (let ((score 0)
	(winner-from 0)
	(winner-to 0))
    (loop for chunk-plus from min-chunk-size below (- max-chunk-size min-chunk-size) by chunk-step
	  do (loop for idx from 0 below (- (length rates) chunk-plus) by slide-step
		   do (let ((s (get-types-score (score-rates (subseq rates idx (+ idx chunk-plus)) :lookahead lookahead :stagnation-threshold stagnation-threshold) types)))
			(when (> s score)
			  (setf score s)
			  (setf winner-from idx)
			  (setf winner-to (+ idx chunk-plus))))))
    (values winner-from winner-to)))

;; (defparameter *rates* (hsinp.rates:fracdiff (hsinp.rates:get-rates-count-big :AUD_USD :H1 10000)))
;; (get-rates-chunk-of-types *rates* '(:bullish) :min-chunk-size 300 :max-chunk-size 601 :slide-step 50)

;; (get-rates-chunk-of-types (subseq *rates* 0 500) '(:bullish))
;; (get-rates-chunk-of-types *rates* '(:bullish))

(defun winning-type-output-dataset (rates type-groups &key (min-dataset-size 24) (max-dataset-size 100) (chunk-step 5) (lookahead 10) (stagnation-threshold 0.5))
  "Calculates what's the ideal index when calling GET-OUTPUT-DATASET, where an ideal index is one that makes GET-OUTPUT-DATASET return a subset of RATES that has a good ratio of uptrend chunks, downtrend chunks and stagnated chunks of RATES."
  (let* ((score 0)
	 (winner-types)
	 (winner-chunk-size))
    (loop for chunk-size from max-dataset-size downto min-dataset-size by chunk-step
	  do (let ((rates (last rates chunk-size)))
	       (loop for types in type-groups
		     do (let ((s (get-types-score
				  (score-rates rates :lookahead lookahead :stagnation-threshold stagnation-threshold)
				  ;; We can send single type, e.g. (:bullish) or collection of types, e.g. '(:bullish :bearish).
				  types)))
			  (when (> s score)
			    (setf score s)
			    (setf winner-chunk-size chunk-size)
			    (setf winner-types types))))))
    winner-types))

(defun get-input-dataset (rates idx)
  (subseq rates 0 idx))

(defun get-output-dataset (rates idx)
  (subseq rates idx))

(defun get-rates-batches (instrument granularity howmany-batches)
  "Gathers prices from Oanda.
A batch = 5,000 rates."
  (labels ((recur (end result counter)
             (let ((candles (ignore-errors
			     (rest (assoc :candles (cl-json:decode-json-from-string
						    (dex:get (format nil "https://api-fxtrade.oanda.com/v3/instruments/~a/candles~
?granularity=~a~
&count=5000~
&end=~a~
&dailyAlignment=0~
&candleFormat=bidask~
&alignmentTimezone=America%2FNew_York"
								     instrument
								     granularity
								     end)
							     :insecure t
							     :headers '(("X-Accept-Datetime-Format" . "UNIX")))))))))
               (sleep 0.5)
               (if (and candles (< counter howmany-batches))
                   (recur (read-from-string
                           (rest (assoc :time (first candles))))
                          ;; (append (mapcar (lambda (candle)
                          ;;                (list (assoc :close-bid candle)
                          ;;                      (assoc :open-bid candle)
                          ;;                      (assoc :high-bid candle)
                          ;;                      (assoc :low-bid candle)
                          ;;                      (assoc :time candle)))
                          ;;              candles)
                          ;;         result)
			  (append candles
                                  result)
                          (incf counter))
                   result))))
  
    (recur (* (local-time:timestamp-to-unix (local-time:now)) 1000000)
           nil
           0)))

(defun get-rates-range (instrument timeframe from to &key (provider :oanda) (type :fx))
  "Requests rates from `PROVIDER` in the range comprised by `FROM` and `TO`."
  (cond ((eq provider :oanda) (oanda-rates-range instrument timeframe from to))
	;; Tiingo is default provider.
	(t (tiingo-rates-range instrument timeframe from to :type type))))

(defun get-rates-count-from (instrument timeframe count from &key (provider :oanda) (type :fx))
  "Requests `COUNT` rates from `PROVIDER` in the starting from `FROM` unix timestamp."
  (oanda-rates-count-from instrument timeframe count from))

;; (get-rates-count-from :EUR_USD :H1 5 (* (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 5 :DAY)) 1000000))

(defun tiingo-rates-range (instrument timeframe start-timestamp end-timestamp &key (type :fx))
  (tiingo-request :type type :instrument instrument :timeframe timeframe :start-timestamp start-timestamp :end-timestamp end-timestamp))

;; (multiple-value-bind (start end) (random-start-date :H1 72)
;;   (defparameter *oanda* (oanda-rates-range :EUR_USD :H1 start end))
;;   (defparameter *tiingo* (tiingo-rates-range :EUR_USD :H1 start end)))

(defun oanda-rates-range (instrument timeframe from to)
  "Requests rates from Oanda in the range comprised by `FROM` and `TO`."
  (let ((from (* from 1000000))
	(to (* to 1000000)))
    (rest (assoc :candles
		 (cl-json:decode-json-from-string
		  (dex:get (format nil "https://api-fxtrade.oanda.com/v3/instruments/~a/candles~
?granularity=~a~
&start=~a~
&end=~a~
&dailyAlignment=0~
&candleFormat=bidask~
&alignmentTimezone=America%2FNew_York"
				   instrument
				   timeframe
				   from
				   to)
			   :insecure t
			   :headers '(("X-Accept-Datetime-Format" . "UNIX"))))))))

(defun oanda-rates-count-from (instrument timeframe count from)
  "Requests rates from Oanda in the range comprised by `FROM` and `TO`."
  (let ((from (* from 1000000)))
    (rest (assoc :candles
		 (cl-json:decode-json-from-string
		  (dex:get (format nil "https://api-fxtrade.oanda.com/v3/instruments/~a/candles~
?granularity=~a~
&start=~a~
&count=~a~
&dailyAlignment=0~
&candleFormat=bidask~
&alignmentTimezone=America%2FNew_York"
				   instrument
				   timeframe
				   from
				   count)
			   :insecure t
			   :headers '(("X-Accept-Datetime-Format" . "UNIX"))))))))

;; (oanda-rates-count-from :EUR_USD :H1 5 (* (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 5 :DAY)) 1000000))

(defun timeframe-for-local-time (timeframe)
  (cond ((eq timeframe :H1) :HOUR)
        ((eq timeframe :D) :DAY)))

(defun get-rates-count (instrument timeframe count &key (provider :oanda) (type :fx))
  "Requests `COUNT` rates from `PROVIDER`."
  (cond ((eq provider :oanda) (oanda-rates-count instrument timeframe count))
	;; Tiingo is default provider.
	(t (tiingo-rates-count instrument timeframe count :type type))))

(defun tiingo-rates-count (instrument timeframe count &key (type :fx))
  (let* ((cal (cl-dates:make-calendar :ny))
	 (start-timestamp (* 1000000 (local-time:timestamp-to-unix
	 			      (local-time:parse-timestring
	 			       (cl-dates:date->string
	 			        (cl-dates:add-workdays
					 (cl-dates:todays-date)
					 cal
					 (- (timeframe-count-to-day-count timeframe count))))))))
	 (end-timestamp (* 1000000 (local-time:timestamp-to-unix (local-time:timestamp+ (local-time:now) 1 :DAY)))))
    (cl:last (tiingo-request :type type :instrument instrument :timeframe timeframe :start-timestamp start-timestamp :end-timestamp end-timestamp) count)))

;; (defparameter *oanda* (oanda-rates-count :EUR_USD :H4 20))
;; (defparameter *tiingo* (tiingo-rates-count :EUR_USD :D 20))

(defun oanda-rates-count (instrument timeframe count)
  "Gathers `COUNT` prices from Oanda."
  (cl:last (rest (assoc :candles
	       (cl-json:decode-json-from-string
		(dex:get (format nil "https://api-fxtrade.oanda.com/v3/instruments/~a/candles~
?granularity=~a~
&count=~a~
&dailyAlignment=0~
&candleFormat=bidask~
&alignmentTimezone=America%2FNew_York"
				 instrument
				 timeframe
				 count)
			 :insecure t
			 :headers '(("X-Accept-Datetime-Format" . "UNIX"))))))
	   count))

(defun add-n-workdays (from-timestamp count)
  "Used by `RANDOM-START-DATE`."
  (let ((cal (cl-dates:make-calendar :ny)))
    (cl-dates:add-workdays (cl-dates:string->date (format nil "~a" from-timestamp))
			   cal
			   count)))

;; 500 days or 500 hours
;; cl-dates works only with days

(defun timeframe-count-to-day-count (timeframe count)
  "`CL-DATES` only handles days, so we need to transform `COUNT` to
equivalent days, depending on `TIMEFRAME`."
  (cond
    ((eq timeframe :M5) (ceiling (/ count 288)))
    ((eq timeframe :H1) (ceiling (/ count 24)))
    ((eq timeframe :H4) (ceiling (/ count 6)))
    ((eq timeframe :H12) (ceiling (/ count 2)))
    ((eq timeframe :D) count)
    ((eq timeframe :W) (ceiling (* count 7)))))

;; (let ((cal (cl-dates:make-calendar :ny))
;;       (max-count 5000)
;;       (timeframe :D)
;;       (count 1))
;;   (cl-dates:date->string
;;    (cl-dates:add-workdays (cl-dates:todays-date)
;; 			  cal
;; 			  (- count))))

(defun random-start-date (timeframe count &optional (max-count 3000))
  "Returns a random unix timestamp from 5000 `TIMEFRAME`s ago to
current time.  The function ensures that at least `COUNT` prices can
be returned using the calculated timestamp."
  (let* ((cal (cl-dates:make-calendar :ny))
	 (from-timestamp (cl-dates:add-workdays (cl-dates:todays-date)
	 					cal
	 					(- (timeframe-count-to-day-count
	 					    timeframe
	 					    (ceiling (+ (1+ count) (* max-count (alexandria:gaussian-random 0 1))))))))
	 (to-timestamp (cl-dates:add-workdays from-timestamp
					      cal
					      (timeframe-count-to-day-count timeframe (1+ count))))
	 )
    (values
     (* 1000000 (local-time:timestamp-to-unix
    		 (local-time:parse-timestring (cl-dates:date->string from-timestamp))))
     (* 1000000 (local-time:timestamp-to-unix
    		 (local-time:parse-timestring (cl-dates:date->string to-timestamp)))))
    ))

;; (random-start-date :D 10)

(defun oanda-random-rates-count (instrument timeframe count)
  (multiple-value-bind (start end) (random-start-date timeframe count)
    (subseq (rest (assoc :candles
			 (cl-json:decode-json-from-string
			  (dex:get (format nil "https://api-fxtrade.oanda.com/v3/instruments/~a/candles~
?granularity=~a~
&start=~a~
&end=~a~
&dailyAlignment=0~
&candleFormat=bidask~
&alignmentTimezone=America%2FNew_York"
					   instrument
					   timeframe
					   start
					   end)
				   :insecure t
				   :headers '(("X-Accept-Datetime-Format" . "UNIX"))))))
	    0 count)))

;; (length (tiingo-random-rates-count :EUR_USD :D 10))

(defun tiingo-random-rates-count (instrument timeframe count &key (type :fx))
  (multiple-value-bind (start end) (random-start-date timeframe count)
    (subseq (tiingo-request :type type :instrument instrument :timeframe timeframe :start-timestamp start :end-timestamp end) 0 count)))

;; (tiingo-random-rates-count :EUR_USD :D 10)

(defun get-random-rates-count (instrument timeframe count &key (provider :oanda) (type :fx))
  "Gathers prices from `PROVIDER`."
  (cond ((eq provider :oanda) (oanda-random-rates-count instrument timeframe count))
	;; Tiingo is default provider.
	(t (tiingo-random-rates-count instrument timeframe count))))

;; (get-random-rates-count :EUR_USD :H1 10 :provider :oanda)
;; (tiingo :iex :ibm :m5)
;; (tiingo :fx :EUR_USD :M5)
;; (tiingo :crypto :cureBTC :D)

(defun construct-uri-prefix-for-tiingo (type)
  (cond ((eq type :iex) "https://api.tiingo.com/")
	(t "https://api.tiingo.com/tiingo/")))

(defun preprocess-rates-for-tiingo (rates)
  ;; (print rates)
  (loop for rate in rates collect
       `((:open-bid . ,(access:access rate :open))
	 (:close-bid . ,(access:access rate :close))
	 (:high-bid . ,(access:access rate :high))
	 (:low-bid . ,(access:access rate :low))
	 (:time . ,(format nil "~a" (* 1000000
				       (local-time:timestamp-to-unix
					(local-time:parse-timestring (access:access rate :date)))))))))

(defun type-for-tiingo (type)
  (string-downcase (format nil "~a" type)))

(defun date-for-tiingo (timestamp)
  (subseq (local-time:to-rfc3339-timestring (local-time:unix-to-timestamp (floor (/ timestamp 1000000)))) 0 10))

(defun tiingo-request (&key (type :FX)
			 (instrument :EUR_USD)
			 (timeframe :H1)
			 start-timestamp
			 end-timestamp)
  (when (or (null start-timestamp)
	    (null end-timestamp))
    ;; If either `START-TIMESTAMP` or `END-TIMESTAMP` are null,
    ;; provide random dates.
    (multiple-value-bind (start end)
	(random-start-date timeframe 48)
      (setf start-timestamp start)
      (setf end-timestamp end)))
  (let ((uri-prefix (construct-uri-prefix-for-tiingo type))
	(type (type-for-tiingo type))
	(instrument (instrument-for-tiingo instrument))
	(timeframe (timeframe-for-tiingo timeframe))
	(start-date (date-for-tiingo start-timestamp))
	(end-date (date-for-tiingo end-timestamp)))
    (preprocess-rates-for-tiingo
     (cl-json:decode-json-from-string
      (flexi-streams:octets-to-string
       (drakma:http-request (format nil "~a~a/~a/prices?startDate=~a&endDate=~a&resampleFreq=~a"
				    uri-prefix
				    type
				    instrument
				    start-date
				    end-date
				    timeframe)
			    :additional-headers `(("Content-Type" . "application/json")
						  ("Accept" . "application/json")
						  ("Authorization" . ,(format nil "Token ~a" *tiingo-token*)))))))))

(defun max-short (plot-results)
  (let* ((profits (mapcar #'second plot-results))
         (highest (first profits))
         (max-short 0))
    (mapcar (lambda (profit)
           (if (> profit highest)
               (setq highest profit)
               (if (> (- highest profit) max-short)
                   (setq max-short (- highest profit)))))
         profits)
    max-short))

(defun gen-genome ()
  (remove-if (lambda (elt) (>= (first elt) (second elt)))
             (apply #'concatenate 'list
                    (mapcar (lambda (inverse?)
                           (apply #'concatenate 'list
                                  (mapcar (lambda (n1)
                                         (remove nil
                                                 (mapcar (lambda (n2)
                                                        (list n1 n2 inverse?)
                                                        )
                                                      ;;omega
                                                      (iota 300 :start 100 :step 1))))
                                       ;;alpha
                                       (iota 1 :start 50 :step 1))))
                         '(t)
                         ))))

;; end optimization algorithm functions

(defun count-lines-in-file (pathname)
  (let* ((in (open pathname :if-does-not-exist nil))
         (counter 0))
    (when in
      (loop for line = (read in nil)
         while line do (incf counter))
      (close in))
    counter))

;; (defun get-data (rates)
;;   (let* ((partition-size 200)
;;          (sample-size (- (length rates)
;;                          partition-size)))
;;     (pmapcar (lambda (close time bounds)
;;                (list close
;;                      (read-from-string time)
;;                      (first (first bounds))
;;                      (first (second bounds))))
;;              (subseq (pmapcar ^(rest (assoc :close-bid %)) rates)
;;                      (1- partition-size)
;;                      (+ sample-size (1- partition-size)))
;;              ;; anadi
;;              (subseq (pmapcar ^(rest (assoc :time %)) rates)
;;                      (1- partition-size)
;;                      (+ sample-size (1- partition-size)))
;;              ;; anadi end
;;              (pmapcar (lambda (subsets)
;;                         (let ((upper (first (rest (first subsets))))
;;                               (lower (first (rest (second subsets)))))
;;                           (if (and (or (< (length upper) 2)
;;                                        (< (length lower) 2)
;;                                        (not upper)
;;                                        (not lower)))
;;                               '("")
;;                               (list (clml-centroids 1 (clml-subset lower))
;;                                     (clml-centroids 1 (clml-subset upper)))
;;                               )))
          
;;                       (pmapcar (lambda (pivot diffs)
;;                                  (get-subsets pivot (fibos diffs)))
;;                                (subseq (pmapcar ^(rest (assoc :close-bid %)) rates)
;;                                        (1- partition-size)
;;                                        (+ sample-size (1- partition-size)))
;;                                (subseq (pmapcar #'diffs (ts-partition partition-size rates))
;;                                        0 sample-size))))))



;; (:HEAT #H(:Z (0 0 1 8 63 4)
;;              :Y (1161/1000 581/500 1163/1000 291/250 233/200 583/500))
;;     :TIME 1.16499
;;     :RATE 1.16522)

;; (get-data :WTICO_USD (get-rates :WTICO_USD 1 :H1))

;; (defparameter *data* (get-rates :EUR_USD 1 :H4))

;; (mapcar (lambda (diffs)
;;        (let ((fibos (fibos diffs)))
;;          (if fibos
;;              (heatmap-values fibos (expt 10 -3)))
;;          )
;;        )
;;      (pmapcar #'diffs (ts-partition 20 *data*))
;;      )

(defun correct-area-position (price area-position)
  (if (< price 0)
      (/ area-position (expt 10 1))
      area-position))

(defun numdigits (n)
  (if (< -10 n 10)
    1
    (1+ (numdigits (truncate n 10)))))

;; (defun heatmap-values (fibos area-size)
;;   ;; area-size is in pips
;;   (let* ((max-area (ceiling (ceiling (first fibos) 1/10000) area-size))
;;          (min-area (floor (ceiling (last fibos) 1/10000) area-size))
;;          (n (- max-area min-area))
;;          (ht (make-hash-table :test #'equal)))
;;     ;; initializing the hash-table
;;     (mapcar (lambda (price)
;;            (setf (gethash ht (/ price 1000)) 0)
;;            )
;;          (iota n :start min-area :step 1))
;;     (mapcar (lambda (fib)
;;            (if (gethash ht (/ (floor (ceiling fib 1/10000) area-size) 1000))
;;                (incf (gethash ht (/ (floor (ceiling fib 1/10000) area-size) 1000))))
;;            )
;;          fibos)
;;     ht))

;; (defun correct-heats (ht-data area-size)
;;   (let (max-rate min-rate)
;;     (mapcar (lambda (data)
;;            (let ((keys (hash-keys (gethash data :heat)))
;;                  mx mn)
;;              (setf mx (apply #'max keys))
;;              (setf mn (apply #'min keys))
;;              (if (or (not max-rate) (> mx max-rate))
;;                  (setf max-rate mx))
;;              (if (or (not min-rate) (< mn min-rate))
;;                  (setf min-rate mn))))
;;          ht-data)
;;     (mapcar (lambda (rate)
;;            (mapcar (lambda (ht)
;;                   ;; (print (gethash (gethash ht :heat) 1.145))
;;                   ;; (print (hash-keys (gethash ht :heat)))
;;                   (if (not (gethash (gethash ht :heat) rate nil))
;;                       (setf (gethash (gethash ht :heat) rate) 0))
;;                   )
;;                 ht-data))
;;          (append (mapcar (lambda (rate)
;;                   (/ rate 1000))
;;                 (iota (- (* max-rate 1000)
;;                     (* min-rate 1000))
;;                       :start (* min-rate 1000)))
;;                  `(,max-rate)))

;;     ;; transforming the heat hash-tables to alists and ordering them
;;     (mapcar (lambda (ht)
;;            (setf (gethash ht :heat) (sort-by-price (gethash ht :heat))))
;;          ht-data)
;;     ht-data))

(defun hash-table-top-n-values (table n)
  "Returns the top N entries from hash table TABLE. Values are expected to be numeric."
  (subseq (sort (hash-table-alist table) #'> :key #'cdr) 0 n))

;; (split-heatmap-y-z (sort-by-price #H(1.92 22 1.93 21)))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun get-tp-sl (rates &optional (lookahead-count 10) (symmetricp nil))
  ;; We need to use `rate-open` because we're starting at that price after
  ;; calculating the inputs before this rate.
  (let* ((init-rate-ask (hsinp.rates:->open-ask (first rates)))
	 (init-rate-bid (hsinp.rates:->open-bid (first rates)))
	 (max-pos-ask 0)
	 (max-pos-bid 0)
	 (max-neg-ask 0)
	 (max-neg-bid 0))
    (loop for rate in (subseq rates 0 lookahead-count)
	  do (let ((delta-high-ask (- (hsinp.rates:->high-ask rate) init-rate-bid)) ;; Started as sell order, then close as ask.
		   (delta-high-bid (- (hsinp.rates:->high-bid rate) init-rate-ask)) ;; Started as buy order, then close as bid.
		   (delta-low-ask (- (hsinp.rates:->low-ask rate) init-rate-bid))
		   (delta-low-bid (- (hsinp.rates:->low-bid rate) init-rate-ask)))
	       ;; Checking for possible price stagnation. If true, ignore.
	       ;; (unless (and (< init-rate delta-high)
	       ;; 		    (> init-rate delta-low))
	       ;; 	 (when (> delta-high max-pos)
	       ;; 	   (setf max-pos delta-high))
	       ;; 	 (when (< delta-low max-neg)
	       ;; 	   (setf max-neg delta-low)))
	       (when (> delta-high-ask max-pos-ask)
		 (setf max-pos-ask delta-high-ask))
	       (when (> delta-high-bid max-pos-bid)
		 (setf max-pos-bid delta-high-bid))
	       (when (< delta-low-ask max-neg-ask)
		 (setf max-neg-ask delta-low-ask))
	       (when (< delta-low-bid max-neg-bid)
		 (setf max-neg-bid delta-low-bid))))
    ;; `((:tp . ,(if (>= snd-max-pos (abs snd-max-neg)) snd-max-pos snd-max-neg))
    ;;   ,(if symmetricp
    ;; 	   `(:sl . ,(if (>= snd-max-pos (abs snd-max-neg)) (* snd-max-pos -1.0) (* snd-max-neg -1.0)))
    ;; 	   `(:sl . ,(if (>= snd-max-pos (abs snd-max-neg)) snd-max-neg snd-max-pos))
    ;; 	   ))

    ;; Can we just use (>= max-pos-ask (abs max-neg-ask)) and totally ignore `-bid` in the condition?
    `((:tp . ,(if (>= max-pos-ask (abs max-neg-ask)) max-pos-bid max-neg-ask))
      ,(if symmetricp
	   `(:sl . ,(if (>= max-pos-ask (abs max-neg-ask)) (* max-pos-bid -1.0) (* max-neg-ask -1.0)))
	   `(:sl . ,(if (>= max-pos-ask (abs max-neg-ask)) max-neg-bid max-pos-ask))
	   ))
    
    ;; `((:tp . ,(if (>= max-pos (abs max-neg)) max-pos max-neg))
    ;;   ,(if symmetricp
    ;; 	   ;; `(:sl . ,(if (>= max-pos (abs max-neg)) (* max-pos -1.0) (* max-neg -1.0)))
    ;; 	   `(:sl . ,(if (>= max-pos (abs max-neg))
    ;; 			(if (> (/ max-pos 2) (abs max-neg))
    ;; 			    (* -1 (/ max-pos 2))
    ;; 			    max-neg)
    ;; 			(if (> (abs (/ max-neg 2)) max-pos)
    ;; 			    (* -1 (/ max-neg 2))
    ;; 			    max-pos)))
    ;; 	   `(:sl . ,(if (>= max-pos (abs max-neg)) max-neg max-pos))
    ;; 	   ;; `(:sl . ,(if (>= max-pos (abs max-neg)) (* max-pos -1.0) (* max-neg -1.0)))
    ;; 	   ))
    ))
;; (time (get-tp-sl (get-input-dataset *rates* 10)))
