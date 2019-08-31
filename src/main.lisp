;; (ql:quickload :overmind-agents)
;; (ql:quickload :overmind-code)
;; (ql:quickload :overmind-intuition)
;; (ql:quickload :overmind-input)
;; (ql:quickload :overmind-perception)
;; (ql:quickload :mlforecasting)
;; (mlforecasting:start :port 2001)
;; (mlforecasting:stop)
;; (ql:quickload :neuropredictions)
;; (neuropredictions:start :port 2001)
;; (ql:quickload :clmath)
;; (coleslaw:main "/home/amherag/quicklisp/local-projects/mlforecasting/src/blog")
(cl21:in-package :cl21-user)
(defpackage overmind-input
  (:use :cl21
	:lparallel)
  (:export :*bests*
           :get-results
           :get-trades
	   :get-transactions
           :get-rates
           :load-data))
(in-package :overmind-input)

(defparameter *token* "")
(defparameter *account* "")

(setf lparallel:*kernel* (lparallel:make-kernel 4))

;; used for optimization
(defparameter *spread* 4)
(defparameter *markets* (list #H(:pair :AUD_USD :granularity :H1 :jpy? nil)
                              #H(:pair :EUR_GBP :granularity :H1 :jpy? nil)
                              #H(:pair :EUR_JPY :granularity :H1 :jpy? t)
                              #H(:pair :EUR_USD :granularity :H1 :jpy? nil)
                              #H(:pair :GBP_USD :granularity :H1 :jpy? nil)
                              #H(:pair :USD_CAD :granularity :H1 :jpy? nil)
                              #H(:pair :USD_JPY :granularity :H1 :jpy? t)))

(defparameter *bests*
  #H(:AUD_USD #H(:M15 '(9 22 T 3.9726815)
                      :H1 '(50 196 T 309411.2))
              ;; :EUR_CHF #H(:M15 '(3 30 NIL 4.738623)
              ;;                  :H1 '(50 175 T 2046414.9))
              :EUR_GBP #H(:M15 '(2 30 T 4.2747073)
                               :H1 '(50 175 T 2046414.9))
              :EUR_JPY #H(:M15 '(5 21 T 5.025353)
                               :H1 '(50 260 T 2.105043e7))
              :EUR_USD #H(:M15 '(10 18 T 3.0197299)
                               :H1 '(50 354 T 462861.13))
              :GBP_USD #H(:M15 '(5 30 T 4.1615114)
                               :H1 '(50 316 T 96142.695))
              :USD_CAD #H(:M15 '(4 29 T 3.6801808)
                               :H1 '(50 267 T 1018960.3))
              ;; :USD_CHF #H(:M15 '(4 13 T 4.1552176)
              ;;                  :H1 '(7 15 T 3.4722807))
              :USD_JPY #H(:M15 '(8 17 T 3.8354201)
                               :H1 '(50 290 T 1.1956281e8))))

(defun mean (sequence)
  (/ (reduce #'+ sequence) (length sequence)))

(defun stdev (sequence)
  (let ((mean (mean sequence))
        (n (length sequence)))
    (sqrt (/ (reduce #'+ (map (lambda (x) (expt (- mean x) 2))
                              sequence))
             (1- n)))))

(defun mem (value store &optional (size 5))
  (if (>= (length store) size)
      (push value (subseq store 0 (1- size)))
      (push value store)))

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

(defun get-rates (instrument howmany-batches granularity)
  "Gathers prices from Oanda.
A batch = 5,000 rates."
  (labels ((recur (end result counter)
             (let ((candles (ignore-errors
                              (rest (assoc :candles (cl-json:decode-json-from-string
                                                     (dex:get #"https://api-fxtrade.oanda.com/v1/candles?\
instrument=${instrument}&\
granularity=${granularity}&\
count=500&\
end=${end}&\
dailyAlignment=0&\
candleFormat=bidask&\
alignmentTimezone=America%2FNew_York"
                                                              :insecure t
                                                              :headers '(("X-Accept-Datetime-Format" . "UNIX")))))))))
               (sleep 0.5)
               (if (and candles (< counter howmany-batches))
                   (recur (read-from-string
                           (rest (assoc :time (first candles))))
                          (append (map (lm (candle)
                                         (list (assoc :close-bid candle)
                                               (assoc :open-bid candle)
                                               (assoc :high-bid candle)
                                               (assoc :low-bid candle)
                                               (assoc :time candle)))
                                       candles)
                                  result)
                          (incf counter))
                   result))))
  
    (recur (* (local-time:timestamp-to-unix (local-time:now)) 1000000)
           nil
           0)))

;; (local-time:timestamp- (local-time:now) 1 :month)

(defun get-transactions ()
  (let* ((headers `(("Authorization" . ,#"Bearer ${*token*}")
		    ;; ("X-Accept-Datetime-Format" . "UNIX")
		    ("Content-Type" . "application/json")))
	 (from (local-time:parse-timestring "2018-11-20"))
	 (page (cadr (assoc :pages (cl-json:decode-json-from-string
				    (dex:get #"https://api-fxtrade.oanda.com/v3/accounts/${*account*}/transactions?\
from=${from}"
					     :insecure t
					     :headers headers)))))
	 (transactions (cl-json:decode-json-from-string
			(dex:get page
				 :insecure t
				 :headers headers)))
	 )
    (reverse
     (remove nil
	     (map (lm (trans)
    		      (if (string= (cdr (assoc :type trans)) "ORDER_FILL")
			  (let ((instrument (assoc :instrument trans))
				(units (assoc :units trans))
				(price (assoc :price trans))
				(account-balance (assoc :account-balance trans))
				(pl (assoc :realized-+pl+ (cadr (assoc :trades-closed trans))))
				;; (time (assoc :time trans))
				(time `(:time . ,(local-time:format-timestring nil (local-time:parse-timestring (cdr (assoc :time trans)))
								    :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec))))
				
				)
			    ;; `(:instrument ,instrument :units ,units :price ,price
			    ;; 	       :account-balance ,account-balance :units ,units :time ,time)
			    ;; (alexandria:flatten
			    ;;  (list instrument units price account-balance units pl time))
			    (list instrument units price account-balance units pl time)
			    ))
		      )
    		  (cdr (assoc :transactions transactions)))))
    ))

;; (get-transactions)

;; (get-rates :EUR_USD 1 :M5)

(defun rsum (f init xs)
  (loop for x in xs collect (setf init (funcall f init x))))

;; end general purpose functions

;; start predictus algorithm functions

(defun get-subsets (pivot fibos)
  (let ((upper (take-while (lm (elt)
                             (> elt pivot))
                           fibos))
        (lower (drop-while (lm (elt)
                             (>= elt pivot))
                           fibos)))
    ;;upper
    `((:upper ,upper)
      (:lower ,lower))))

;; (defun clml-centroids (k clml-subset)
;;   (map (lm (centroid)
;;          (first (rest centroid)))
;;        (clml.hjs.k-means:get-cluster-centroids
;;         (clml.hjs.k-means:k-means k clml-subset))))

;; (defun clml-subset (subset)
;;   (let ((subset (map (lm (r)
;;                        #"${r}\n")
;;                      subset)))
;;     (clml.hjs.read-data:pick-and-specialize-data
;;      (clml.hjs.read-data:read-data-from-stream
;;       (make-string-input-stream #"@{subset}")
;;       :csv-header-p '("rates")
;;       :csv-type-spec '(double-float))
;;      :range :all
;;      :data-types '(:numeric)
;;      )))


;; end predictus algorithm functions

;; start optimization algorithm functions

(defun mem (value store &optional (size 1))
  (if (>= (length store) size)
      (push value (subseq store 0 (1- size)))
      (push value store)))

;;(update-optimizations)

(defun update-optimizations ()
  (map (lm (market)
         (let* ((data (load-data (getf market :pair)
                                 (getf market :granularity)
                                 79000))
                (jpy? (getf market :jpy?))
                (evals (pmap 'list
                             (lm (genome)
                               (let* ((final-results
                                       (let* ((alpha (pips (first genome) jpy? t))
                                              (beta (pips (second genome) jpy? t)))
                                         (labels ((trade (pdata
                                                          data results tradingp index)
                                                    (if (emptyp data)
                                                        results
                                                        (let* (
                                                               (datum (first data))
                                                               (pdata (mem (first data) pdata))
                                                               (pivot (first datum))
                                                               (time (second datum))
                                                               (lower (mean (map #'third pdata)))
                                                               (upper (mean (map #'fourth pdata)))

                                                               (open-buy-p (and (< (- upper pivot) alpha)
                                                                                    (> (- upper lower) beta)))
                                                               (open-sell-p (and (< (- pivot lower) alpha)
                                                                                 (> (- upper lower) beta)))
                                                               (close-buy-p (and (eq (first (last results)) :buy)
                                                                                 (or (> (abs (- pivot (last (last results))))
                                                                                        (pips 1000 jpy? nil))
                                                                                     (< (- upper lower) (* 2 beta)))))
                                                               (close-sell-p (and (eq (first (last results)) :sell)                                                               
                                                                                  (or (> (abs (- pivot (last (last results))))
                                                                                         (pips 1000 jpy? nil))
                                                                                      (< (- upper lower) (* 2 beta))))))

                                                          (trade pdata
                                                                 (rest data)
                                                                 (if tradingp
                                                                     (if close-buy-p
                                                                         (progn
                                                                           (concatenate 'list results `((:close ,index ,pivot)))
                                                                           )
                                                                         (if close-sell-p
                                                                             (progn
                                                                               (concatenate 'list results `((:close ,index ,pivot))))
                                                                             results
                                                                             ))
                                                                     (if open-buy-p
                                                                         (concatenate 'list results `((:buy ,index ,pivot)))
                                                                         (if open-sell-p
                                                                             (concatenate 'list results `((:sell ,index ,pivot)))
                                                                             results)))

                                                                 (if tradingp
                                                                     (if (and (eq (first (last results)) :buy)
                                                                              (not close-buy-p))
                                                                         t
                                                                         (if (and (eq (first (last results)) :sell)
                                                                                  (not close-sell-p))
                                                                             t
                                                                             nil))
                                                                     (if (or open-buy-p open-sell-p)
                                                                         t))
                                                                 ;; send this pivot to next iteration
                                                                 (1+ index)
                                                                 )
                                                          ))))
                                           (trade nil data '() nil 0)
                                           ))
                                        )
                   
                                      (plot-results (progn
                                                      (map (lm (x y)
                                                             (list x y))
                                                           (iota (length final-results))
                                                           (rsum #'+ 0
                                                                 (map ^(* % 10000)
                                                                      (remove nil
                                                                              (map (lm (open close)
                                                                                     (if (eq (first close) :close)
                                                                                         (if (eq (first open) :buy)
                                                                                             (- (last close)
                                                                                                (last open)
                                                                                                (pips *spread* jpy? t))
                                                                                             (- 
                                                                                              (last open)
                                                                                              (last close)
                                                                                              (pips *spread* jpy? t)
                                                                                              ))))
                                                                                   final-results
                                                                                   (rest final-results)))))))))
                                 ;;(break "~a" final-results)
                                 (concatenate 'list (list (first genome))
                                              (list (second genome))
                                              (list (third genome))
                                              (list (score plot-results))
                                              )
                                 ))
                             (gen-genome))))
           (first (sort evals
                        #'>
                        :key #'last))))
       *markets*
       ))

;;(update-rates-and-data)

;; (defun update-rates-and-data ()
;;   (map (lm (market)
;;          (let* ((partition-size 200)
;;                 (rates (get-rates (getf market :pair)
;;                                   4
;;                                   (getf market :granularity)))
;;                 (sample-size (- (length rates)
;;                                 partition-size))
;;                 (data (pmapcar (lm (close time bounds)
;;                                  (list close
;;                                        (read-from-string time)
;;                                        (first (first bounds))
;;                                        (first (second bounds))))
;;                                (subseq (pmapcar ^(rest (assoc :close-bid %)) rates)
;;                                        (1- partition-size)
;;                                        (+ sample-size (1- partition-size)))
;;                                (subseq (pmapcar ^(rest (assoc :time %)) rates)
;;                                        (1- partition-size)
;;                                        (+ sample-size (1- partition-size)))
;;                                (pmapcar (lm (subsets)
;;                                           (let ((upper (first (rest (first subsets))))
;;                                                 (lower (first (rest (second subsets)))))
;;                                             (if (and (or (< (length upper) 2)
;;                                                          (< (length lower) 2)
;;                                                          (not upper)
;;                                                          (not lower)))
;;                                                 '("")
;;                                                 (list (clml-centroids 1 (clml-subset lower))
;;                                                       (clml-centroids 1 (clml-subset upper)))
;;                                                 )))
          
;;                                         (pmapcar (lm (pivot diffs)
;;                                                    (get-subsets pivot (fibos diffs)))
;;                                                  (subseq (pmapcar ^(rest (assoc :close-bid %)) rates)
;;                                                          (1- partition-size)
;;                                                          (+ sample-size (1- partition-size)))
;;                                                  (subseq (pmapcar #'diffs (ts-partition partition-size rates))
;;                                                          0 sample-size))))))
;;            ;; saving rates
;;            (with-open-file (str #"${*data-directory*}${(getf market :pair)}_${(getf market :granularity)}-rates.lisp"
;;                                 :direction :output
;;                                 :if-exists :supersede
;;                                 :if-does-not-exist :create)
;;              (map (lm (sexpr)
;;                     (format str "~a~%" sexpr))
;;                   rates))
;;            ;;saving data
;;            (with-open-file (str #"${*data-directory*}${(getf market :pair)}_${(getf market :granularity)}-data.lisp"
;;                                 :direction :output
;;                                 :if-exists :supersede
;;                                 :if-does-not-exist :create)
;;              (map (lm (sexpr)
;;                     (format str "~a~%" sexpr))
;;                   data))
;;            ))
;;        *markets*))

(defun max-short (plot-results)
  (let* ((profits (map #'second plot-results))
         (highest (first profits))
         (max-short 0))
    (map (lm (profit)
           (if (> profit highest)
               (setq highest profit)
               (if (> (- highest profit) max-short)
                   (setq max-short (- highest profit)))))
         profits)
    max-short))

(defun score (points)
  (let* ((ys (map #'second points))
         (xs (map #'first points))
         (howmany-positive (length (remove nil (map
                                                ^(if (> %1 %2)
                                                     %1)
                                                (rest ys)
                                                ys
                                                ))))
         (time-stdev (ignore-errors (stdev xs))))
    (if (ignore-errors (/ howmany-positive
                          time-stdev))
        (/ (* (- (last ys) (first ys))
              howmany-positive
              (length points))
           time-stdev)
        0)
    ))

(defun gen-genome ()
  (remove-if ^(>= (first %) (second %))
             (apply #'concatenate 'list
                    (map (lm (inverse?)
                           (apply #'concatenate 'list
                                  (map (lm (n1)
                                         (remove nil
                                                 (map (lm (n2)
                                                        (list n1 n2 inverse?)
                                                        )
                                                      ;;omega
                                                      (iota 300 :start 100 :step 1))))
                                       ;;alpha
                                       (iota 1 :start 50 :step 1))))
                         '(t)
                         ))))

;; end optimization algorithm functions

(defun load-data (pair granularity)
  (let* ((in (open #"${*data-directory*}${pair}_${granularity}-data.lisp" :if-does-not-exist nil))
         (result (list (read in))))
    (when in
      (loop for line = (read in nil)
         while line do (nconc result (list line)))
      (close in))
    result))

(defun count-lines-in-file (pathname)
  (let* ((in (open pathname :if-does-not-exist nil))
         (counter 0))
    (when in
      (loop for line = (read in nil)
         while line do (incf counter))
      (close in))
    counter))

(defun load-data (pair granularity &optional (howmany 300))
  (let* ((in (open #"${*data-directory*}${pair}_${granularity}-data.lisp" :if-does-not-exist nil))
         ;;(result (list (read in)))
         (result nil)
         (start (- (count-lines-in-file #"${*data-directory*}${pair}_${granularity}-data.lisp") howmany))
         (aux-line-number 0))
    (when in
      (loop for line = (read in nil)
         while line
         do (progn
              (incf aux-line-number)
              (when (> aux-line-number start)
                (if result
                    (nconc result (list line))
                    (setf result (list line)))))
           )
      (close in))
    result))
;; (fare-memoization:memoize 'load-data)

;; (defparameter *data* (load-data :AUD_USD :H1))

;; (defun get-data (rates)
;;   (let* ((partition-size 200)
;;          (sample-size (- (length rates)
;;                          partition-size)))
;;     (pmapcar (lm (close time bounds)
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
;;              (pmapcar (lm (subsets)
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
          
;;                       (pmapcar (lm (pivot diffs)
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

;; (map (lm (diffs)
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
;;     (map (lm (price)
;;            (setf (gethash ht (/ price 1000)) 0)
;;            )
;;          (iota n :start min-area :step 1))
;;     (map (lm (fib)
;;            (if (gethash ht (/ (floor (ceiling fib 1/10000) area-size) 1000))
;;                (incf (gethash ht (/ (floor (ceiling fib 1/10000) area-size) 1000))))
;;            )
;;          fibos)
;;     ht))

;; (defun correct-heats (ht-data area-size)
;;   (let (max-rate min-rate)
;;     (map (lm (data)
;;            (let ((keys (hash-keys (gethash data :heat)))
;;                  mx mn)
;;              (setf mx (apply #'max keys))
;;              (setf mn (apply #'min keys))
;;              (if (or (not max-rate) (> mx max-rate))
;;                  (setf max-rate mx))
;;              (if (or (not min-rate) (< mn min-rate))
;;                  (setf min-rate mn))))
;;          ht-data)
;;     (map (lm (rate)
;;            (map (lm (ht)
;;                   ;; (print (gethash (gethash ht :heat) 1.145))
;;                   ;; (print (hash-keys (gethash ht :heat)))
;;                   (if (not (gethash (gethash ht :heat) rate nil))
;;                       (setf (gethash (gethash ht :heat) rate) 0))
;;                   )
;;                 ht-data))
;;          (append (map (lm (rate)
;;                   (/ rate 1000))
;;                 (iota (- (* max-rate 1000)
;;                     (* min-rate 1000))
;;                       :start (* min-rate 1000)))
;;                  `(,max-rate)))

;;     ;; transforming the heat hash-tables to alists and ordering them
;;     (map (lm (ht)
;;            (setf (gethash ht :heat) (sort-by-price (gethash ht :heat))))
;;          ht-data)
;;     ht-data))

(defun hash-table-top-n-values (table n)
  "Returns the top N entries from hash table TABLE. Values are expected to be numeric."
  (subseq (sort (hash-table-alist table) #'> :key #'cdr) 0 n))

;; (split-heatmap-y-z (sort-by-price #H(1.92 22 1.93 21)))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun get-results (trades &optional (spread 4) (jpy? nil) (decimal? nil))
  (let ((profits (remove nil
                         (map (lm (open close)
                                (if (eq (first close) :close)
                                    (list (second close)
                                          (* (if jpy?
                                                 100
                                                 10000)
                                             (if (eq (first open) :buy)
                                                 (- (last close)
                                                    (last open)
                                                    (pips spread jpy? decimal?))
                                                 (- 
                                                  (last open)
                                                  (last close)
                                                  (pips spread jpy? decimal?)))))))
                              trades
                              (rest trades)))))
    (map (lm (time rp)
           (list time rp))
         (map #'first profits)
         (rsum (lm (elt1 elt2)
                 (+ elt1
                    (second elt2)))
               0 profits))))

(defun get-trades (data best &optional (jpy? nil) (decimal? nil))
  (let* ((alpha (pips (first best) jpy? decimal?))
         (omega (pips (second best) jpy? decimal?))
         (inverse? (third best)))
    (labels ((trade (pdata
                     data results tradingp index)
               (if (emptyp data)
                   results
                   (let* (
                          
                          (datum (first data))
                          (pdata (mem (first data) pdata))
                          (ppivot (mean (map #'first pdata)))
                          (pivot (first datum))
                          (time (second datum))
                          (lower (mean (map #'third pdata)))
                          (upper (mean (map #'fourth pdata)))
                          
                          ;; variables pasadas
                          ;; (datum (first data))
                          ;; (pivot (first datum))
                          ;; (time (second datum))
                          ;; (lower (third datum))
                          ;; (upper (fourth datum))

                          (open-buy-p (if inverse?
                                          (and
                                           (< (- upper ppivot) alpha))
                                          (and
                                           (< (- ppivot lower) alpha))
                                          ))
                          (open-sell-p (if inverse?
                                           (and
                                            (< (- ppivot lower) alpha))
                                           (and
                                            (< (- upper ppivot) alpha))
                                           ))
                          (close-buy-p (and (eq (first (last results)) :buy)
                                            (if inverse?
                                                (> (- upper ppivot) omega)
                                                (> (- ppivot lower) omega))
                                            ))
                          (close-sell-p (and (eq (first (last results)) :sell)
                                             (if inverse?
                                                 (> (- ppivot lower) omega)
                                                 (> (- upper ppivot) omega))
                                             ))
                          
                          ;; (open-buy-p (if inverse?
                          ;;                 (< (abs (- pivot upper)) alpha)
                          ;;                 (< (abs (- pivot lower)) alpha)))
                          ;; (open-sell-p (if inverse?
                          ;;                  (< (abs (- pivot lower)) alpha)
                          ;;                  (< (abs (- pivot upper)) alpha)))
                          ;; (close-buy-p (and (eq (first (last results)) :buy)
                          ;;                   (if inverse?
                          ;;                       (< (abs (- lower pivot)) omega)
                          ;;                       (< (abs (- upper pivot)) omega))))
                          ;; (close-sell-p (and (eq (first (last results)) :sell)
                          ;;                    (if inverse?
                          ;;                        (< (abs (- upper pivot)) omega)
                          ;;                        (< (abs (- lower pivot)) omega))))
                          )
                     (trade pdata
                            (rest data)
                            (if tradingp
                                (if close-buy-p
                                    (progn
                                      (concatenate 'list results `((:close ,time ,pivot)))
                                      )
                                    (if close-sell-p
                                        (progn
                                          (concatenate 'list results `((:close ,time ,pivot))))
                                        results
                                        ))
                                (if open-buy-p
                                    (concatenate 'list results `((:buy ,time ,pivot)))
                                    (if open-sell-p
                                        (concatenate 'list results `((:sell ,time ,pivot)))
                                        results)))

                            (if tradingp
                                (if (and (eq (first (last results)) :buy)
                                         (not close-buy-p))
                                    t
                                    (if (and (eq (first (last results)) :sell)
                                             (not close-sell-p))
                                        t
                                        nil))
                                (if (or open-buy-p open-sell-p)
                                    t))
                            ;; send this pivot to next iteration
                            (1+ index)
                            )
                     ))))
      (trade nil data '() nil 0))))
