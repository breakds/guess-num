;;;; guess-num.lisp

(defpackage #:breakds.guess-num
  (:nicknames #:guess-num)
  (:use #:cl
        #:parenscript
        #:realispic))

(in-package #:breakds.guess-num)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-jsx-reader))

(defun random-4-digit ()
  (let ((sorted (sort (loop for i below 10
			 collect (list (random 1.0) i))
		      #'<
		      :key #'car)))
    (loop 
       for x in sorted
       for i below 4
       collect (cadr x))))

(def-rpc initialize ()
  (setf (hunchentoot:session-value 'key) (random-4-digit))
  (setf (hunchentoot:session-value 'records) nil)
  (format nil "~{~a~}" (hunchentoot:session-value 'key)))

(def-rpc calculate-hint (num)
  (let* ((attempt (loop for i below 4
		    collect (parse-integer num :start i :end (1+ i))))
	 (a (loop for i below 4
	       sum (if (= (elt attempt i)
			  (elt (hunchentoot:session-value 'key) i))
		       1 0)))
	 (b (- (loop for x in attempt
		  sum (if (member x (hunchentoot:session-value 'key))
			  1 0))
	       a)))
    (push (list :obj 
		(cons "number" num)
		(cons "hint" (format nil "~aA~aB" a b)))
	  (hunchentoot:session-value 'records))
    (reverse (hunchentoot:session-value 'records))))

(def-widget game-box ((records (array)))
    ((handle-submit (data)
                    (with-rpc (calculate-hint (@ data text))
		      (chain this (set-state (create records 
						     rpc-result)))
                      (chain console (log rpc-result)))
		    false))
  #jsx(:div ()
            (:h1 ((class-name "title")) "Guess Number")
	    (:div ((class-name "topcoat-list__container"))
		  (:ul ((class-name "topcoat-list"))
		       (:h3 ((class-name "topcoat-list__header"))
			    "Result:")
		       ((@ this state records map)
			(lambda (record)
			  (:game-record ((number (@ record number))
					 (hint (@ record hint))))))))
            (:interaction-area ((on-submit (@ this handle-submit))))))

(def-widget interaction-area ()
    ((handle-submit ()
                    (chain this props 
                           (on-submit (create text 
                                              (chain (local-node text-num) value (trim)))))
                    (setf (@ (local-node text-num) value) "")
                    false))
  #jsx(:form ((on-submit (@ this handle-submit)))
             (:input ((ref "text-num")
                      (type "search")
                      (placeholder "  e.g. 1234")
                      (class-name "topcoat-search-input")))
             (:button ((class-name "topcoat-button")
                       (type "submit")) "Submit")))

(def-widget game-record ()
    ()
  #jsx(:li ((class-name "topcoat-list__item"))
	   (+ (@ this props number) " " (@ this props hint))))


(def-realispic-app (guess-num-app :title "Guess Number"
				  :libs ("http://fb.me/react-0.10.0.min.js"
					 "http://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js")
                                  :css ("http://cdnjs.cloudflare.com/ajax/libs/topcoat/0.8.0/css/topcoat-desktop-light.min.css")
				  :document-base (asdf:system-source-directory 'guess-num)
                                  :port 14386)
  (let ((input-data (array (create number "1234" hint "1A1B")
                           (create number "2345" hint "3A0B"))))
    (with-rpc (initialize)
      (chain console (log rpc-result)))
    #jsx(:game-box ((data input-data)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (disable-jsx-reader))

                  
