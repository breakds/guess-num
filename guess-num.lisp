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
    `(:obj ,(cons "records" (reverse (hunchentoot:session-value 'records)))
           ,(cons "guessed" (if (= a 4) t :f)))))

(def-widget game-box ()
    ((state (records (array))
            (game-over nil))
     (handle-submit (data)
                    (with-rpc (calculate-hint (@ data text))
		      (chain this (set-state (create records 
						     (@ rpc-result records)
                                                     game-over (@ rpc-result
                                                                  guessed))))
                      (chain console (log rpc-result)))
		    false))
  #jsx(:div ()
            (:h1 ((class-name "title")) "Guess Number")
	    (:div ((class-name "topcoat-list__container"))
		  (:ul ((class-name "topcoat-list"))
		       (:h3 ((class-name "topcoat-list__header"))
			    "Game Records")
		       (chain (local-state records) 
			      (map (lambda (record)
				     (:game-record ((number (@ record number))
						    (hint (@ record hint)))))))))
            (if (local-state game-over)
                (:reset-panel)
                (:interaction-area ((process-guess (@ this handle-submit)))))))

(def-widget interaction-area (process-guess)
    ((handle-submit ()
                    (process-guess (create text 
					   (chain (local-node text-num) 
						  value (trim))))
                    (setf (@ (local-node text-num) value) "")
                    false))
  #jsx(:form ((on-submit (@ this handle-submit)))
             (:input ((ref "text-num")
                      (type "search")
                      (placeholder "  e.g. 1234")
                      (class-name "topcoat-search-input")))
             (:button ((class-name "topcoat-button--cta")
                       (type "submit")) "Guess")))

(def-widget reset-panel ()
    ((handle-submit () 
                    (chain window location (reload t))))
  #jsx(:form ((on-submit (@ this handle-submit)))
             (:button ((class-name "topcoat-button--cta")
                       (style (create "background-color" "lightgreen")))
                      "You win. Restart.")))

(def-widget game-record (number hint)
    ()
  #jsx(:li ((class-name "topcoat-list__item"))
	   (:span ((style (create "font-size" "20px")))
		  (+ number "  "))
	   (:span ((class-name "topcoat-notification"))
		  hint)))

(def-realispic-app (guess-num-app :title "Guess Number"
				  :libs ("http://fb.me/react-0.10.0.js"
				  	 "http://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js")
                                  :css ("http://cdnjs.cloudflare.com/ajax/libs/topcoat/0.8.0/css/topcoat-mobile-light.min.css")
				  :document-base (asdf:system-source-directory 'guess-num)
                                  :port 14386)
  (with-rpc (initialize)
    (chain console (log rpc-result)))
  #jsx(:game-box))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (disable-jsx-reader))

                  
