;;;; guess-num.lisp

(defpackage #:breakds.guess-num
  (:nicknames #:guess-num)
  (:use #:cl
        #:parenscript
        #:realispic))

(in-package #:breakds.guess-num)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-jsx-reader))

(def-rpc calculate-hint ()
  (+ 1 1))


(def-widget game-box ((records (array)))
    ((handle-submit (data)
                    (with-rpc (result (calculate-hint))
                      (chain console (log "RPC success")))
                    (chain console (log (@ data text)))
                    ;; (chain this (set-state (create records (array (create number 3456 hint "3A0B")))))
                    false))
  #jsx(:div ()
            (:h1 ((class-name "title")) "Guess Number")
            (:ul ()
                 ((@ this state records map)
                  (lambda (record)
                    (:game-record ((number (@ record number))
                                   (hint (@ record hint)))))))
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
  #jsx(:li ()
           (:div ((class-name "topcoat-button-bar"))
                 (:div ((class-name "topcoat-button-bar__item"))
                       (:button ((class-name "topcoat-button-bar__button--large"))
                                (@ this props number)))
                 (:div ((class-name "topcoat-button-bar__item"))
                       (:button ((class-name "topcoat-button-bar__button--large"))
                                (@ this props hint))))))

(def-realispic-app (guess-num-app :title "Guess Number"
                                  :libs ("http://fb.me/react-0.10.0.min.js")
                                  :css ("http://cdnjs.cloudflare.com/ajax/libs/topcoat/0.8.0/css/topcoat-desktop-light.min.css")
                                  :port 14386)
  (let ((input-data (array (create number "1234" hint "1A1B")
                           (create number "2345" hint "3A0B"))))
    #jsx(:game-box ((data input-data)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (disable-jsx-reader))

                  
