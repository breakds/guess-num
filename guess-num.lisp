;;;; guess-num.lisp

(defpackage #:breakds.guess-num
  (:nicknames #:guess-num)
  (:use #:cl
        #:parenscript
        #:realispic))

(in-package #:breakds.guess-num)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-jsx-reader))


(def-widget game-box5 ()
    ()
  #jsx(:div ()
            (:game-record ())))
  
(def-widget game-record ()
    ()
  #jsx(:ul ()
           (:li () "Row 1")
           (:li () "Row 2")))



(def-realispic-app (guess-num-app :title "Guess Number"
                                  :libs ("http://fb.me/react-0.10.0.min.js")
                                  :port 14386)
  #jsx(:game-box5 ()))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (disable-jsx-reader))

