;;;; guess-num.asd

(asdf:defsystem #:guess-num
    :serial t
    :depends-on (#:basicl
                 #:parenscript
                 #:realispic)
    :components ((:file "guess-num")))
