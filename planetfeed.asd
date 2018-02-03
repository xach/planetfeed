;;;; planetfeed.asd

(asdf:defsystem #:planetfeed
  :serial t
  :description "Parse Planet Lisp's feed into a westbrook feed."
  :license "MIT"
  :author "Zach Beane <xach@xach.com>"
  :depends-on (#:cxml #:westbrook #:drakma #:chirp #:alexandria)
  :components ((:file "package")
               (:file "tweeter")
               (:file "date-parser")
               (:file "xml-parser")
               (:file "utils")
               (:file "planetfeed")
               (:file "cli")))
