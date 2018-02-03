;;;; utils.lisp

(in-package #:planetfeed)

(defun prefixp (prefix string)
  (alexandria:starts-with-subseq prefix string))
