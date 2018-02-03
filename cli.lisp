;;;; cli.lisp

(in-package #:planetfeed)

(defun command-line-update (credentials-file)
  (load-credentials credentials-file)
  (check-logged-in)
  (update-twitter))

(defun cli (argv)
  (sb-ext:disable-debugger)
  (let ((options (rest argv))
        (credentials-file nil ))
    (loop
      (let ((option (pop options)))
        (cond ((null option)
               (unless credentials-file
                 (error "--credentials-file is required"))
               (return (command-line-update credentials-file)))
              ((equal option "--login")
               (unless credentials-file
                 (error "--credentials-file is required"))
               (return (twitter-login credentials-file)))
              ((equal option "--credentials-file")
               (setf credentials-file (pop options)))
              (t
               (error "Unknown option ~S" option)))))))
