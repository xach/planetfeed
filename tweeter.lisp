;;;; tweeter.lisp

(in-package #:planetfeed)

(defparameter *chirp-oath-variables*
  '(chirp:*oauth-api-key*
    chirp:*oauth-api-secret*
    chirp:*oauth-access-token*
    chirp:*oauth-access-secret*))

(defun load-credentials (file)
  (with-open-file (stream file)
    (dolist (sym *chirp-oath-variables*)
      (setf (symbol-value sym) (read-line stream)))))

(defun save-credentials (file)
  (dolist (sym *chirp-oath-variables*)
    (unless (symbol-value sym)
      (error "Credentials not initialized enough to save -- ~S unset" sym)))
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (dolist (sym *chirp-oath-variables*)
      (write-line (symbol-value sym) stream))))

(defun twitter-login (credentials-file)
  (let ((url (chirp:initiate-authentication)))
    (format t "Visit this URL to authenticate:~%  ~A~%~%" url)
    (format t "Enter PIN to continue, or nothing to give up: ")
    (force-output)
    (let ((pin (read-line)))
      (when (every #'digit-char-p pin)
        (chirp:complete-authentication pin)
        (save-credentials credentials-file)
        (format t "Successful login, saved to ~A.~%"
                (probe-file credentials-file))))))

(define-condition credentials-unset-error (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition ))
             (format stream "Chirp credentials not set"))))

(defun check-logged-in ()
  (unless (and chirp:*oauth-api-key* chirp:*oauth-api-secret*
               chirp:*oauth-access-token* chirp:*oauth-access-secret*)
    (error 'credentials-unset-error))
  (chirp:screen-name (chirp:account/verify-credentials)))

(deftype login-needed-error ()
  '(or credentials-unset-error chirp-objects:oauth-request-error))

(defun ensure-logged-in ()
  (handler-case
      (check-logged-in)
    (login-needed-error ()
      (twitter-login))))
