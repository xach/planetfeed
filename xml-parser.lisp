;;;; xml-parser.lisp
;;;;
;;;; Cribbed from zs3. Overkill...

(in-package #:planetfeed)


;;; utility

(defun skip-document-start (source)
  (let ((type (klacks:peek source)))
    (when (eql :start-document type)
      (klacks:consume source))
    (values)))

(defun skip-characters (source)
  (loop 
   (if (member (klacks:peek source) '(:characters :comment))
       (klacks:consume source)
       (return))))

(defun collect-characters (source)
  (with-output-to-string (stream)
    (loop
     (multiple-value-bind (type data)
         (klacks:peek source)
       (cond ((eql type :characters)
              (write-string data stream)
              (klacks:consume source))
             (t
              (return)))))))

(defun collect-rest-alist (source)
  "Collect the rest of SOURCE, up to an un-nested closing tag, as an
alist of element names and their character contents."
  (let ((result '()))
    (loop
     (multiple-value-bind (type uri lname)
         (klacks:peek source)
       (declare (ignore uri))
       (ecase type
         (:characters (klacks:consume source))
         (:end-element
          (return (nreverse result)))
         (:start-element
          (klacks:consume source)
          (push (cons lname (collect-characters source)) result)
          (klacks:find-event source :end-element)
          (klacks:consume source)))))))

;;; Match failure conditions

(define-condition xml-binding-error (error)
  ((expected
    :initarg :expected
    :accessor expected)
   (actual
    :initarg :actual
    :accessor actual))
  (:report
   (lambda (condition stream)
     (format stream "Unexpected XML structure: expected ~S, got ~S instead"
             (expected condition)
             (actual condition)))))


;;; API

(defvar *binder-definitions*
  (make-hash-table))

(defclass binder ()
  ((source
    :initarg :source
    :accessor source)
   (closure
    :initarg :closure
    :accessor closure)))

(defmacro defbinder (name &body source)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *binder-definitions*)
           (make-instance 'binder
                          :closure (make-binder ',@source)
                          :source ',@source))))

(defun find-binder (name &optional (errorp t))
  (let ((binder (gethash name *binder-definitions*)))
    (or binder
        (and errorp
             (error "No binder named ~S" name)))))

(defun xml-bind (binder-name source)
  (funcall (closure (find-binder binder-name)) source))

(defun try-to-xml-bind (binder-name source)
  "Like XML-BIND, but catches any XML-BINDING-ERRORs; if any errors
  are caught, NIL is the primary value and the error object is the
  secondary value."
  (handler-case
      (xml-bind binder-name source)
    (xml-binding-error (c)
      (values nil c))))

;;; Creating the matchers/binders

(defvar *current-element-name*)

(defun create-element-start-matcher (element-name kk)
  "Return a function that expects to see the start of ELEMENT-NAME
next in SOURCE."
  (lambda (source bindings k)
    (skip-characters source)
    (multiple-value-bind (type uri lname qname)
        (klacks:peek source)
      (declare (ignore uri qname))
      (when (not (eql type :start-element))
        (error 'xml-binding-error
               :expected (list :start-element element-name)
               :actual (list :event type)))
      (when (string/= element-name lname)
        (error 'xml-binding-error
               :expected (list :start-element element-name)
               :actual (list type lname)))
      (klacks:consume source)
      (funcall kk source bindings k))))

(defun create-element-end-matcher (element-name kk)
  "Return a function that expects to see the end of ELEMENT-NAME next in
SOURCE."
  (lambda (source bindings k)
    (skip-characters source)
    (multiple-value-bind (type uri lname qname)
        (klacks:peek source)
      (declare (ignore uri qname))
      (when (not (eql type :end-element))
        (error 'xml-binding-error
               :expected (list :end-element element-name)
               :actual (list :event type lname)))
      (when (string/= element-name lname)
        (error 'xml-binding-error
               :expected (list :end-element element-name)
               :actual (list type lname)))
      (klacks:consume source)
      (funcall kk source bindings k))))

(defun create-bindings-extender (key kk)
  "Return a function that extends BINDINGS with KEY and a value of
whatever character data is pending in SOURCE."
  (lambda (source bindings k)
    (funcall kk source
             (acons key (collect-characters source) bindings)
             k)))

(defun create-skipper (element-name kk)
  "Return a function that skips input in SOURCE until it sees a
closing tag for ELEMENT-NAME. Nested occurrences of elements with the
same ELEMENT-NAME are also skipped."
  (let ((depth 0))
    (lambda (source bindings k)
      (loop
       (multiple-value-bind (type uri lname)
           (klacks:consume source)
         (declare (ignore uri))
         (cond ((and (eql type :end-element)
                     (string= lname element-name))
                (if (zerop depth)
                    (return (funcall kk source bindings k))
                    (decf depth)))
               ((and (eql type :start-element)
                     (string= lname element-name))
                (incf depth))))))))

(defun create-bindings-returner ()
  "Return a function that does nothing but return its BINDINGS,
effectively ending matching."
  (lambda (source bindings k)
    (declare (ignore source k))
    (nreverse bindings)))

(defmacro catching-xml-errors (&body body)
  `(handler-case
       (progn ,@body)
     (xml-binding-error (c)
       (values nil c))))

(defun create-sequence-binder (key forms kk)
  "Return a function that creates a list of sub-bindings based on a
sub-matcher, with KEY as the key."
  (let ((binder (create-binder forms (create-bindings-returner))))
    (lambda (source bindings k)
      (let ((sub-bindings '()))
        (loop
          (skip-characters source)
          (multiple-value-bind (sub-binding failure)
              (catching-xml-errors
                (funcall binder source nil k))
            (if failure
                (return (funcall kk
                                 source
                                 (acons key
                                        (nreverse sub-bindings)
                                        bindings)
                                 k))
                (push sub-binding sub-bindings))))))))

(defun create-alist-binder (key kk)
  "Return a function that returns the rest of SOURCE as an alist of
element-name/element-content data."
  (lambda (source bindings k)
    (funcall kk source
             (acons key (collect-rest-alist source) bindings)
             k)))

(defun create-attributes-binder (attributes kk)
  (let ((attributes
          (loop for (name keyword) on attributes by #'cddr
                collect (cons name keyword))))
    (lambda (source bindings k)
      (klacks:map-attributes
       (lambda (namespace local-name qname value explicitp)
         (declare (ignore namespace qname explicitp))
         (let ((entry (assoc local-name attributes :test 'string=)))
           (unless entry (error "Missing attribute ~S" local-name))
           (let ((keyword (cdr entry)))
             (push (cons keyword value) bindings))))
       source)
      (funcall kk source bindings k))))

(defun create-optional-binder (subforms kk)
  (let ((binder (create-binder subforms kk)))
    (lambda (source bindings k)
      (skip-characters source)
      (multiple-value-bind (optional-bindings failure)
          (catching-xml-errors (funcall binder source bindings k))
        (if failure
            (funcall kk source bindings k)
            optional-bindings)))))

(defun create-alternate-binder (subforms kk)
  (let ((binders (mapcar (lambda (form) (create-binder form kk)) subforms)))
    (lambda (source bindings k)
      ;; FIXME: This xml-binding-error needs :expected and :action
      ;; ooptions. Can get actual with peeking and expected by getting
      ;; the cl:cars of subforms...maybe.
      (dolist (binder binders (error 'xml-binding-error))
        (multiple-value-bind (alt-bindings failure)
            (catching-xml-errors (funcall binder source bindings k))
          (unless failure
            (return alt-bindings)))))))

(defun create-sub-binder-binder (binder-name kk)
  (lambda (source bindings k)
    (let ((binder (find-binder binder-name)))
      (let ((sub-bindings (funcall (closure binder) source)))
        (funcall k source (append sub-bindings bindings) kk)))))

(defun create-special-processor (operator form k)
  "Handle special pattern processing forms like BIND, SKIP-REST, SEQUENCE,
etc."
  (ecase operator
    (include (create-sub-binder-binder (second form) k))
    (alternate (create-alternate-binder (rest form) k))
    (bind (create-bindings-extender (second form) k))
    (optional (create-optional-binder (second form) k))
    (skip-rest (create-skipper *current-element-name* k))
    (sequence
     (destructuring-bind (key subforms)
         (rest form)
       (create-sequence-binder key subforms k)))
    (attributes
     (create-attributes-binder (rest form) k))
    (elements-alist
     (create-alist-binder (second form) k))))

(defun create-binder (form &optional (k (create-bindings-returner)))
  "Process FORM as an XML binder pattern and return a closure to
process an XML source."
  (let ((operator (first form)))
    (etypecase operator
      (string
       (let ((*current-element-name* operator))
         (create-element-start-matcher *current-element-name*
                                       (create-binder (rest form) k))))
      (null
       (create-element-end-matcher *current-element-name*
                                   k))
      (cons
       (create-binder operator (create-binder (rest form) k)))
      (symbol
       (create-special-processor operator form k)))))

(defun xml-source (source)
  (typecase source
    (cxml::cxml-source source)
    (t (cxml:make-source source))))

(defun make-binder (form)
  (let ((binder (create-binder form (create-bindings-returner))))
    (lambda (source)
      (let ((source (xml-source source)))
        (skip-document-start source)
        (funcall binder
                 source
                 nil
                 (create-bindings-returner))))))



(defun xml-document-element (source)
  (nth-value 2 (klacks:find-event (xml-source source) :start-element)))

(defun bvalue (key bindings)
  (cdr (assoc key bindings)))

(defun bfun (key)
  (lambda (binding)
    (bvalue key binding)))

(defmacro alist-bind (bindings alist &body body)
  (let ((binds (gensym)))
    (flet ((one-binding (var)
             (let ((keyword (intern (symbol-name var) :keyword)))
               `(when (eql (caar ,binds) ,keyword)
                  (setf ,var (cdr (pop ,binds)))))))
    `(let ,bindings
       (let ((,binds ,alist))
         ,@(mapcar #'one-binding bindings)
         ,@body)))))


;;; Protocol

(defgeneric merge-bindings (object bindings)
  (:documentation "Update OBJECT with the data from BINDINGS."))

