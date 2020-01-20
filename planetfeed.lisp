;;;; planetfeed.lisp

;;; Update the @planet_lisp feed on twitter from the Planet Lisp RSS
;;; feed. Works by posting all RSS items not seen in recent tweets.

(in-package #:planetfeed)

(defvar *feed-url* "http://planet.lisp.org/rss20.xml")
(defvar *github-feed-url* "http://planet.lisp.org/github.atom")
(defvar *max-tweet-length* 240)

(defbinder planet-feed
  ("rss"
   ("channel"
    ("title" (bind :title))
    ("link" (bind :link))
    ("description" (bind :description))
    ("language")
    (sequence
     :items
     ("item"
      ("title" (bind :title))
      ("guid" (bind :guid))
      ("link" (bind :link))
      ("description" (bind :description))
      ("pubDate" (bind :pub-date)))))))

(defbinder github-atom-binder
  ("feed"
   ("id")
   ("updated")
   ("title" (bind :title))
   (sequence
    :items
    ("entry"
     ("title" (bind :title))
     ("author"
      ("name"))
     ("link" (attributes "href" :link))
     ("id" (bind :guid))
     ("published" (bind :pub-date))
     ("updated")
     ("content" (bind :description))))))

(defun latest-planet-feed ()
  (let* ((data (drakma:http-request *feed-url*))
         (bindings (xml-bind 'planet-feed data))
         (feed (make-instance 'westbrook:feed
                              :title (bvalue :title bindings)
                              :link (bvalue :link bindings))))
    (let* ((raw-items (bvalue :items bindings))
           (items
            (mapcar
             (lambda (item-alist)
               (let ((date-entry (assoc :pub-date item-alist)))
                 (setf (cdr date-entry)
                       (parse-date (cdr date-entry)))
                 (apply #'make-instance
                        'westbrook:item
                        (alexandria:alist-plist item-alist))))
             raw-items)))
      (setf (westbrook:items feed) items)
      feed)))

(defun latest-github-feed ()
  (let* ((data (drakma:http-request *github-feed-url*))
         (bindings (xml-bind 'github-atom-binder data))
         (feed (make-instance 'westbrook:feed
                              :title (bvalue :title bindings)
                              :link "none")))
    (let* ((raw-items (bvalue :items bindings))
           (items
             (mapcar
              (lambda (item-alist)
                (let ((description (assoc :description item-alist)))
                  (setf (cdr description)
                        (string-trim '(#\Space #\Newline)
                                     (cdr description))))
                (let ((date-entry (assoc :pub-date item-alist)))
                  (setf (cdr date-entry)
                        (parse-date (cdr date-entry)))
                  (apply #'make-instance
                         'westbrook:item
                         (alexandria:alist-plist item-alist))))
              raw-items)))
      (setf (westbrook:items feed) items)
      feed)))

(defun latest-tweets (&key (count 10) (skip 0))
  (let ((statuses (chirp:statuses/user-timeline :count (+ count skip)
                                                :screen-name "planet_lisp")))
    (unless statuses
      (error "No statuses retrieved for @planet_lisp - WTF?"))
    (when (< (length statuses) skip)
      (error "Not enough statuses to skip ~D of them" skip))
    (subseq statuses skip)))

(defvar *data-directory* nil)

(defun item-identifier (item)
  "Return a text string usable to uniquely identify ITEM."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence 'ironclad:sha1
                             (babel:string-to-octets (item-tweet-text item)))))

(defun item-storage-pathname (item)
  (unless *data-directory*
    (error "~S unset" '*data-directory*))
  (let* ((id (item-identifier item))
         (a (subseq id 0 1))
         (b (subseq id 1 2))
         (directory (list :relative a b)))
    (merge-pathnames
     (make-pathname :directory directory
                    :name id
                    :type "dat")
     *data-directory*)))

(defun nonempty-text-p (text)
  (and (plusp (length text))
       text))

(defun clamp-text-to (text length)
  (if (<= (length text) length)
      text
      (subseq text 0 length)))

(defun item-tweet-text (item)
  (let* ((max-length (- *max-tweet-length*
                        (length (westbrook::title item))
                        (length (westbrook::link item))))
         (text
           (format nil "~A~@[ — ~A~] ~A"
                  (westbrook::title item)
                  (clamp-text-to (nonempty-text-p (westbrook::description item))
                                 max-length)
                  (westbrook::link item))))
    text))

(defun tweet (thing)
  (let ((text
         (etypecase thing
           (westbrook:item
            (item-tweet-text thing))
           (string
            thing))))
    (chirp:statuses/update text)))

(defun already-posted-error-p (error)
  (and (typep error 'chirp-objects:oauth-request-error)
       ;; Wotta nitemare!
       (eql 187
            (cdr
             (assoc :code
                    (first
                     (cdr (assoc :errors (chirp:http-body error)))))))))

(deftype already-posted-error ()
  `(satisfies already-posted-error-p))

(defun ensure-tweeted (item)
  (let ((file (item-storage-pathname item)))
    (ensure-directories-exist file)
    (with-open-file (stream file :direction :output :if-exists nil)
      (cond (stream
             (handler-case
                 (let* ((status (tweet item))
                        (id (chirp:id status)))
                   (prin1 id stream)
                   status)
               (already-posted-error ()
                 (prin1 -1 stream)
                 -1)))
            (t
             nil)))))

;;; Now do the thing

(defun update-twitter ()
  (let* ((feed (latest-feed)))
    (dolist (item (westbrook:items feed))
      (let ((status (ensure-tweeted item)))
        (when status
          (format t "Posted ~A...~%" item))))))
