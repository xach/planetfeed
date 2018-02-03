;;;; planetfeed.lisp

;;; Update the @planet_lisp feed on twitter from the Planet Lisp RSS
;;; feed. Works by posting all RSS items not seen in recent tweets.

(in-package #:planetfeed)

(defvar *feed-url* "http://planet.lisp.org/rss20.xml")

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

(defun latest-feed ()
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

(defun latest-tweets (&key (count 10) (skip 0))
  (let ((statuses (chirp:statuses/user-timeline :count (+ count skip)
                                                :screen-name "planet_lisp")))
    (unless statuses
      (error "No statuses retrieved for @planet_lisp - WTF?"))
    (when (< (length statuses) skip)
      (error "Not enough statuses to skip ~D of them" skip))
    (subseq statuses skip)))

(defun postable-items (recent-tweets feed)
  "Return all postable items from FEED that are not found in RECENT-TWEETS."
  (let ((strings (mapcar #'chirp:text recent-tweets))
        (items (westbrook:items feed)))
    (when (< (length strings) (length items))
      (error "Too few tweets to check against the feed items"))
    (loop for item in items
          for title = (westbrook::title item)
          unless (member title strings :test 'prefixp)
          collect item)))

(defun item-tweet-text (item)
  (format nil "~A ~A"
          (westbrook::title item)
          (westbrook::link item)))

(defun tweet (thing)
  (let ((text
         (etypecase thing
           (westbrook:item
            (item-tweet-text thing))
           (string
            thing))))
    (chirp:statuses/update text)))

;;; Now do the thing

(defun update-twitter ()
  (let* ((feed (latest-feed))
         (tweets (latest-tweets :count (* (length (westbrook:items feed))
                                          2)))
         (postable (postable-items tweets feed)))
    (dolist (item postable)
      (format t "Posting ~A...~%" item)
      (tweet item))))
