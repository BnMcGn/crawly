;;;; url.lisp

(in-package #:crawly)



(defparameter *data-urls* '("commoncrawl.s3.amazonaws.com" "data.commoncrawl.org" "web.archive.org"))

(defparameter *cc-index-url* "https://index.commoncrawl.org/")

(defparameter *cc-data-url* "https://data.commoncrawl.org/")

(defun add-cc-endpoints (url) (quri:merge-uris "/collinfo.json" url))

(defparameter *ia-index-url* "https://web.archive.org/")

(defun add-ia-index (url) (quri:merge-uris "/cdx/search/cdx" url))

(defun add-search (url search &key (limit 1000) filter)
  (quri:merge-uris
   (quri:make-uri :query (list (cons "url" search)
                               (cons "limit" limit)
                               (cons "filter" filter)
                               (cons "output" "json")))
   url))

(defun add-ia-data (base &key url timestamp)
  (quri:merge-uris
   (format nil "/web/~aid_/~a" timestamp (quri:url-encode url))
   base))

;;Find out how to get warcs from internet archive
;;(defun add-ia-data (base &key ))

(defun make-cc-range-header (offset length)
  (list (cons "Range" (format nil "bytes=~a-~a" offset (+ offset length 1)))))

(defun add-cc-data (base &key filename)
  (quri:merge-uris
   filename
   base))
