;;;; package.lisp

(defpackage #:crawly
  (:use #:cl)
  (:export
   #:get-record-for-url
   #:first-matching-record
   #:read-warc
   #:url-search
   #:get-archive-from-capture))
