;;;; crawly.asd

(asdf:defsystem #:crawly
  :description "Get and read Internet Archive and Common Crawl archives"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license  "Apache 2.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:dexador #:quri #:cl-json #:gadgets 
               #:flexi-streams #:cl-hash-util #:gzip-stream)
  :components ((:file "package")
               (:file "util")
               (:file "warc")
               (:file "url")
               (:file "retry")
               (:file "crawly")))
