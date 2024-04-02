;;;; crawly.asd

(asdf:defsystem #:crawly
  :description "Get and read Internet Archive and Common Crawl archives"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license  "Apache 2.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:dexador #:quri #:cl-json #:gadgets)
  :components ((:file "package")
               (:file "url")
               (:file "retry")
               (:file "crawly")))
