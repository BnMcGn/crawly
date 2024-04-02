;;;; crawly.lisp

(in-package #:crawly)

(defun get-cc-indice-data ()
  (cl-json:decode-json-from-string (dex:get (add-cc-endpoints *cc-index-url*))))

(defun get-cc-indices ()
  (sort
   (mapcar (lambda (x) (gadgets:assoc-cdr :cdx-api x)) (get-cc-indice-data))
   #'string>))

(defgeneric get-indice-list (source &key n)
  (:method ((source (eql :common-crawl)) &key (n 10))
    (subseq (get-cc-indices) 0 n))
  (:method ((source (eql :internet-archive)) &key n)
    (declare (ignore n))
    (list (add-ia-index *ia-index-url*))))

(defparameter *cdx-field-transforms* '((:statuscode . :status) (:original . :url) (:mimetype . :mime)))

(defun transform-fields (fields)
  (mapcar (lambda (field)
            (let ((itm (assoc field *cdx-field-transforms*)))
              (if itm
                  (cdr itm)
                  field)))
          fields))

(defgeneric pre-process-captures (source endpoint data)
  (:method ((source (eql :common-crawl)) endpoint data) (list (cons (cons :endpoint endpoint) data)))
  (:method ((source (eql :internet-archive)) endpoint data)
    (let* ((headers (mapcar (lambda (x) (alexandria:make-keyword (string-upcase x))) (first data)))
           (headers (transform-fields headers)))
      (loop for row in (rest data)
            collect (loop for k in headers
                          for v in row
                          collecting (cons k v) into c
                          finally (return (list* (cons :endpoint endpoint) c)))))))

(defun process-capture (data)
  (gadgets:with-alist-keys ((:offset :length) data)
    (if length
        (remove-if-not #'identity
                       (list* (when offset (cons :offset (parse-integer offset)))
                              (cons :length (parse-integer length))
                              data))
        data)))

(defun url-search (search &key (limit 1000) filter (source :common-crawl) endpoints)
  (let ((endpoints (if endpoints
                       endpoints
                       (get-indice-list source)))
        (res nil))
    (dolist (e endpoints)
      (let ((page (with-response-handling
                    (dex:get
                     (add-search e search :limit limit :filter filter)))))
        (when page
          (push (mapcar
                 #'process-capture
                 (pre-process-captures
                  source
                  e
                  (cl-json:decode-json-from-string page))) res))))
    (gadgets:flatten-1 (nreverse res))))

(defgeneric get-archive-from-capture (source capture)
  (:method ((source (eql :common-crawl)) capture)
    (gadgets:with-alist-keys ((:filename :offset :length) capture)
      (dex:get (add-cc-data *cc-data-url* :filename filename)
               :headers (make-cc-range-header offset length))))
  (:method ((source (eql :internet-archive)) capture)
    (gadgets:with-alist-keys ((:url :timestamp) capture)
      (dex:get (add-ia-data *ia-index-url* :url url :timestamp timestamp)))))
