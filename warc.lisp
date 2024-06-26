;;;; warc.lisp

(in-package #:crawly)

(defun read-field (item)
  (multiple-value-bind (k v) (gadgets:split-sequence-on-subseq ":" item)
    (list (alexandria:make-keyword (string-upcase (gadgets:string-strip k)))
          (gadgets:string-strip v))))

(defun chunk-field (stream)
  (gadgets:string-join
   #\Newline
   (loop for line = (read-line stream)
         collecting line
         until (not (member (peek-char nil stream) '(#\Space #\Tab))))))

(defun read-header (stream)
  (hu:collecting-hash-table (:mode :keep)
    (loop for line = (chunk-field stream)
          for (k v) = (read-field line)
          until (gadgets:length1 line)
          do (hu:collect k v))))

(defun read-warc-header (stream)
  (let* ((wversion (gadgets:string-strip (read-line stream)))
         (header (read-header stream))
         (len (parse-integer (gethash :content-length header))))
    (values header len wversion)))

(defun read-chunk-to-octets (stream length)
  (let ((content (make-array length :element-type '(unsigned-byte 8) :fill-pointer t)))
    (read-sequence content stream)
    content))

(defun read-warc-record (stream &optional decider)
  (multiple-value-bind (header len wversion) (read-warc-header stream)
    (let* ((read? (when decider (funcall decider header))))
      (unless read?
        (file-position stream (+ (file-position stream) len)))
      (values header (when read? (read-chunk-to-octets stream (1- len))) wversion))))

(defun queue-up-warc-record (stream)
  (loop do (case (peek-char nil stream nil :eof)
             (:eof (return nil))
             (#\W (return t))
             (otherwise (let ((res (read-line stream nil :eof)))
                          (when (eq res :eof) (return nil)))))))

(defun read-warc (stream)
  (let ((res nil))
    (stream-or-path stream stream
      (loop for next = (queue-up-warc-record stream)
            while next
            do (multiple-value-bind (headers body) (read-warc-record stream)
                 (push headers res)
                 (push body res))))
    (nreverse res)))

(define-condition warc-record-not-found (error)
  ((text :initarg :text :reader text)))

(defun first-matching-record (stream-or-path predicate &key (return-type :content))
  (stream-or-path stream-or-path stream
    (loop for next = (queue-up-warc-record stream)
          while next
          do (multiple-value-bind (header clen) (read-warc-header stream)
               (if (funcall predicate header)
                   (return (case return-type
                             (:content (read-chunk-to-octets stream (1- clen)))
                             (:length clen)
                             (:consume (progn
                                         (file-position stream (+ (file-position stream) clen))
                                         t))))
                   (file-position stream (+ (file-position stream) clen))))
          finally (error 'warc-record-not-found :text "Stream exhausted with no match"))))

(defun get-response-payload (stream length)
  (let* ((start-pos (file-position stream))
         (status (read-line stream))
         (header (read-header stream))
         (body-length (- length (- (file-position stream) start-pos))))
    (flexi-streams:octets-to-string (read-chunk-to-octets stream body-length))))

(defun uri-tidify (uri)
  (let* ((u (quri:uri uri))
         (path (quri:uri-path u))
         (path (if (alexandria:emptyp path) "/" path))
         (host (quri:uri-host u))
         (host (if (alexandria:starts-with-subseq "www." host)
                   (subseq host 4)
                   host))
         (scheme (quri:uri-scheme u))
         (scheme (if (eq "https" scheme) "http" scheme)))
    (quri:copy-uri u :scheme scheme :path path :host host)))

(defun uri-equalish (uri1 uri2)
  (quri:uri= (uri-tidify uri1) (uri-tidify uri2)))

(defun get-record-for-url (stream-or-path url &key (url-comparison #'uri-equalish))
  (stream-or-path stream-or-path strx
    (with-file-buffered-stream (strx stream)
        (let ((length (first-matching-record
                       stream
                       (lambda (headers)
                         (hu:with-keys (:warc-type :warc-target-uri :content-length) headers
                           (and (string= warc-type "response")
                                (funcall url-comparison warc-target-uri url))))
                       :return-type :length)))
          (when (< 0 length)
            (get-response-payload stream length))))))
