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

(defun read-warc-record (stream &optional decider)
  (let* ((wversion (gadgets:string-strip (read-line stream)))
         (header (read-header stream))
         (len (parse-integer (gethash :content-length header)))
         (read? (when decider (funcall decider header)))
         (content (when read?
                    (make-array (1- len)
                              :element-type '(unsigned-byte 8);(stream-element-type stream)
                              :fill-pointer t))))
    (if read?
        (read-sequence content stream)
        (file-position stream (+ (file-position stream) len)))
    (values header content wversion)))

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

(defun first-matching-record (stream-or-path predicate)
  (stream-or-path stream-or-path stream
    (loop for next = (queue-up-warc-record stream)
          while next
          do (multiple-value-bind (_ body __) (read-warc-record stream predicate)
               (declare (ignore _ __))
               (when body (return body))))))

(defun get-record-for-url (stream-or-path url)
  (first-matching-record
   stream-or-path
   (lambda (headers)
     (hu:with-keys (:warc-type :warc-target-uri :content-length) headers
       (and (string= warc-type "request")
            (string= warc-target-uri url)
            (> (parse-integer content-length) 700)
            (print (hu:hash->plist headers)))))))
