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

(defun read-warc-record (stream)
  (let* ((wversion (gadgets:string-strip (read-line stream)))
         (header (read-header stream))
         (len (parse-integer (gethash :content-length header)))
         (content (make-array (1- len)
                              :element-type '(unsigned-byte 8);(stream-element-type stream)
                              :fill-pointer t)))
    (read-sequence content stream)
    (values (hu:hash->plist header) content wversion)))

(defun queue-up-warc-record (stream)
  (loop do (case (peek-char nil stream nil :eof)
             (:eof (return nil))
             (#\W (return t))
             (otherwise (let ((res (read-line stream nil :eof)))
                          (when (eq res :eof) (return nil)))))))

(defun read-warc (stream)
  (let ((res nil)
        (stream (flexi-streams:make-flexi-stream stream)))
    (loop for next = (queue-up-warc-record stream)
          while next
          do (multiple-value-bind (headers body) (read-warc-record stream)
               (push body res)
               (push headers res)))
    (nreverse res)))
