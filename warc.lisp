;;;; warc.lisp

(in-package #:crawly)

(defun process-field (item)
  (multiple-value-bind (k v) (gadgets:split-sequence-on-subseq ":" item)
    (list (alexandria:make-keyword (string-upcase (gadgets:string-strip k)))
          (gadgets:string-strip v))))

(defun chunk-field (stream)
  (gadgets:string-join
   #\Newline
   (loop for line = (read-line stream)
         collecting line
         until (not (member (peek-char nil stream) '(#\Space #\Tab))))))

(defun process-header (stream)
  (hu:collecting-hash-table (:mode :keep)
    (loop for line = (chunk-field stream)
          for (k v) = (process-field line)
          until (gadgets:length1 line)
          do (hu:collect k v))))

(defun process-warc-record (stream)
  (let ((wversion (gadgets:string-strip (read-line stream)))
        (header (process-header stream)))
    (values wversion (hu:hash->plist header))))
