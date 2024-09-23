;;;; util.lisp

(in-package #:crawly)

(defmacro stream-or-path (stream-or-path var &body body)
  (alexandria:once-only (stream-or-path)
    (alexandria:with-gensyms (tvar)
      `(if (or (pathnamep ,stream-or-path) (stringp ,stream-or-path))
           (with-open-file (,tvar ,stream-or-path :element-type '(unsigned-byte 8))
             (let ((,var (flexi-streams:make-flexi-stream ,tvar)))
               ,@body))
           (let ((,var (flexi-streams:make-flexi-stream ,stream-or-path)))
             ,@body)))))

(defvar *buffered-file-path*)

;;Because we can't get a 'file-position' out of chipz streams
(defmacro with-file-buffered-stream ((in out) &body body)
  (alexandria:with-gensyms (thandle ihandle)
    `(uiop:with-temporary-file (:pathname *buffered-file-path*
                                :stream ,ihandle
                                :direction :output)
       (uiop:copy-stream-to-stream ,in ,ihandle)
       (finish-output ,ihandle)
       (with-open-file (,thandle *buffered-file-path*
                        :direction :input
                        :element-type '(unsigned-byte 8))
         (let ((,out (flexi-streams:make-flexi-stream ,thandle)))
           ,@body)))))
