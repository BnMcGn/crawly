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
