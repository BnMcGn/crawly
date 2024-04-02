;;;; retry.lisp

(in-package #:crawly)

(defun call-with-response-handling (func)
  (handler-case
    (let ((retry5 (dex:retry-request 5 :interval 1)))
      (handler-bind
          ((dex:http-request-too-many-requests retry5) ;429
           (dex:http-request-internal-server-error retry5) ;500
           (dex:http-request-bad-gateway retry5) ;502
           (dex:http-request-service-unavailable retry5) ;503
           (dex:http-request-gateway-timeout retry5) ;504
           ;;Shoultd do something with 400, 404 (bad-request, not-found)?
           )
        (funcall func)))
    (dex:http-request-failed (e)
      (declare (ignore e))
      nil)))

(defmacro with-response-handling (&body body)
  `(call-with-response-handling (lambda () ,@body)))
