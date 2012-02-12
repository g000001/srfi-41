;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-41
  (:use)
  (:export
   :stream-null :stream-cons :stream? :stream-null? :stream-pair?
   :stream-car :stream-cdr :stream-lambda :define-stream :list->stream
   :port->stream :stream :stream->list :stream-append :stream-concat
   :stream-constant :stream-drop :stream-drop-while :stream-filter :stream-fold
   :stream-for-each :stream-from :stream-iterate :stream-length :stream-let
   :stream-map :stream-match ;; :_
   :stream-of :stream-range :stream-ref
   :stream-reverse :stream-scan :stream-take :stream-take-while :stream-unfold
   :stream-unfolds :stream-zip))

(defpackage :srfi-41.internal
  (:use :srfi-41 :cl :fiveam :srfi-23 :srfi-9 :mbe)
  (:shadowing-import-from :srfi-23 :error)
  (:shadow :loop :lambda :assoc :member :map :stream))

;;; eof
