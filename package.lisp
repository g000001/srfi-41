;;;; package.lisp

(cl:in-package common-lisp-user)


(eval-when (:load-toplevel :compile-toplevel :execute)
  (intern "_" 'common-lisp-user))


(defpackage "https://github.com/g000001/srfi-41"
  (:use)
  (:export
   stream-null stream-cons stream? stream-null? stream-pair?
   stream-car stream-cdr stream-lambda define-stream list->stream
   port->stream stream stream->list stream-append stream-concat
   stream-constant stream-drop stream-drop-while stream-filter
   stream-fold stream-for-each stream-from stream-iterate stream-length
   stream-let stream-map stream-match cl-user::_ stream-of stream-range
   stream-ref stream-reverse stream-scan stream-take stream-take-while
   stream-unfold stream-unfolds stream-zip)
  (:shadowing-import-from common-lisp-user _))


(defpackage "https://github.com/g000001/srfi-41#internals"
  (:use
   "https://github.com/g000001/srfi-41"
   "https://github.com/g000001/srfi-23"
   "https://github.com/g000001/srfi-61"
   "https://github.com/g000001/srfi-9"
   cl
   fiveam
   mbe)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-23" 
   error)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-61" 
   cond)
  (:shadow loop lambda assoc member map stream))


;;; *EOF*
