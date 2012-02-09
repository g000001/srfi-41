(cl:in-package :srfi-41.internal)

(def-suite srfi-41)

(in-suite srfi-41)

(define-syntax isqu
  (syntax-rules ()
    ((isqu ?x ?y)
     (is (cl:equal ?x ?y)))))

;;; The (streams primitive) library

(test stream-primitive
  (is-true (stream-null? stream-null))
  (isqu '(a)
        (stream->list (stream-cons 'a stream-null)))
  (is-true (stream? stream-null))
  (is-true (stream-cons 'a stream-null))
  (is-true (stream-pair? (stream-cons 'a stream-null)))
  (isqu 'a (stream-car (stream-cons 'a stream-null)))
  (isqu (stream->list stream-null)
        (stream->list (stream-cdr (stream-cons 'a stream-null)))))

;;; The (streams derived) library


;;; eof
