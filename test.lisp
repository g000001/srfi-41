(cl:in-package :srfi-41.internal)

(def-suite srfi-41)

(in-suite srfi-41)

(define-syntax isqu
  (syntax-rules ()
    ((isqu ?x ?y)
     (is (cl:equal ?x ?y)))))

;;; is-finite-stream-equal
(define-syntax isfsqu
  (syntax-rules ()
    ((isfsqu ?x ?y)
     (is (equal (the list ?x)
                (stream->list ?y) )))))

;;;
;;; The (streams primitive) library
;;;
(test stream-primitive
  (is-true (stream-null? stream-null))
  (isqu '(a)
        (stream->list (stream-cons 'a stream-null)) )
  (is-true (stream? stream-null))
  (is-true (stream-cons 'a stream-null))
  (is-true (stream-pair? (stream-cons 'a stream-null)))
  (isqu 'a (stream-car (stream-cons 'a stream-null)))
  (isqu (stream->list stream-null)
        (stream->list (stream-cdr (stream-cons 'a stream-null))) ))

;;;
;;; The (streams derived) library
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-stream (test-stream-map proc strm)
    (if (stream-null? strm)
        stream-null
        (stream-cons
         (funcall proc (stream-car strm))
         (stream-map proc (stream-cdr strm)) )))


  (defun test-read-char (p)
    (read-char p nil +eof+ nil) )

  (define-stream (test-file->stream filename)
      (let ((p (open filename)))
        (stream-let loop ((c (read-char p)))
                    (if (eof-object? c)
                        (begin (close p)
                               stream-null)
                        (stream-cons c
                                     (loop (test-read-char p)))))))

  (define-function (test-stream-maximum lt? strm)
    (stream-fold
     (lambda (x y) (if (funcall lt? x y) y x))
     (stream-car strm)
     (stream-cdr strm)))

  (define-stream (test-stream-member eql? obj strm)
      (stream-let loop ((strm strm))
                  (cond ((stream-null? strm) strm)
                        ((funcall eql? obj (stream-car strm)) strm)
                        (:else (loop (stream-cdr strm))))))

  (define-function (stream-partition pred? strm)
    (declare (optimize (debug 1)))
    (stream-unfolds
    (lambda (s)
      (if (stream-null? s)
          (values s '() '())
          (let ((a (stream-car s))
                (d (stream-cdr s)))
            (if (funcall pred? a)
                (values d (list a) :false)
                (values d :false (list a))))))
    strm))

  (define-stream (interleave x yy)
      (stream-match yy
                    (() (stream (stream x)))
                    ((y . ys)
                     (stream-append
                      (stream (stream-cons x yy))
                      (stream-map
                       (lambda (z) (stream-cons y z))
                       (interleave x ys))))))


  (define-stream (perms xs)
      (if (stream-null? xs)
          (stream (stream))
          (stream-concat
           (stream-map
            (lambda (ys)
              (interleave (stream-car xs) ys))
            (perms (stream-cdr xs)))))))

(test stream
  ;; define-stream
  (isfsqu '(2 3 4 5)
          (test-stream-map #'1+ (list->stream '(1 2 3 4))) )
  ;; list->stream
  (for-all ((e (gen-list)))
    (is (equal e (stream->list (list->stream e)))) )
  ;; stream & file
  (let ((file (asdf:system-relative-pathname :srfi-41 "srfi-41.lisp" )))
    (is (string= (coerce
                  (stream->list (test-file->stream file))
                  'string )
                 (with-open-file (in file)
                   (let ((s (make-string (file-length in))))
                     (read-sequence s in)
                     s )))))
  ;; stream
  (isfsqu '(1 2 3 4)
          (stream 1 2 3 4) )
  ;; stream-append
  (for-all ((e (gen-list))
            (z (gen-list)) )
    (isfsqu (append e z)
            (stream-append (list->stream e)
                           (list->stream z) )))
  ;; stream-concat
  (isfsqu '(1 2 3 2 1)
          (stream-concat
           (stream (stream 1 2)
                   (stream)
                   (stream 3 2 1) )))
  (isfsqu '(1 1 1 1 1)
          (stream-take 5 (stream-constant 1)) )
  (isfsqu '(1 2 1 2 1)
          (stream-take 5 (stream-constant 1 2)) )
  ;; stream-drop
  (isfsqu '(1 1 1 1 1)
          (stream-take 5 (stream-drop 5 (stream-constant 1))) )
  (is (null (stream->list
             (stream-drop 5 stream-null) )))
  ;; stream-drop-while

  (isfsqu '(100 101 102 103 104)
          (stream-take 5
                       (stream-drop-while (lambda (x) (< x 100))
                                          (stream-from 0) )))
  (isfsqu '(1 3 5 7 9)
          (stream-take 5 (stream-filter #'oddp (stream-from 0))) )
  ;; stream-fold
  (is (= 9
         (test-stream-maximum #'< (stream-take 10 (stream-from 0))) ))
  ;; stream-for-each
  (is (string= "0123456789"
               (with-output-to-string (w)
                 (stream-for-each (lambda (x) (princ x w))
                                  (stream-take 10
                                               (stream-from 0) )))))
  ;; stream-iterate
  (isfsqu '(0 1 2 3 4)
          (stream-take 5 (stream-iterate (lambda (x) (+ x 1)) 0)) )
  ;; stream-length
  (is (= 5 (stream-length (stream-take 5 (stream-from 0)))))
  ;; stream-let
  (isfsqu '(5 6 7 8 9)
          (stream-take 5 (test-stream-member #'= 5 (stream-from 0))) )
  ;; stream-map
  (isfsqu '(5 6 7 8 9)
          (stream-take 5 (stream-map (lambda (x) (+ 5 x))
                                     (stream-from 0) )))
  ;; stream-of
  (isfsqu '(0 4 16 36 64)
          (stream-of (* x x)
                     (x :in (stream-range 0 10))
                     (even? x) ))
  (isfsqu '((1 1) (1 2) (2 1) (2 2) (3 1) (3 2))
          (stream-of (list a b)
                     (a :in (stream-range 1 4))
                     (b :in (stream-range 1 3)) ))
  (isfsqu '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4))
          (stream-of (list i j)
                     (i :in (stream-range 1 5))
                     (j :in (stream-range (+ i 1) 5)) ))
  (isfsqu '(0 1 2 3 4 5 6 7 8 9)
          (stream-range 0 10) )
  (isfsqu '(0 2 4 6 8)
          (stream-range 0 10 2) )
  ;; stream-ref
  (is (= 100 (stream-ref (stream-from 0) 100)))
  ;;
  (is (equal (stream->list
              (stream-take 100 (stream-from 0)) )
             (stream->list
              (stream-reverse (stream-take 100 (stream-range 99 -1))) )))
  (isfsqu '(0 1 3 6 10 15 21 28)
          (stream-take 8 (stream-scan #'+ 0 (stream-from 1))) )
  (isfsqu '(0 1 2 3 4 5 6 7 8 9)
          (stream-take-while (lambda (x) (< x 10))
                             (stream-from 0) ))
  (isfsqu '(0 1 4 9 16 25 36 49 64 81)
          (stream-unfold
           (lambda (x) (expt x 2)) ; map
           (lambda (x) (< x 10))   ; pred?
           (lambda (x) (+ x 1))    ; gen
           0 ))
  (is (equal '((1 3 5) (2 4))
             (multiple-value-call
               (lambda (odds evens)
                 (list (stream->list odds)
                       (stream->list evens) ))
               (stream-partition #'oddp
                                 (stream-range 1 6) ))))
  (isfsqu '((0 1) (0 2) (0 3) (0 4) (0 5) (0 6) (0 7) (0 8))
          (stream-take 8 (stream-zip (stream-constant 0)
                                     (stream-from 1) )))
  (isfsqu '((A 1 2 3 4) (1 A 2 3 4) (1 2 A 3 4) (1 2 3 A 4) (1 2 3 4 A))
          (stream-map #'stream->list (interleave 'a (list->stream '(1 2 3 4)))) )
  (isfsqu '((1 2 3 4) (2 1 3 4) (2 3 1 4) (2 3 4 1) (1 3 2 4) (3 1 2 4) (3 2 1 4)
            (3 2 4 1) (1 3 4 2) (3 1 4 2) (3 4 1 2) (3 4 2 1) (1 2 4 3) (2 1 4 3)
            (2 4 1 3) (2 4 3 1) (1 4 2 3) (4 1 2 3) (4 2 1 3) (4 2 3 1) (1 4 3 2)
            (4 1 3 2) (4 3 1 2) (4 3 2 1))
          (stream-map #'stream->list (perms (list->stream '(1 2 3 4))))))

;;; eof
