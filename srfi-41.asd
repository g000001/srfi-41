;;;; srfi-41.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)


(defsystem :srfi-41
  :version "20200325"
  :description "SRFI 41 for CL: Streams"
  :long-description "SRFI 41 for CL: Streams
https://srfi.schemers.org/srfi-41"
  :author "Philip L. Bewig"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:fiveam
               :mbe
               :srfi-9
               :srfi-23
               :srfi-5
               :srfi-61)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-41")
               (:file "stream-derived")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-41))))
  (let ((name "https://github.com/g000001/srfi-41")
        (nickname :srfi-41))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-41))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-41#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-41)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
