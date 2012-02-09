;;;; srfi-41.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :srfi-41
  :serial t
  :depends-on (:fiveam
               :mbe
               :srfi-9
               :srfi-23
               :srfi-5)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-41")
               (:file "stream-derived")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-41))))
  (load-system :srfi-41)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-41.internal :srfi-41))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
