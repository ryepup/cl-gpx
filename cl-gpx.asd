;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :cl-gpx
  :serial t
  ;; add new files to this list:
  :components ((:file "package") (:file "cl-gpx"))
  :depends-on (#+nil :cl-ppcre))
