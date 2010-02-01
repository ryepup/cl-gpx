;;; -*- mode: lisp; indent-tabs: nil -*-

(asdf:defsystem #:cl-gpx
  :serial t
  ;; add new files to this list:
  :components ((:file "package") (:file "cl-gpx"))
  :depends-on (#:iterate #:alexandria #:vecto #:cxml))
