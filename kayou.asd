;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem kayou
  :description "Asteroid clone"
  :depends-on (glaw glop)
  :serial t
  :components
  ((:file "package")
   (:file "main")))

