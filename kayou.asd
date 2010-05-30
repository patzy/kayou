;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem kayou
  :description "Asteroid clone"
  :depends-on (glaw glaw-imago glop)
  :serial t
  :components
  ((:file "package")
   (:file "main")))

