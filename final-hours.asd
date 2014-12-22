;;;; final-hours.asd

(asdf:defsystem #:final-hours
  :description "A Remake of the Classic Game Missile Command"
  :author "Jan Tatham <jan@sebity.com>"
  :license "GPL v2"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-ttf
               #:lispbuilder-sdl-mixer)
  :serial t
  :components ((:file "package")
               (:file "final-hours")))

