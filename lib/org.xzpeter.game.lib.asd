(defpackage :org.xzpeter.game.lib-system
  (:use :cl :asdf))
(in-package :org.xzpeter.game.lib-system)

(defsystem org.xzpeter.game.lib
  :name "org.xzpeter.game.lib" 
  :author "Peter Xu"
  :version "0.0.1"
  :licence "MIT"
  :description "Some basic functions related to game"
  :depends-on (:lispbuilder-sdl)
  :components ((:file "packages")
	       (:file "vector" :depends-on ("packages"))
	       (:file "misc" :depends-on ("packages"
					  "vector"))))
