(in-package :cl-user)
(defpackage :org.xzpeter.game.lib
  (:use :cl :lispbuilder-sdl)
  (:export :run-game			; in misc.lisp
	   :color-random
	   :color-darken
	   :color-invert
	   :color-lighten
	   :random-with-neg
	   :rad-fix			; in vector.lisp
	   :vx
	   :vy
	   :vector-norm
	   :vector-length
	   :vec2rad
	   :rad2vec
	   :vector+
	   :vector-
	   :vector*
	   :vector-floor
	   :random-vect
	   :distance
	   :distance-less-than-p))
