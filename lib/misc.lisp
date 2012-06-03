;; I will put misc functions related to CL game here
(in-package :org.xzpeter.game.lib)

;; use run-game to run any game with threading
(defvar *game-thread* nil)
(defun run-game (main)
  ;; since we are using sbcl with linux, we can use thread
  (setq *game-thread* (sb-thread:make-thread main))
  (setf (sb-thread:thread-name *game-thread*) "game-thread"))

;; (defun get-random-color ()
;;   (let* ((color-list (list sdl:*red* sdl:*green* sdl:*yellow*
;; 			   sdl:*blue* sdl:*cyan* sdl:*white*
;; 			   sdl:*magenta*)))
;;     (nth (random (length color-list)) color-list)))

(defun color-random ()
  (sdl:color :r (random 255) :g (random 255) :b (random 255)))

(defun color-darken (c r)
  (sdl:color :r (* r (sdl:r c))
	     :g (* r (sdl:g c))
	     :b (* r (sdl:b c))))

(defun color-invert (c)
  (sdl:color :r (- 255 (sdl:r c))
	     :g (- 255 (sdl:g c))
	     :b (- 255 (sdl:b c))))

(defun color-lighten (c0 ratio)
  "lighten up color C"
  (let ((c (color-invert c0)))
    (setq c (color-darken c ratio))
    (color-invert c)))

(defun random-with-neg (n)
  (let ((f (float n)))
    (- (random (* f 2)) f)))
