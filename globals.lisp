;; variables definitions
(in-package :org.xzpeter.game.starwar)

(defparameter *debug-string* "")

(defvar color-grey (sdl:color :r 80 :g 80 :b 80))

(defparameter screen-width 800)
(defparameter screen-height 600) 
(defparameter bg-color sdl:*black*)
(defparameter game-frame-color sdl:*white*)

(defparameter fullscreen t)
(defparameter frame-rate 60)
(defparameter scroll-speed 20)

(defvar *game-over* nil)
(defvar *news* "")
(defvar *player1* nil)
(defvar *player2* nil)
(defvar *players* nil)
(defvar *planet-list* nil)
(defvar *running* nil)
(defvar *paused* nil)
(defvar *show-arrow* nil)
(defvar *selected-planet* nil)
(defvar *star-count* 0
  "how many stars in the world")
(defvar *bg-stars* nil
  "the stars as background.")

;; in this game, I am preparing to be able to only display a little part of
;; the world map, and user can navigate by putting the mouse cursor to the
;; edge of the screen
;; *screen-pos* points to the lefttop of the current screen
(defvar *screen-pos-x* 0)
(defvar *screen-pos-y* 0)

(defparameter planet-smallest 30)
(defparameter planet-biggest 150)
(defparameter planet-default-acc-max 120)
(defparameter planet-distance-min 20
  "the minimun distance between two planet. currently only used when auto
generateing the map. ")
(defparameter planet-total 15
  "Total number of planets. Only used when auto generating maps. ")
(defparameter planet-neutral-stars-max 10
  "max stars one neutral planet can have")

;; these defines the size of the world
(defparameter world-leftmost 0)
(defparameter world-topmost 0)
(defparameter world-rightmost 1600)
(defparameter world-bottommost 1200)
(defparameter world-width (- world-rightmost world-leftmost))
(defparameter world-height (- world-bottommost world-topmost))
;; these params defines the biggest places that user can go via moving the
;; mouse
(defparameter screen-leftmost world-leftmost)
(defparameter screen-topmost world-topmost)
(defparameter screen-rightmost (- world-rightmost screen-width))
(defparameter screen-bottommost (- world-bottommost screen-height))

(defparameter star-life-max 10)
(defparameter star-life-min 3)

(defparameter star-speed-max 3.0)
(defparameter star-speed 1.5)
(defparameter star-speed-min 0.5)
(defparameter star-tail-len 15)
(defparameter star-dist-max-from-home 20
  "how far can a star moves away from its owner")

;; when the mouse moves to the edge of the screen, we should move the screen
;; position on the worldmap. 
;; these are the margin place, outside which the moving is activated. 
(defparameter margin-left 10)
(defparameter margin-right (- screen-width 10))
(defparameter margin-top 10)
(defparameter margin-bottom (- screen-height 10))

;; defines the background
(defparameter bg-star-size-max 2)
(defparameter bg-star-size-min 1)
(defparameter bg-star-n 50)
    
(defparameter *star-color-selected* sdl:*yellow*)

(defparameter star-safe-distance 40)

(defparameter planet-life-regeneration-speed 0.01)
(defparameter planet-core-radius 3)
(defparameter star-max-amount 1000)

(defun generate-bg-stars (n)
  "generate N bg stars and return list"
  (loop repeat n collect
       (list (random world-width) (random world-height)
	     (+ bg-star-size-min (random (- bg-star-size-max
					    bg-star-size-min -1))))))
(defun auto-generate-planets (amount)
  "auto generate planets and normally set the *planet-list* var.
AMOUNT is how many planet to generate in all,
INIT-AMOUNT is how many planets one player own at the beginning of game"
  (let ((width world-width)
	(height world-height)
	(total 10000)
	(count 0)
	(vlist nil))
    (labels ((x (v) (nth 0 v))
	     (y (v) (nth 1 v))
	     (r (v) (nth 2 v))
	     (outside-p (v)
	       (or (< (- (x v) (r v)) world-leftmost)
		   (> (+ (x v) (r v)) world-rightmost)
		   (< (- (y v) (r v)) world-topmost)
		   (> (+ (y v) (r v)) world-bottommost)))
	     (collide-list (v vlist)
	       (dolist (n vlist)
		 (when
		     (distance-less-than-p (x v) (y v)
					   (x n) (y n)
					   (+ (r v) (r n)
					      planet-distance-min))
		   (return t)))))
      (loop do
	   (progn
	     (decf total)
	     ;; overflow, no good result
	     (when (< total 0)
	       (error "failed auto generate map! please retry!"))
	     ;; prepare for the output of list
	     (when (>= count amount)
	       (return (loop for v in vlist collect
			    (make-instance 'planet
					   :x (x v)
					   :y (y v)
					   :r (r v)
					   :player nil))))
	     (let ((v (list (random width)
			    (random height)
			    (+ planet-smallest
			       (random
				(- planet-biggest planet-smallest))))))
	       (when (and (not (collide-list v vlist))
			  (not (outside-p v)))
		 (incf count)
		 (setq vlist (cons v vlist)))))))))

(defun set-game-running (n)
  "set running status of the game"
  (setf (sdl:frame-rate) (if n frame-rate -1)))

(defun clear-global-vars ()
  (setq *planet-list* nil)
  (setq *running* nil)
  (setq *paused* nil
	*game-over* nil)
  (setq *show-arrow* nil)
  (setq *selected-planet* nil)
  (setq *star-count* 0)
  (setq *player1*			; the real player
	(make-instance 'player
		       :planet-color sdl:*blue*
		       :star-color (color-lighten sdl:*blue* 0.6)))
  (setq *player2*			; the COM (temporarily)
	(make-instance 'player
		       :planet-color sdl:*red*
		       :star-color (color-lighten sdl:*red* 0.6)
		       :use-ai t))
  (setq *players* (list *player1* *player2*))
  (setq *screen-pos-x* 0
	*screen-pos-y* 0
	star-speed 1.5)
  (setq *news* ""
	*bg-stars* (generate-bg-stars bg-star-n)
	*planet-list* (planet-sort-by-size
		       (auto-generate-planets planet-total)))
  ;; defaultly, the first planet for player1, then player2
  (setf (player (fifth *planet-list*)) *player1*)
  (setf (player (sixth *planet-list*)) *player2*)
  (set-game-running t))
