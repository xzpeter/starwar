;; definitions of stars
(in-package :org.xzpeter.game.starwar)

(defmethod life ((star star))
  (r star))
(defmethod (setf life) (value (star star))
  (setf (r star) value))
(defmethod initialize-instance :after ((star star) &key)
  (setf (r star) (slot-value star 'life)))

(defmethod enemy-p ((s1 star) (s2 star))
  "whether the two stars are enemies"
  (not (eq (player s1) (player s2))))

(defmethod attack ((host star) (target star))
  "make HOST star attacks the TARGET"
  (setf (status host) :attack)
  (setf (target host) target))

(defmethod attack-each-other ((s1 star) (s2 star))
  "let two stars fight each other"
  (attack s1 s2)
  (attack s2 s1))

(defun star-get-tail-point (pos vect)
  (let* ((x (vx vect))
	 (y (vy vect))
	 (a (sqrt (+ (* x x) (* y y)))))
    (vector+ (vector* vect (/ star-tail-len (* -1 a)))
	     pos)))

(defmethod draw ((star star))
  (with-accessors ((pos pos) (vect vect) (life life) (planet planet)) star
    (let* ((player (player star))
	   (color (if player
		      (star-color player)
		      (color-lighten color-grey 0.5))))
      (sdl:draw-line (rvec-int pos)
		     (rvec-int (star-get-tail-point pos vect))
		     :color sdl:*white*)
      (sdl:draw-filled-circle (vector-floor (rvec pos)) life :color
			      (if (and (eq planet *selected-planet*)
				       (eq (player planet) *player1*)
				       (eq (player star) *player1*))
				  *star-color-selected*
				  color)))))

(defun restrict-vect (vect)
  (let ((len (vector-length vect)))
    (if (> len star-speed)
	(vector* (vector-norm vect) star-speed)
	vect)))

(defmethod decrease-life ((star star) value)
  "decrease hitpoint of star"
  (setf (life star) (- (life star) value)))

(defmethod fight-each-other ((s1 star) (s2 star))
  "This time, really HURTS!!!"
  (let ((point (min (life s1) (life s2))))
    (decrease-life s1 point)
    (decrease-life s2 point)))

(defmethod move ((star star))
  "update the position of the star, and try to move it according to its
own status"
  (with-accessors ((owner planet) (pos pos)
		   (vect vect) (enemy target)) star
    ;; first, update pos
    (setf pos (vector+ pos vect))
    ;; then, update speed vect
    (setf vect
	  (let ((res
		 (restrict-vect
		  (vector+
		   ;; here is the core code defines the star movement
		   (cond
		     ;; highest priority, to fight!
		     ((attack-p star)
		      (when (collide-p star enemy)
			(fight-each-other star enemy))
		      (vector* (vector-norm (vector- (pos enemy) pos)) 5))
		     ;; on the planet of another player
		     ((enemy-p owner star)
		      (vector* (vector-norm (vector- (pos owner) pos)) 5))
		     ;; far away from home planet
		     ((not (distance-less-than-p (x owner) (y owner)
						 (vx pos) (vy pos)
						 (+ star-dist-max-from-home
						    (r owner))))
		      (vector-norm (vector- (vect owner) pos)))
		     ;; there is not much thing todo, just going around
		     (t (random-vect 0.1)))
		   vect))))
	    
	    (when (outside-world-x (vx pos))
	      (setq res (vector* res #(-1 1))))
	    (when (outside-world-y (vy pos))
	      (setq res (vector* res #(1 -1))))
	    res))))

(defmethod idle ((star star))
  "whether the star is idle"
  (eq (status star) :idle))

(defmethod attack-p ((star star))
  "whether the star is attacking"
  (eq (status star) :attack))

(defmethod close-p ((s1 star) (s2 star))
  "test whether two stars are close or not"
  (let* ((pos1 (pos s1))
	 (pos2 (pos s2))
	 (x1 (vx pos1))
	 (x2 (vx pos2))
	 (y1 (vy pos1))
	 (y2 (vy pos2)))
    (distance-less-than-p x1 y1 x2 y2 star-safe-distance)))

(defmethod set-free ((star star))
  "free one star from one single battle"
  (setf (status star) :idle)
  (setf (target star) nil))

(defmethod dead-p ((star star))
  "the star is dead? "
  (<= (life star) 0))

(defmethod reach-heart-of-planet ((star star) (planet planet))
  "check whether one star is reaching the core of the planet"
  (let* ((pos1 (pos star))
	 (x1 (vx pos1))
	 (y1 (vy pos1))
	 (x2 (x planet))
	 (y2 (y planet)))
    (distance-less-than-p x1 y1 x2 y2 planet-core-radius)))
