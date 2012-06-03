;; defines the planet and related behavior
(in-package :org.xzpeter.game.starwar)

(defmethod initialize-instance :after ((planet planet) &key)
  (with-accessors ((r r)) planet
    (when (or (> r planet-biggest)
	      (< r planet-smallest))
      (error "planet size not right!"))
    (setf (life planet) r)))

(defmethod vect ((planet planet))
  (vector (x planet) (y planet)))
(defmethod pos ((planet planet))
  (vector (x planet) (y planet)))

(defmethod draw ((planet planet))
  (with-accessors ((s selected-p) (x x) (y y) (r r) (p player)) planet
    (let ((c (if p (planet-color p) color-grey)))
      (sdl:draw-filled-circle-* (rx x) (ry y) r :color (color-darken c 0.3))
      (sdl:draw-filled-circle-* (rx x) (ry y) (- r 5) :color (color-darken c 0.7))
      (sdl:draw-filled-circle-* (rx x) (ry y) (- r 10) :color c)
      (when s
	;; draw the selection circle
	(sdl:draw-circle-* (rx x) (ry y) (+ r 20) :color sdl:*white*)))))
;; the planet is growing all the time! spawning new stars
(defmethod grow ((planet planet))
  (with-accessors ((acc acc) (acc-max acc-max)) planet
    (incf acc)
    (when (>= acc acc-max)
      (setq acc 0)
      (when (or (not (eq (player planet) nil))
		(< (length (star-list planet))
		   planet-neutral-stars-max))
	(spawn planet)))))
;; this moves all stars from FROM to TO
(defmethod transport-stars ((from planet) (to planet))
  (dolist (star (star-list from))
    (when (eq (player star) (player from))
      (setf (planet star) to)
      (setf (star-list to) (cons star (star-list to)))
      (setf (star-list from)
	    (delete-if #'(lambda (s) (eq s star)) (star-list from))))))

(defun get-life-from-size (size)
  (let* ((ratio (/ (- star-life-max star-life-min)
		   (- planet-biggest planet-smallest)))
	 (delta (* (- size planet-smallest)
		   ratio)))
    (floor (+ star-life-min delta))))

;; one star can only be given birth by calling SPAWN of one planet.
(defmethod spawn ((planet planet))
  (with-accessors ((x x) (y y) (r r) (alist star-list)) planet
    (when (<= *star-count* star-max-amount)
      (setf alist (cons (make-instance 'star
				       :planet planet
				       :player (player planet)
				       :pos (vector x y)
				       :life (get-life-from-size r)) alist))
      (incf *star-count*))))

(defmethod mouse-inside-planet-p ((planet planet))
  (with-accessors ((x x) (y y) (r r)) planet
    (distance-less-than-p x
			  y
			  (global-x (sdl:mouse-x))
			  (global-y (sdl:mouse-y))
			  r)))
(defmethod move-star-list ((planet planet))
  "move all the stars that belongs to the planet"
  (dolist (star (star-list planet))
    (move star)))

(defmethod draw-star-list ((planet planet))
  "draw all the stars on the planet"
  (dolist (star (star-list planet))
    (draw star)))

(defmethod friend-p ((planet planet) stuff)
  (eq (player planet) (player stuff)))

(defmethod enemy-p ((planet planet) stuff)
  (not (friend-p planet stuff)))

(defmethod update-enemy-stars-in-planet ((planet planet))
  "update star status in the planet, if enemy stars met, fight each other"
  (dolist (s1 (star-list planet))
    ;; only check the enemy node
    (when (enemy-p planet s1)
      ;; if the node is very near the core of planet, it heats the planet
      ;; and reduce life of it
      (when (reach-heart-of-planet s1 planet)
	(invade-planet s1 planet))
      ;; if this node is idle, it may fight with the guardian stars
      (when (idle s1)
	(dolist (s2 (star-list planet))
	  (when (and (idle s2)
		     (enemy-p s1 s2)
		     (close-p s1 s2))
	    (attack-each-other s1 s2)
	    (return)))))))

(defmethod clear-dead-body ((planet planet))
  "clear all the dead stars"
  (with-accessors ((star-list star-list)) planet
    (dolist (star star-list)
      (when (and (dead-p star)
		 (attack-p star))
	;; if it has one component and fight to death,
	;; free the enemy to do something else
	(set-free (target star))))
    (decf *star-count*
	  (loop for star in star-list when (dead-p star) count 1))
    (setf star-list (delete-if #'dead-p star-list))))

(defmethod regenerate-life ((planet planet))
  "life regeneration of planet"
  (let ((life (+ (life planet)
		 planet-life-regeneration-speed)))
    (setf (life planet) (min (r planet)
			     life))))

(defmethod update-life ((planet planet))
  "check life status, if zero, then it should lost its player"
  (when (<= (life planet) 0)
    (setf (life planet) 0)
    (if (player planet)
	(setf (player planet) nil)
	(setf (player planet) (last-attacker planet)))))

(defmethod update ((planet planet))
  "update the planet information"
  (update-life planet)
  (regenerate-life planet)
  (update-enemy-stars-in-planet planet)
  (move-star-list planet)
  (grow planet)
  (clear-dead-body planet))

(defmethod decrease-life ((planet planet) value)
  "decrease life of a planet (possibly be invaded by other stars)"
  (setf (life planet) (- (life planet) value)))

(defmethod invade-planet ((star star) (planet planet))
  "the PLANET is invaded by STAR"
  (let ((point (life star)))
    (setf (last-attacker planet) (player star))
    (decrease-life planet point)
    (decrease-life star point)))

(defmethod power ((planet planet))
  "return the current total power of owned stars on the planet"
  (loop for star in (star-list planet) sum (life star)))

(defmethod size-bigger ((p1 planet) (p2 planet))
  "whether p1 is bigger than p2"
  )

(defun planet-sort-by-size (planet-list)
  "sort the planet list by size"
  (labels ((size-bigger (p1 p2) (> (r p1) (r p2))))
    (sort (copy-list planet-list) #'size-bigger)))

(defun planet-sort-by-player (planet-list)
  (labels ((sort-player (p1 p2) p2 (not (null (player p1)))))
    (sort (copy-list planet-list) #'sort-player)))

(defmethod planet-distance ((p1 planet) (p2 planet))
  "the distance of two planet"
  (distance (x p1) (y p1) (x p2) (y p2)))

(defmethod planet-sort-by-distance ((planet planet))
  "return the planet list sorted by the distance between the target planet
  to the owner planet"
  (labels ((cmp-func (p1 p2)
	     (< (planet-distance p1 planet)
		(planet-distance p2 planet))))
    ;; skip the first one (which must be the owner planet)
    (cdr (sort (copy-list *planet-list*) #'cmp-func))))
