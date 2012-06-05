(in-package :org.xzpeter.game.starwar)

;; game control
;; [LEFT BUTTON] to select planet
;; hold [LEFT] to move stars
;; move to screen edges to navigate the world

(defun dbg (s)
  (setq *debug-string* s))

(defun outside-world-x (x)
  (or (<= x world-leftmost) (>= x world-rightmost)))

(defun outside-world-y (y)
  (or (<= y world-topmost) (>= y world-bottommost)))

;; the relative X/Y axis position. we should be aware that, if what we are
;; trying to draw has WORLDWIDE position (rather than the screen
;; position), we should fix those axis with RX and RY to get relative
;; position of the object.
(defun rx (x)
  (- x *screen-pos-x*))
(defun ry (y)
  (- y *screen-pos-y*))
(defun rvec (vec)
  (vector (rx (vx vec)) (ry (vy vec))))
(defun rvec-int (vect)
  (vector-floor (rvec vect)))
(defun global-x (x)
  (+ x *screen-pos-x*))
(defun global-y (y)
  (+ y *screen-pos-y*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc functions used in this game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun buggy-win ()
  "tricky winner!"
  (dolist (planet *planet-list*)
    (setf (player planet) *player1*)))

(defun clear-selected-planet ()
  (dolist (planet *planet-list*)
    (setf (selected-p planet) nil))
  (setq *selected-planet* nil))

(defun handle-mouse-button (button xm ym down)
  (if down
      (when (= button 1)
	(clear-selected-planet)
	(dolist (planet *planet-list*)
	  (with-accessors ((x x) (y y) (r r) (s selected-p)) planet
	    (when (distance-less-than-p x y (global-x xm) (global-y ym) r)
	      (setf s t)
	      (setq *selected-planet* planet)
	      (if (sdl:key-down-p :sdl-key-space)
		  (spawn planet))
	      (return))))
	(when (and *selected-planet*
		   (eq (player *selected-planet*) *player1*))
	  (setq *show-arrow* t)))
      ;; button up
      (when (= button 1)
	(when *show-arrow*
	  (dolist (planet *planet-list*)
	    (when (and (mouse-inside-planet-p planet)
		       (not (eq planet *selected-planet*)))
	      ;; trying to move a star to another planet
	      (transport-stars *selected-planet* planet)
	      (return))))
	(setq *show-arrow* nil))))

(defun increase-star-speed ()
  (let ((speed (+ star-speed 0.5)))
    (if (> speed star-speed-max)
	(setq speed star-speed-max))
    (setq star-speed speed))
  (setq *news* (format nil "Increase speed to ~ax" star-speed)))
(defun decrease-star-speed ()
  (let ((speed (- star-speed 0.5)))
    (if (< speed star-speed-min)
	(setq speed star-speed-min))
    (setq star-speed speed))
  (setq *news* (format nil "Decrease speed to ~ax" star-speed)))

(defun handle-key (key)
  (case key
    (:sdl-key-escape
     (sdl:push-quit-event))
    (:sdl-key-p
     (setf *paused*
	   (not *paused*))
     (set-game-running *paused*))
    (:sdl-key-r
     (initialize-game))
    (:sdl-key-minus
     (decrease-star-speed))
    (:sdl-key-equals
     (increase-star-speed))
    (:sdl-key-space
     (when *game-over*
       (clear-global-vars)
       (set-game-running t)))))

;; draw the information line by line
(defun draw-information (&rest infos)
  (let ((x 10)
	(start-y 10)
	(step-y 15))
    (dolist (info infos)
      (sdl:draw-string-solid-* info x start-y)
      (setq start-y (+ step-y start-y)))))

(defun scroll-screen (direction)
  (case direction
    (:left (decf *screen-pos-x* scroll-speed))
    (:right (incf *screen-pos-x* scroll-speed))
    (:up (decf *screen-pos-y* scroll-speed))
    (:down (incf *screen-pos-y* scroll-speed))
    (otherwise (error "unknown direction!"))))
(defun fix-screen-pos-overflow ()
  (when (> *screen-pos-x* screen-rightmost)
    (setq *screen-pos-x* screen-rightmost))
  (when (< *screen-pos-x* screen-leftmost)
    (setq *screen-pos-x* screen-leftmost))
  (when (> *screen-pos-y* screen-bottommost)
    (setq *screen-pos-y* screen-bottommost))
  (when (< *screen-pos-y* screen-topmost)
    (setq *screen-pos-y* screen-topmost)))
(defun move-screen-on-worldmap ()
  (let ((x (sdl:mouse-x)) (y (sdl:mouse-y)))
    (when (or (<= x margin-left)
	      (sdl:key-down-p :sdl-key-left))
      (scroll-screen :left))
    (when (or (>= x margin-right)
	      (sdl:key-down-p :sdl-key-right))
      (scroll-screen :right))
    (when (or (<= y margin-top)
	      (sdl:key-down-p :sdl-key-up))
      (scroll-screen :up))
    (when (or (>= y margin-bottom)
	      (sdl:key-down-p :sdl-key-down))
      (scroll-screen :down)))
  (fix-screen-pos-overflow))

(defun draw-game-frame ()
  (sdl:draw-rectangle-* (rx world-leftmost)
			(ry world-topmost)
			(- world-rightmost world-leftmost)
			(- world-bottommost world-topmost)
			:color game-frame-color))

(defun draw-planet-list ()
  (dolist (planet *planet-list*)
    (draw planet)))

(defun update-planet-list ()
  "update the status of planets, including the stars on it"
  (dolist (planet *planet-list*)
    (update planet)))

(defun draw-arrow (start end &key (color sdl:*white*))
  (sdl:draw-line start end :color color)
  (let* ((rad (vec2rad (vector- end start)))
	 (v1 (rad2vec (rad-fix (+ rad 0.35 pi))))
	 (v2 (rad2vec (rad-fix (- rad 0.35 pi))))
	 (p1 (vector+ end (vector* (vector-norm v1) 20)))
	 (p2 (vector+ end (vector* (vector-norm v2) 20))))
    (sdl:draw-line end (vector-floor p1) :color color)
    (sdl:draw-line end (vector-floor p2) :color color)))

(defun draw-indication-arrow ()
  (when (and *show-arrow*
	     (not (mouse-inside-planet-p *selected-planet*))) 
    (let ((start (rvec (vect *selected-planet*)))
	  ;; if target point inside a planet, directly arrow to
	  ;; that
	  (end (dolist (planet *planet-list*
			(vector (sdl:mouse-x) (sdl:mouse-y)))
		 (when (mouse-inside-planet-p planet)
		   (return (rvec (vect planet)))))))
      (draw-arrow start end :color sdl:*white*))))

(defun draw-stars ()
  (dolist (planet *planet-list*)
    (draw-star-list planet)))

(defun draw-background (star-list)
  "draw the bg stars"
  (dolist (info star-list)
    (destructuring-bind (x y r) info
      (sdl:draw-filled-circle-* (rx x) (ry y) r :color sdl:*white*))))

(defun display-percentage-rectangle (pos width height value full-value)
  "POS is the right-bottom point of the rect, WIDTHxHEIGHT is the size of
the outter rect. the rect is filled by VALUE/FULL-VALUE"
  (let ((p (vector- pos (vector width height))))
    (sdl:draw-rectangle-* (vx p) (vy p) width height))
  (let* ((per (/ value full-value))
	 (w (* (- width 10) per))
	 (h (- height 10))
	 (p (vector- pos (vector 5 5)))
	 (plist (list (vector+ p (vector (- w) (- h)))
		      (vector+ p (vector 0 (- h)))
		      p
		      (vector+ p (vector (- w) 0)))))
    (sdl:draw-filled-polygon plist :color sdl:*white*)))

(defun display-planet-life ()
  "if one planet is selected, display its life on the screen"
  (if *selected-planet*
      (display-percentage-rectangle (vector (- screen-width 60)
					    (- screen-height 20))
				    (r *selected-planet*) 24
				    (life *selected-planet*)
				    (r *selected-planet*))))

(defun winner-p ()
  "detect if there is winner"
  (let ((p1 nil)
	(p2 nil))
    (dolist (planet *planet-list*)
      (when (eq (player planet) *player1*)
	(setq p1 t))
      (when (eq (player planet) *player2*)
	(setq p2 t)))
    (if (null p1)
	*player2*
	(if (null p2)
	    *player1*
	    nil))))

(defun draw-game-over ()
  (let ((win (eq (winner-p) *player1*)))
    (sdl:draw-string-solid-* (concatenate 'string
					  (if win
					      "You WIN!"
					      "You LOSE!")
					  " Press [SPACE] to start new game!")
			     (- (/ screen-width 2) 170)
			     (/ screen-height 2)
			     :color sdl:*yellow*)))

(defun update-AI ()
  (dolist (player *players*)
    (when (use-ai player)
      (update-player-ai player))))

(defun set-game-running (n)
  "set running status of the game"
  (setf (sdl:frame-rate) (if n frame-rate -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main thread
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun main ()
  ;; this might be useful when making binary images
  (in-package :org.xzpeter.game.starwar)
  (sdl:with-init (sdl:sdl-init-video)
    (setq sdl:*default-font* (sdl:initialise-font sdl:*font-8x13o*))
    ;; (sdl:initialise-default-font sdl:*font-9x18b*)
    (clear-global-vars)
    (format t "fullscreen: ~a~%" fullscreen)
    (sdl:window screen-width screen-height
		:fullscreen fullscreen
		:title-caption "Star War"
		:icon-caption "Star War")
    (set-game-running t)
    (sdl:with-events ()
      (:quit-event () (prog1 t
			(setf *running* nil)
			(format t "Quit.")))
      (:key-down-event (:key key)
		       (handle-key key))
      (:mouse-button-down-event (:button button :x x :y y)
				(handle-mouse-button button x y t))
      (:mouse-button-up-event (:button button :x x :y y)
			      (handle-mouse-button button x y nil))
      (:idle ()
	     (unless *game-over*
	       (sdl:clear-display bg-color)

	       (unless *paused*
		 (update-planet-list))
	       (move-screen-on-worldmap)

	       ;; do all the drawings here
	       (draw-background *bg-stars*)
	       (draw-planet-list)
	       (draw-stars)
	       (draw-indication-arrow)
	       (draw-game-frame)
	       (display-planet-life)

	       ;; if the player is COM, update AI control
	       (update-AI)

	       ;; whether there is a winner
	       (let ((winner (winner-p)))
		 (when winner
		   (set-game-running nil)
		   (setq *game-over* t)
		   (draw-game-over)))
	       
	       (draw-information "Welcome Game StarWar"
				 (format nil "CurPos: (~a,~a)"
					 (+ *screen-pos-x* (sdl:mouse-x))
					 (+ *screen-pos-y* (sdl:mouse-y)))
				 (format nil "CurStars: ~a/~a" 
					 *star-count*
					 star-max-amount)
				 (concatenate 'string
					      (if *paused*
						  "[P] to UNPAUSE"
						  "[P] to PAUSE")
					      ", [ESC] to quit")
				 *news*
				 *debug-string*)
	       
	       (sdl:update-display))))))

(defun run ()
  (run-game #'main))
