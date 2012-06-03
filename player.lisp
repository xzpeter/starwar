;; for player
(in-package :org.xzpeter.game.starwar)

(defmethod player-planets ((player player))
  "find all the planets that belongs to player"
  (let ((res nil))
    (dolist (planet *planet-list*)
      (when (eq (player planet) player)
	(setq res (cons planet res))))
    res))

(defmethod power ((player player))
  "get the power of the player"
  (loop for planet in (player-planets player) sum (power planet)))

(defmethod nearest-planets ((player player) n)
  "get the nearest N planets that is near the player's planet. "
  (let ((my-planets (player-planets player))
	(res nil))
    (dolist (planet my-planets)
      (let ((enemy-planets-sorted
	     (nreverse (delete-if #'(lambda (p) (eq (player p) player))
				  (planet-sort-by-distance planet)))))
	(setq res
	      (nconc res
		     (nreverse (nthcdr
				(let ((v (- (length enemy-planets-sorted)
					    n)))
				  (if (< v 0) 0 v))
				enemy-planets-sorted))))))
    (setq res (delete-duplicates res))
    (planet-sort-by-player res)))

(defmethod update-player-ai-lv1 ((player player))
  "first attempt of AI: find nearest planet which is smaller, and conqure
it"
  (let* ((own-power (power player))
	 (nearest-planets (nearest-planets player 3))
	 (target (dolist (planet nearest-planets)
		   (when (> own-power (+ 30 (life planet) (power planet)))
		     ;; if we use full power, we mostly can conqure the planet.
		     (return planet)))))
    (when target
      (dolist (planet (player-planets player))
	(transport-stars planet target)))))

(defmethod update-player-ai ((player player))
  "update the AI logic"
  (update-player-ai-lv1 player))
