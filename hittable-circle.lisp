;; defines the hittable-circle object
(in-package :org.xzpeter.game.starwar)

(defclass hittable-circle ()
  ((pos :initarg :pos :initform (error "must have init position")
	:accessor pos :documentation "center of the circle")
   (r :initarg :r :initform 0 
      :accessor r :documentation "radius of the circle"))
  (:documentation "A circle that is hittable"))

(defmethod collide-p ((a hittable-circle) (b hittable-circle))
  "Whether two hittable circles collided or not"
  (with-accessors ((pos1 pos) (r1 r)) a
    (with-accessors ((pos2 pos) (r2 r)) b
      (let ((x1 (vx pos1))
	    (x2 (vx pos2))
	    (y1 (vy pos1))
	    (y2 (vy pos2)))
	(distance-less-than-p x1 y1 x2 y2 (+ r1 r2))))))
