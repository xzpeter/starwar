;; define all the classes in the game
(in-package :org.xzpeter.game.starwar)

(defclass star (hittable-circle)
  ((planet :initarg :planet :initform (error "cannot without planet!")
	   :accessor planet)
   (player :initarg :player :initform (error "must have a player")
	   :accessor player :documentation
	   "the player side it belongs to")
   (vect :initarg :vect :initform #(0.5 0) :accessor vect)
   (life :initarg :life :initform star-life-min)
   (target :initform nil :accessor target :documentation
	   "target star to fight with") 
   (status :initform :idle :accessor status :documentation
	   "what does the star think now? possble status are:
:idle -- not doing anything now
:attack -- met some bad guys, and willing to fight!")))

(defclass planet ()
  ((x :initarg :x :initform (random screen-rightmost)
      :accessor x)
   (y :initarg :y :initform (random screen-bottommost)
      :accessor y)
   ;; (color :initarg :color :initform sdl:*white* :accessor color)
   (r :initarg :r
      :initform (+ planet-smallest
		   (random (- planet-biggest planet-smallest)))
      :accessor r)
   (selected :initform nil :accessor selected-p)
   (star-list :initarg star-list :initform nil :accessor star-list)
   (acc :initform 0 :accessor acc
	:documentation "when acc reaches ACC-MAX, clear and spawn star")
   (acc-max :initarg :acc-max :initform planet-default-acc-max
	    :accessor acc-max)
   (player :initarg :player :initform nil :accessor player
	   :documentation "which side the star belongs to (nil for\
noside)")
   (last-attacker :initform nil :accessor last-attacker :documentation
		  "this is the last attacker that leads to the destruction
of the planet. ")
   (life :accessor life :documentation
	 "life of the planet. if falls to zero, it converts to
nobody. Currently, by defaul, planet has life the same amount as the
size")))

(defclass player ()
  ((planet-color :initarg :planet-color
		 :initform (error "we must have planet color!")
		 :accessor planet-color)
   (star-color :initarg :star-color
	       :initform (error "must have star color!")
	       :accessor star-color)
   (use-AI :initarg :use-AI :initform nil :accessor use-AI
	   :documentation "whether this player is using AI. "))
  (:documentation "The player class, defining team in the game"))
