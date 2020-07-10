(defclass point-2d ()
  ((x
    :initarg :x
    :initform 0
    :accessor x)
   (y
    :initarg :y
    :initform 0
    :accessor y)))

(defclass point-3d ()
  ((x
    :initarg :x
    :initform 0
    :accessor x)
   (y
    :initarg :y
    :initform 0
    :accessor y)
   (z
    :initarg :z
    :initform 0
    :accessor z)))

(defgeneric distance (p1 p2)
  (:documentation "Learning how to use generic methods by doing something dumb with the distance formula and 2d/3d points"))

(defmethod distance ((p1 point-2d) (p2 point-2d))
  ;;Distance method for 2D points
  (sqrt (+ (expt (- (slot-value p2 'x) (slot-value p1 'x)) 2)
	   (expt (- (slot-value p2 'y) (slot-value p1 'y)) 2))))

(defmethod distance ((p1 point-3d) (p2 point-3d))
  ;;Distance method for 3D points
  (sqrt (+ (expt (- (slot-value p2 'x) (slot-value p1 'x)) 2)
	   (expt (- (slot-value p2 'y) (slot-value p1 'y)) 2)
	   (expt (- (slot-value p2 'z) (slot-value p1 'z)) 2))))

(defgeneric slope (p1 p2)
  (:documentation "Slope method for 2d and 3d points.  Yes, I know this could be done with a generic length vector but I'm learning the defgeneric method"))

(defmethod slope ((p1 point-2d) (p2 point-2d))
  ;; Rise over Run
           (/ (- (slot-value p2 'y) (slot-value p1 'y))
              (- (slot-value p2 'x) (slot-value p1 'y))))

(defmethod slope ((p1 point-3d) (p2 point-3d))
  ;;This one is interesting because I need to return a list of slopes on each plane (xy, xz, yz)
  (let ((d (distance p1 p2)))
    (list (/ (- (slot-value p2 'z) (slot-value p1 'z)) d)
	  (/ (- (slot-value p2 'y) (slot-value p1 'y)) d)
	  (/ (- (slot-value p2 'x) (slot-value p1 'x)) d))))

(defgeneric midpoint (p1 p2)
  (:documentation "Let's do it again for midpoints of a line defined by p1 and p2"))

(defmethod midpoint ((p1 point-2d) (p2 point-2d))
  ;; Find the pointpoint of a line in 2 dimensions
  (make-point (mean (list (slot-value p2 'x) (slot-value p1 'x)))
	      (mean (list (slot-value p2 'y) (slot-value p1 'y)))))

(defmethod midpoint ((p1 point-3d) (p2 point-3d))
  ;; Find the midpoint of a line in 3 dimensions
  (make-point (mean (list (slot-value p2 'x) (slot-value p1 'x)))
	      (mean (list (slot-value p2 'y) (slot-value p1 'y)))
	      (mean (list (slot-value p2 'z) (slot-value p1 'z)))))

(defun mean (values)
  (/ (reduce #'+ values) (length values)))

(defun make-point (x y &optional (z nil z-p))
  ;; This function turned out really cool.  Given 3 parameters make a 3d point object, otherwise, make a 2d point.
  (cond
    (z-p (make-instance 'point-3d :x x :y y :z z))
    (t (make-instance 'point-2d :x x :y y))))

(defun factors-helper (num x)
           (cond
             ((eq num x) (list num))
           ((factorp num x)
               (append (factors-helper (/ num x) 2)  (list x)))
           (t (factors-helper num (+ x 1)))))

(defun factors (num)
  (factors-helper num 2))


(defun geo-mean (a b)
  (factors (* a b)))

(defun factorp (a b)
  (integerp (/ a b)))
