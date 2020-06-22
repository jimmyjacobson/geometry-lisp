(defun distance (p1 p2)
           (sqrt (+ (expt (- (getf p2 :x) (getf p1 :x)) 2)
                    (expt (- (getf p2 :y) (getf p1 :y)) 2)
                    )
                 )
           )

(defun slope (p1 p2)
           (/ (- (getf p2 :y) (getf p1 :y))
              (- (getf p2 :x) (getf p1 :x))))

(defun midpoint (p1 p2)
           (make-point (mean (list (getf p2 :x) (getf p1 :x)))
                       (mean (list (getf p2 :y) (getf p1 :y)))))

(defun mean (values)
  (/ (reduce #'+ values) (length values)))

(defun make-point (x y)
  (list :x x :y y))

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

