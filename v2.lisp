(in-package #:sdf)

(declaim (notinline v2 vx vy))
(defun vx (v) (aref v 0))
(defun vy (v) (aref v 1))
(deftype v2 () '(simple-array double-float (2)))

(defun v2 (x y)
  (make-array 2 :element-type 'double-float
                :initial-contents (list (coerce x 'double-float)
                                        (coerce y 'double-float))))

(defun v2- (a b)
  (declare (type v2 a b))
  (map 'v2 #'- a b))

(defun v2+ (a b)
  (declare (type v2 a b))
  (map 'v2 #'+ a b))

(defun v2h* (a b)
  (declare (type v2 a b))
  (map 'v2 #'* a b))

(defun v2x (a b)
  (declare (type v2 a b))
  (- (* (vx a) (vy b))
     (* (vy a) (vx b))))

(defun v2. (a b)
  (declare (type v2 a b))
  (reduce '+ (v2h* a b)))

(defun v2dist (a b)
  (declare (type v2 a b))
  (let ((d (v2- a b)))
    (sqrt (v2. d d))))

(defun v2mag (v)
  (declare (type v2 v))
  (sqrt (v2. v v)))

(defun v2scale (v f)
  (declare (type v2 v))
  (v2 (* f (vx v)) (* f (vy v))))

