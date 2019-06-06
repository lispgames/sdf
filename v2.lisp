(in-package #:sdf)

(declaim (inline v2 vx vy
                    v2- v2+ v2h* v2. v2x v2dist v2scale v2mag v2n v2rx))
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
  (+ (* (vx a) (vx b))
     (* (vy a) (vy b))))

(defun v2dist (a b)
  (declare (type v2 a b))
  (let* ((d (v2- a b))
         (l (v2. d d)))
    (if (minusp l)
        (error "complex length?")
        (sqrt l))))

(defun v2mag (v)
  (declare (type v2 v))
  (let ((l (v2. v v)))
    (if (minusp l)
        (error "complex mag?")
        (sqrt l))))

(defun v2scale (v f)
  (declare (type v2 v))
  (v2 (* f (vx v)) (* f (vy v))))


(defun v2n (v)
  (declare (type v2 v))
  (let ((l (v2mag v)))
    (assert (not (zerop l)))
    (v2scale v (/ l))))

(defun v2rx (v)
  (declare (type v2 v))
  (v2 (- (vy v)) (vx v)))
