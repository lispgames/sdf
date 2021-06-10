(in-package #:sdf/base)

(declaim (inline v2 vx vy (setf vx) (setf vy)
                 v2- v2+ v2h* v2. v2x v2dist v2scale v2mag v2n v2rx))
(defun vx (v) (aref v 0))
(defun vy (v) (aref v 1))

(defun (setf vx) (n v) (setf (aref v 0) n))
(defun (setf vy) (n v) (setf (aref v 1) n))
(deftype v2 () '(simple-array double-float (2)))
(deftype rv2 () '(simple-array real (2)))


(declaim (inline d))
(defun d (x)
  (coerce x 'double-float))

(defmacro cd ((&rest vars) &body body)
  `(let (,@ (loop for v in vars collect `(,v (d ,v))))
     ,@body))


(defun v2 (x y)
  (make-array 2 :element-type 'double-float
                :initial-contents (list (d x) (d y))))

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
  ;; Z value of cross product of vectors A,B in XY plane
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

(defun v2lerp (a b f)
  (v2+ (v2scale a (- 1.0 f))
       (v2scale b f)))


(declaim (inline rv2 rv2- rv2+ rv2h* rv2. rv2x
                 rv2dist rv2scale rv2mag rv2n rv2rx))

(defun rv2 (x y)
  (make-array 2 :element-type 'real
                :initial-contents (list x y)))

(defun rv2d (v)
  (declare (type rv2 v))
  (v2 (vx v) (vy v)))

(defun rv2- (a b)
  (declare (type rv2 a b))
  (map 'rv2 #'- a b))

(defun rv2+ (a b)
  (declare (type rv2 a b))
  (map 'rv2 #'+ a b))

(defun rv2h* (a b)
  (declare (type rv2 a b))
  (map 'rv2 #'* a b))

(defun rv2x (a b)
  (declare (type rv2 a b))
  (- (* (vx a) (vy b))
     (* (vy a) (vx b))))

(defun rv2. (a b)
  (declare (type rv2 a b))
  (+ (* (vx a) (vx b))
     (* (vy a) (vy b))))

(defun rv2dist (a b)
  (declare (type rv2 a b))
  (let* ((d (rv2- a b))
         (l (rv2. d d)))
    (if (minusp l)
        (error "complex length?")
        (sqrt l))))

(defun rv2mag (v)
  (declare (type rv2 v))
  (let ((l (rv2. v v)))
    (if (minusp l)
        (error "complex mag?")
        (sqrt l))))

(defun rv2scale (v f)
  (declare (type rv2 v))
  (rv2 (* f (vx v)) (* f (vy v))))


(defun rv2n (v)
  (declare (type rv2 v))
  (let ((l (rv2mag v)))
    (assert (not (zerop l)))
    (rv2scale v (/ l))))

(defun rv2rx (v)
  (declare (type rv2 v))
  (rv2 (- (vy v)) (vx v)))

(defun rv2lerp (a b f)
  (rv2+ (rv2scale a (- 1.0 f))
       (rv2scale b f)))
