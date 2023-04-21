(in-package #:sdf/base)

(declaim (inline v2 vx vy (setf vx) (setf vy)
                 v2- v2+ v2h* v2. v2x v2dist v2scale v2mag v2n v2rx v2lerp))
(defun vx (v) (aref v 0))
(defun vy (v) (aref v 1))

(defun (setf vx) (n v) (setf (aref v 0) n))
(defun (setf vy) (n v) (setf (aref v 1) n))
(deftype v2 () '(simple-array double-float (2)))
(deftype rv2 () '(simple-array real (2)))

;; v2 with extra elements for storing T values, to be returned from
;; intersection routines
(deftype v4 () '(simple-array double-float (4)))

(declaim (inline d))
(defun d (x)
  (coerce x 'double-float))

(defmacro cd ((&rest vars) &body body)
  `(let (,@ (loop for v in vars collect `(,v (d ,v))))
     ,@body))


(defun v2 (x y)
  (make-array 2 :element-type 'double-float
                :initial-contents (list (d x) (d y))))

(defun v2copy (a)
  (make-array 2 :element-type 'double-float
                :initial-contents (list (vx a) (vy a))))

(declaim (inline v4 v2->4))
(defun v4 (x y at1 at2)
  (make-array 4 :element-type 'double-float
                :initial-contents (list (d x) (d y) (d at1) (d at2))))
(defun v2->4 (v2 at1 at2)
  (declare (type v2 v2))
  (v4 (vx v2) (vy v2) at1 at2))


(defun v2= (a b)
  ;; strict =
  (or (eql a b)
      (and (= (vx a) (vx b))
           (= (vy a) (vy b)))))

(defun ~ (a b epsilon)
  (or (= a b)
      (< (abs (- a b)) epsilon)))

(defun v2~ (a b epsilon)
  ;; approximately equal
  (or (eql a b)
      (and (< (abs (- (vx a) (vx b))) epsilon)
           (< (abs (- (vy a) (vy b))) epsilon))))

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
  (let ((1-f (- 1 f)))
    (flet ((lerp (a1 b1)
             ;; version with 2 multiplies is supposedly more stable
             ;; than (+ a (* f (- b a))), but gets wrong answer
             ;; sometimes if a=b, so handle that explicitly
             (if (= a1 b1)
                 a1
                 (+ (* a1 1-f) (* b1 f)))))
      (declare (inline lerp))
      (v2 (lerp (vx a) (vx b))
          (lerp (vy a) (vy b))))))

(declaim (inline rv2 rv2- rv2+ rv2h* rv2. rv2x
                 rv2dist rv2scale rv2mag rv2n rv2rx))

(defun rv2 (x y)
  ;; make sure we don't accidentally use single-floats while assuming
  ;; they are rationals
  (when (typep x 'single-float)
    (setf x (coerce x 'double-float)))
  (when (typep y 'single-float)
    (setf y (coerce y 'double-float)))
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

;; 3x3 matrix and some simple transforms, mostly for debugging/visualization
;; stored as column-major.
(deftype m3x3 () `(simple-array double-float (9)))

(declaim (inline %m3 m3i m3c rotation translation align mv* m*))
;; args in row-major order so it looks write when written out
(defun %m3 (m11 m12 m13 m21 m22 m23 m31 m32 m33)
  (make-array 9 :element-type 'double-float
                :initial-contents (list (d m11) (d m21) (d m31)
                                        (d m12) (d m22) (d m32)
                                        (d m13) (d m23) (d m33))))
(defun m3i ()
  (%m3 1d0 0d0 0d0
       0d0 1d0 0d0
       0d0 0d0 1d0))

(defun m3c (m)
  (declare (type m3x3 m))
  (copy-seq m))


(defun rotation (radians)
  (let* ((r (d radians))
         (c (cos r))
         (s (sin r)))
    (%m3 c (- s) 0d0
         s c 0d0
         0d0 0d0 1d0)))

(defun translation (dx dy)
  (%m3 1d0 0d0 (d dx)
       0d0 1d0 (d dy)
       0d0 0d0 1d0))

(defun scale (sx &optional (sy sx))
  (%m3 (d sx) 0d0 0d0
       0d0 (d sy) 0d0
       0d0 0d0 1d0))

(defun %m* (a b into)
  (macrolet ((ij (i j)
               (labels ((i (i j)
                          (+ i (* j 3)))
                        (a (i j)
                          `(aref a ,(i i j)))
                        (b (i j)
                          `(aref b ,(i i j))))
                 `(+ (* ,(a i 0) ,(b 0 j))
                     (* ,(a i 1) ,(b 1 j))
                     (* ,(a i 2) ,(b 2 j))))))
    (psetf (aref into 0) (ij 0 0)
           (aref into 1) (ij 1 0)
           (aref into 2) (ij 2 0)

           (aref into 3) (ij 0 1)
           (aref into 4) (ij 1 1)
           (aref into 5) (ij 2 1)

           (aref into 6) (ij 0 2)
           (aref into 7) (ij 1 2)
           (aref into 8) (ij 2 2))))

(defun m* (&rest matrices)
  (let ((r (if matrices
               (m3c (car matrices))
               (m3i))))
    (loop for m in (cdr matrices)
          do (%m* r m r))
    r))

(defun mv* (m v)
  (declare (type m3x3 m) (type v2 v))
  (v2 (+ (* (aref m 0) (aref v 0))
         (* (aref m 3) (aref v 1))
         (aref m 6))
      (+ (* (aref m 1) (aref v 0))
         (* (aref m 4) (aref v 1))
         (aref m 7))))

(defun align (p1 p2)
  (let* ((d (v2- p2 p1))
         (a (atan (vy d) (vx d))))
    (m* (rotation (- a))
        (translation (- (vx p1)) (- (vy p1))))))
#++
(defun transform-b2 (m b)
  (b2 (mv* m (b2-p0 b))
      (mv* m (b2-p1 b))
      (mv* m (b2-p2 b))))
