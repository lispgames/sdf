(in-package #:sdf/base)

(declaim (inline %make-aabb aabb-p1 aabb-p2 make-aabb
                 aabb-x1 aabb-y1 aabb-x2 aabb-y2))
(defstruct (aabb (:constructor %make-aabb (p1 p2)))
  (p1 (v2 0d0 0d0) :type v2)
  (p2 (v2 0d0 0d0) :type v2))

(defun aabb-x1 (b) (vx (aabb-p1 b)))
(defun aabb-y1 (b) (vy (aabb-p1 b)))
(defun aabb-x2 (b) (vx (aabb-p2 b)))
(defun aabb-y2 (b) (vy (aabb-p2 b)))

(defun (setf aabb-x1) (n b) (setf (vx (aabb-p1 b)) n))
(defun (setf aabb-y1) (n b) (setf (vy (aabb-p1 b)) n))
(defun (setf aabb-x2) (n b) (setf (vx (aabb-p2 b)) n))
(defun (setf aabb-y2) (n b) (setf (vy (aabb-p2 b)) n))

(defun make-aabb (&optional
                    (x1 most-positive-double-float)
                    (y1 most-positive-double-float)
                    (x2 most-negative-double-float)
                    (y2 most-negative-double-float))
  (cd (x1 y1 x2 y2)
    (%make-aabb (v2 x1 y1) (v2 x2 y2))))

(defun update-aabb (bb x y)
  (cd (x y)
    (setf (aabb-x1 bb) (min x (aabb-x1 bb)))
    (setf (aabb-x2 bb) (max x (aabb-x2 bb)))
    (setf (aabb-y1 bb) (min y (aabb-y1 bb)))
    (setf (aabb-y2 bb) (max y (aabb-y2 bb)))))

(defun aabb-wx (bb)
  (- (aabb-x2 bb) (aabb-x1 bb)))
(defun aabb-wy (bb)
  (- (aabb-y2 bb) (aabb-y1 bb)))


(declaim (inline %make-point make-point p-v p-x p-y))

(defstruct (point (:conc-name p-)
                  (:constructor %make-point (v)))
  (v (v2 0d0 0d0) :type v2))

(defun p-x (p) (vx (p-v p)))
(defun p-y (p) (vy (p-v p)))

;; not sure if points should actually be mutable or not?
#++
(defun (setf p-x) (n p) (setf (vx (p-v p)) n))
#++
(defun (setf p-y) (n p) (setf (vy (p-v p)) n))

(defmacro with-point ((p x y) &body body)
  `(symbol-macrolet ((,x (p-x ,p))
                     (,y (p-y ,p)))
     ,@body))

(defun make-point (&optional (x 0d0) (y 0d0))
  (%make-point (v2 x y)))

(defun point= (p1 p2)
  (or (eq p1 p2)
      (and (= (p-x p1) (p-x p2))
           (= (p-y p1) (p-y p2)))))

(declaim (inline %make-segment s-p1 s-p2 s-x1 s-y1 s-x2 s-y2))
(defstruct (segment (:conc-name s-)
                    (:constructor %make-segment (p1 p2)))
  (p1 (make-point) :type point)
  (p2 (make-point) :type point))

(defun s-x1 (b) (p-x (s-p1 b)))
(defun s-y1 (b) (p-y (s-p1 b)))
(defun s-x2 (b) (p-x (s-p2 b)))
(defun s-y2 (b) (p-y (s-p2 b)))

(defun make-segment/p (p1 p2)
  (%make-segment p1 p2))

(defun make-segment (x1 y1 x2 y2)
  (%make-segment (make-point x1 y1) (make-point x2 y2)))

(defstruct (bezier2
            (:conc-name b2-)
            (:constructor %make-bezier2 (p1 c1 p2)))
  (p1 (make-point) :type point)
  (c1 (v2 0d0 0d0) :type v2)
  (p2 (make-point) :type point))

(defun b2-x1 (b) (p-x (b2-p1 b)))
(defun b2-y1 (b) (p-y (b2-p1 b)))
(defun b2-xc (b) (vx (b2-c1 b)))
(defun b2-yc (b) (vy (b2-c1 b)))
(defun b2-x2 (b) (p-x (b2-p2 b)))
(defun b2-y2 (b) (p-y (b2-p2 b)))




(declaim (inline dist/v2-point dist/v2-point/sf))
(defun dist/v2-point (v p)
  (declare (optimize speed))
  (check-type p point)
  (check-type v v2)
  (v2dist v (p-v p)))

(defun dist/v2-point/sf (v p)
  (declare (optimize speed))
  (check-type p point)
  (check-type v v2)
  (coerce (v2dist v (p-v p)) 'single-float))


(defun dist/v2-line/sf (v s)
  (declare (optimize speed))
  (check-type s segment)
  (check-type v v2)
  (let* ((p0 (p-v (s-p1 s)))
         (n (v2- (p-v (s-p2 s)) p0))
         (l (v2. n n))
         (tt (/ (v2. (v2- v p0) n)
                l))
         (pp (v2+ p0 (v2scale n tt)))
         (sig (v2x n (v2- pp v))))
    (coerce
     (* (if (plusp sig) 1 -1)
        (v2dist v pp))
     'single-float)))

(defun dist/v2-segment/sf (v s)
  (declare (optimize speed))
  (check-type s segment)
  (check-type v v2)
  (let* ((p0 (p-v (s-p1 s)))
         (n (v2- (p-v (s-p2 s)) p0))
         (l (v2. n n))
         (tt (/ (v2. (v2- v p0) n)
                l))
         (pp (v2+ p0 (v2scale n tt))))
    (when (<= 0 tt 1)
      (coerce
       (v2dist v pp)
       'single-float))))

(defun solve-cubic (io)
  (declare (type (simple-array double-float (4)) io)
           (optimize speed))
  (let* ((a (aref io 0))
         (b (aref io 1))
         (c (aref io 2))
         (d (aref io 3))
         (bc (* b c))
         (ad (* a d))
         (abc (* a bc))
         (abcd (* ad bc))
         (b3 (expt b 3))
         (det (+ (* 18 abcd)
                 (* -4 b3 d)
                 (* bc bc)
                 (* -4 a (expt c 3))
                 (* -27 ad ad)))
         (det0 (+ (expt b 2)
                  (* -3 a c))))
    (cond
      ((and (zerop det) (zerop det0))
       (let ((r io))
         (setf (aref r 0)
               (if (zerop a)
                   0d0
                   (/ b (* -3 a))))
         r
         1))
      ((zerop det)
       (let ((r io))
         (setf (aref r 0) (/ (- (* 9 ad) bc)
                             (* 2 det0))
               (aref r 1) (/ (- (* 4 abc) (* 9 a ad) b3)
                             (* a det0)))
         r
         2))
      (t
       (let* ((det1 (+ (* 2 b3) (* -9 abc) (* 27 a ad)))
              (pm (sqrt (* -27 a a det)))
              (cc (expt
                   (/ (if (= det1 pm)
                          (+ det1 pm)
                          (- det1 pm))
                      2)
                   1/3))
              (xi (+ -1/2 (/ #c(0 #.(sqrt 3)) 2)))
              (c 0))
         (loop for k upto 2
               for r = (* (/ (* -3 a))
                          (+ b
                             (* (expt xi k) cc)
                             (/ det0 (* (expt xi k) cc))))
               when (and (< (abs (imagpart r)) 0.001)
                         (< 0 (realpart r) 1))
                 do (setf (aref io c) (realpart r))
                    (incf c))
         c)))))

(defun dist/v2-bezier2/sf (p c)
  (declare (optimize speed))
  (check-type c bezier2)
  (check-type p v2)
  (let* ((p0 (p-v (b2-p1 c)))
         (p1 (b2-c1 c))
         (p2 (p-v (b2-p2 c)))
         (d (v2- p p0))
         (d1 (v2- p1 p0))
         (d2 (v2- p2 (v2- (v2scale p1 2) p0)))
         (s1 (v2. d2 d2))
         (s2 (* 3 (v2. d1 d2)))
         (s3 (* 2 (v2. d1 d1)))
         (abcd (make-array 4 :element-type 'double-float
                             :initial-contents
                             (list s1 s2 (- s3 (v2. d2 d))
                                   (* -1 (v2. d1 d)))))
         (roots (solve-cubic abcd)))
    (declare (type (and unsigned-byte fixnum) roots)
             (dynamic-extent abcd))
    (flet ((eval-curve (at)
             (v2+ (v2+ (v2scale d2 (* at at))
                       (v2scale d1 (* 2 at)))
                  p0)))
      (declare (inline eval-curve))
      (loop for r double-float across abcd
            repeat roots
            for b = (eval-curve r)
            for d double-float = (v2dist p b)
            minimize d into dd double-float
            finally (return (coerce dd 'single-float))))))
