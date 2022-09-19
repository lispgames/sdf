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



;; version of aabb that preserves type of inputs, so we can calculate
;; things with rationals if desired
(defstruct (raabb (:constructor make-raabb (&optional x1 y1 x2 y2)))
  (x1 nil :type (or null real))
  (y1 nil :type (or null real))
  (x2 nil :type (or null real))
  (y2 nil :type (or null real)))

(defun update-raabb (bb x y)
  (setf (raabb-x1 bb) (if (raabb-x1 bb) (min x (raabb-x1 bb)) x))
  (setf (raabb-y1 bb) (if (raabb-y1 bb) (min y (raabb-y1 bb)) y))
  (setf (raabb-x2 bb) (if (raabb-x2 bb) (max x (raabb-x2 bb)) x))
  (setf (raabb-y2 bb) (if (raabb-y2 bb) (max y (raabb-y2 bb)) y)))

(defun raabb-wx (bb)
  (- (raabb-x2 bb) (raabb-x1 bb)))
(defun raabb-wy (bb)
  (- (raabb-y2 bb) (raabb-y1 bb)))



(declaim (inline %make-point make-point p-dv p-x p-y p-rx p-ry p-dx p-dy))

(defstruct (point (:conc-name p-)
                  (:constructor %make-point (rv dv)))
  (rv (rv2 0 0) :type rv2)
  (dv (v2 0d0 0d0) :type v2))

(defun p-rx (p) (vx (p-rv p)))
(defun p-ry (p) (vy (p-rv p)))

(defun p-dx (p) (vx (p-dv p)))
(defun p-dy (p) (vy (p-dv p)))

;; not sure if points should actually be mutable or not?
#++
(defun (setf p-x) (n p) (setf (vx (p-v p)) n))
#++
(defun (setf p-y) (n p) (setf (vy (p-v p)) n))

(defmacro with-rpoint ((p x y) &body body)
  `(symbol-macrolet ((,x (p-rx ,p))
                     (,y (p-ry ,p)))
     ,@body))

(defmacro with-dpoint ((p x y) &body body)
  `(symbol-macrolet ((,x (p-dx ,p))
                     (,y (p-dy ,p)))
     ,@body))

(defun make-point (&optional (x 0) (y 0))
  (%make-point (rv2 x y) (v2 x y)))

(defun v2p (v)
  ;; todo: decide if it is safe to reuse input point
  (%make-point (rv2 (vx v) (vy v)) (v2 (vx v) (vy v))))

(declaim (inline point=))
(defun point= (p1 p2)
  (or (eq p1 p2)
      (and (= (p-dx p1) (p-dx p2))
           (= (p-dy p1) (p-dy p2)))))

(declaim (inline %make-segment s-p1 s-p2
                 s-rx1 s-ry1 s-rx2 s-ry2
                 s-dx1 s-dy1 s-dx2 s-dy2))
(defstruct (segment (:conc-name s-)
                    (:constructor %make-segment (p1 p2)))
  (p1 (make-point) :type point)
  (p2 (make-point) :type point))

(defun s-rx1 (b) (p-rx (s-p1 b)))
(defun s-ry1 (b) (p-ry (s-p1 b)))
(defun s-rx2 (b) (p-rx (s-p2 b)))
(defun s-ry2 (b) (p-ry (s-p2 b)))

(defun s-dx1 (b) (p-dx (s-p1 b)))
(defun s-dy1 (b) (p-dy (s-p1 b)))
(defun s-dx2 (b) (p-dx (s-p2 b)))
(defun s-dy2 (b) (p-dy (s-p2 b)))

(defun make-segment/p (p1 p2)
  (%make-segment p1 p2))

(defun make-segment (x1 y1 x2 y2)
  (%make-segment (make-point x1 y1) (make-point x2 y2)))

(defstruct (bezier2
            (:conc-name b2-)
            (:constructor %make-bezier2 (p1 c1 p2)))
  (p1 (make-point) :type point)
  (c1 (make-point) :type point)
  (p2 (make-point) :type point))

(defun make-bezier2 (x1 y1 xc yc x2 y2)
  (%make-bezier2 (make-point x1 y1)
                 (make-point xc yc)
                 (make-point x2 y2)))

(defun make-bezier2/v2 (p1 pc p2)
  (%make-bezier2 (v2p p1) (v2p pc) (v2p p2)))

(defun b2-rx1 (b) (p-rx (b2-p1 b)))
(defun b2-ry1 (b) (p-ry (b2-p1 b)))
(defun b2-rxc (b) (p-rx (b2-c1 b)))
(defun b2-ryc (b) (p-ry (b2-c1 b)))
(defun b2-rx2 (b) (p-rx (b2-p2 b)))
(defun b2-ry2 (b) (p-ry (b2-p2 b)))

(defun b2-dx1 (b) (p-dx (b2-p1 b)))
(defun b2-dy1 (b) (p-dy (b2-p1 b)))
(defun b2-dxc (b) (p-dx (b2-c1 b)))
(defun b2-dyc (b) (p-dy (b2-c1 b)))
(defun b2-dx2 (b) (p-dx (b2-p2 b)))
(defun b2-dy2 (b) (p-dy (b2-p2 b)))

(declaim (inline b2-dp1 b2-dc1 b2-dp2))
(defun b2-dp1 (b) (p-dv (b2-p1 b)))
(defun b2-dc1 (b) (p-dv (b2-c1 b)))
(defun b2-dp2 (b) (p-dv (b2-p2 b)))


(declaim (inline dist/v2-point dist/v2-point/sf))
(defun dist/v2-point (v p)
  (declare (optimize speed))
  (check-type p point)
  (check-type v v2)
  (v2dist v (p-dv p)))

(defun dist/v2-point/sf (v p)
  (declare (optimize speed))
  (check-type p point)
  (check-type v v2)
  (coerce (v2dist v (p-dv p)) 'single-float))


(declaim (inline dist/v2-line*/sf))
(defun dist/v2-line*/sf (v e1 e2)
  (declare (optimize speed))
  (check-type v v2)
  (check-type e1 v2)
  (check-type e2 v2)
  (let* ((p0 e1)
         (n (v2- e2 p0))
         (l (v2. n n))
         (tt (/ (v2. (v2- v p0) n)
                l))
         (pp (v2+ p0 (v2scale n tt)))
         (sig (v2x n (v2- pp v))))
    (coerce
     (* (if (plusp sig) 1 -1)
        (v2dist v pp))
     'single-float)))

(defun dist/v2-line/sf (v s)
  (declare (optimize speed))
  (check-type s segment)
  (check-type v v2)
  (dist/v2-line*/sf v (p-dv (s-p1 s)) (p-dv (s-p2 s))))

(declaim (inline %dist/v2-segment/sf))

(defun %dist/v2-segment/sf (v s)
  (let* ((p0 (p-dv (s-p1 s)))
         (n (v2- (p-dv (s-p2 s)) p0))
         (l (v2. n n))
         (tt (/ (v2. (v2- v p0) n)
                l))
         (pp (v2+ p0 (v2scale n tt))))
    (values pp tt)))

(defun dist/v2-segment/sf (v s)
  (declare (optimize speed))
  (check-type s segment)
  (check-type v v2)
  (if (point= (s-p1 s) (s-p2 s))
      (dist/v2-point/sf v (s-p1 s))
      (multiple-value-bind (pp tt) (%dist/v2-segment/sf v s)
        (when (< 0 tt 1)
          (coerce
           (v2dist v pp)
           'single-float)))))

(defun dist/v2-segment/sf* (v s)
  ;; calculate signed distance and signed pseudo-distance from segment
  (declare (optimize speed))
  (check-type s segment)
  (check-type v v2)
  (if (point= (s-p1 s) (s-p2 s))
      (error "can't find signed distance from degenerate segment ~s" s)
      (multiple-value-bind (pp tt) (%dist/v2-segment/sf v s)
        (flet ((sf (x) (coerce x 'single-float)))
          (declare (inline sf))
          (let* ((a (sf (v2dist v pp)))
                 (dot (v2x (v2- v pp)
                           (v2- (p-dv (s-p2 s))
                                (p-dv (s-p1 s)))))
                 (sign (if (minusp dot) -1 1)))
            (values
             (cond
               ((<= 0 tt 1)
                (* a sign))
               ((< tt 0)
                (* (sf (v2dist v (p-dv (s-p1 s)))) sign))
               ((> tt 1)
                (* (sf (v2dist v (p-dv (s-p2 s)))) sign)))
             (* a sign)))))))

(defun pdist/v2-segment/sf* (v s)
  ;; calculate signed pseudo-distance from segment
  (declare (optimize speed))
  (check-type s segment)
  (check-type v v2)
  (if (point= (s-p1 s) (s-p2 s))
      (error "can't find signed distance from degenerate segment ~s" s)
      (multiple-value-bind (pp tt) (%dist/v2-segment/sf v s)
        (flet ((sf (x) (coerce x 'single-float)))
          (declare (inline sf))
          (let* ((a (sf (v2dist v pp)))
                 (dot (v2x (v2- v pp)
                           (v2- (p-dv (s-p2 s))
                                (p-dv (s-p1 s)))))
                 (sign (if (minusp dot) -1 1)))
            (values
             (cond
               ((<= 0 tt 1)
                (* a sign))
               ((< tt 0)
                (* (sf (v2dist v (p-dv (s-p1 s)))) sign))
               ((> tt 1)
                (* (sf (v2dist v (p-dv (s-p2 s)))) sign)))
             (* a sign)))))))

(defun dist/v2-*/sf* (v n)
  (etypecase n
    (point (dist/v2-point/sf v n))
    (segment (dist/v2-segment/sf* v n))
    (bezier2 (dist/v2-bezier2/sf* v n))))

(defvar *dump-distance* nil)

(declaim (inline solve-quadratic))
(defun solve-quadratic (a b c)
  ;; http://www.numerical.recipes/ ch 5.6
  (let ((d (- (expt b 2) (* 4 a c))))
    (cond
      ((minusp d)
       (values))
      ((zerop a)     ;; linear, bx+c=0, bx=-c,
       (if (zerop b) ;; constant, no (useful) solutions
           (values)
           (- (/ c b))))
      ((zerop d)
       (/ b (* -2 a)))
      (t
       (let* ((rd (sqrt (coerce d 'double-float)))
              (q (* -1/2 (if (minusp b)
                             (- b rd)
                             (+ b rd)))))
         (declare (type double-float rd q))
         (let ((r1 (/ q a))
               (r2 (/ c q)))
           (if (<= r1 r2)
               (values r1 r2)
               (values r2 r1))))))))

(defun solve-quadratic-cubic (io)
  (multiple-value-bind (a b) (solve-quadratic (aref io 1)
                                              (aref io 2)
                                              (aref io 3))
    (return-from solve-quadratic-cubic
      (cond
        (b
         (setf (aref io 0) a)
         (setf (aref io 1) b)
         2)
        (a
         (setf (aref io 0) a)
         1)
        (t 0)))))


(defun solve-cubic (io)
  ;; http://www.numerical.recipes/ ch 5.6
  (declare (type (simple-array double-float (4)) io)
           (optimize speed))
  ;; a=0, actually a quadratic
  (when (zerop (aref io 0))
    (return-from solve-cubic (solve-quadratic-cubic io)))
  (let* ((a0 (aref io 0))
         (b0 (aref io 1))
         (c0 (aref io 2))
         (d0 (aref io 3))
         (a (/ b0 a0))
         (b (/ c0 a0))
         (c (/ d0 a0))
         (q (/ (- (expt a 2)
                  (* 3 b))
               9))
         (r (/ (+ (* 2 (expt a 3))
                  (* -9 a b)
                  (* 27 c))
               54))
         (r2 (expt r 2))
         (q3 (expt q 3)))
    (declare (type double-float a0 b0 c0 d0 a b c q r r2 q3))
    (cond
      ((< r2 q3)
       ;; r2 is (expt real 2) so >= 0, so q3 is >= 0, so q is >= 0
       (locally (declare (type (double-float 0d0) r2 q3 q))
         (let* ((r/rq3 (/ r (sqrt q3)))
                (th/3 (/ (acos r/rq3)
                         3))
                (a/3 (/ a 3))
                (-2rq (* -2 (sqrt q)))
                (2pi/3 (* pi 2/3)))
           (declare (type (double-float -1d0 1d0) r/rq3))
           (setf (aref io 0) (- (* -2rq (cos th/3)) a/3))
           (setf (aref io 1) (- (* -2rq (cos (+ th/3 2pi/3))) a/3))
           (setf (aref io 2) (- (* -2rq (cos (- th/3 2pi/3))) a/3))
           3)))
      (t
       (let* ((r2-q3 (- r2 q3))
              (aa (* (- (signum r))
                     (expt (+ (abs r)
                              (sqrt r2-q3))
                           #. (coerce 1/3 'double-float))))
              (bb (if (zerop aa) 0d0 (/ q aa))))
         ;; r2>=r3, so r2-r3 is >=0
         (declare (type (double-float 0d0) r2-q3))
         (setf (aref io 0) (+ aa bb (/ a -3)))
         1)))))

(defun dist/v2-bezier2/sf (p c)
  (declare (optimize speed))
  (check-type c bezier2)
  (check-type p v2)
  (let* ((p0 (p-dv (b2-p1 c)))
         (p1 (p-dv (b2-c1 c)))
         (p2 (p-dv (b2-p2 c)))
         (d (v2- p0 p))
         (d1 (v2- p1 p0))
         ;;(d2 (v2- p2 (v2- (v2scale p1 2) p0)))
         (d2 (v2+ p0 (v2+ (v2scale p1 -2) p2)))
         ;; s1 = a = (p0 - 2p1 + p2) . (p0 - 2p1 + p2) = d2 . d2
         (s1 (v2. d2 d2))
         ;; s2 = b = 3 ((p0 - 2p1 + p2) . (p1 - p0)) = 3(d2 . d1)
         (s2 (* 3 (v2. d1 d2)))
         ;; c = ((p0 - 2p1 + p2) . (p0 - p)) + 2((p1-p0).(p1-p0)))
         ;;   = 3(d2 . d) + 2(d1 . d1)
         (s3 (* 2 (v2. d1 d1)))
         ;; d = (p0-p).(p0-p) = d.d
         (abcd (make-array 4 :element-type 'double-float
                             :initial-contents
                             (list s1
                                   s2
                                   (+ s3 (v2. d2 d))
                                   (v2. d1 d))))
         (roots (solve-cubic abcd)))
    (declare (type (and unsigned-byte fixnum) roots)
             (dynamic-extent abcd))
    (when *dump-distance*
      (format t "roots = ~s, abcd=~s~%" roots abcd))

    (flet ((eval-curve (at)
             (v2+ (v2+ (v2scale d2 (* at at))
                       (v2scale d1 (* 2 at)))
                  p0)))
      (declare (inline eval-curve))
      (loop for r double-float across abcd
            repeat roots
            when (< 0 r 1)
              minimize (v2dist p (eval-curve r)) into dd double-float
            else minimize (v2dist p (eval-curve r)) into dd2 double-float
            finally (return (values (coerce dd 'single-float)
                                    (coerce dd2 'single-float)))))))

(defun dist/v2-bezier2/sf* (p c)
  ;; calculate signed pseudo-distance from bez2
  (declare (optimize speed))
  (check-type c bezier2)
  (check-type p v2)
  (let* ((p0 (p-dv (b2-p1 c)))
         (p1 (p-dv (b2-c1 c)))
         (p2 (p-dv (b2-p2 c)))
         (d (v2- p0 p))
         (d1 (v2- p1 p0))
         ;;(d2 (v2- p2 (v2- (v2scale p1 2) p0)))
         (d2 (v2+ p0 (v2+ (v2scale p1 -2) p2)))
         ;; s1 = a = (p0 - 2p1 + p2) . (p0 - 2p1 + p2) = d2 . d2
         (s1 (v2. d2 d2))
         ;; s2 = b = 3 ((p0 - 2p1 + p2) . (p1 - p0)) = 3(d2 . d1)
         (s2 (* 3 (v2. d1 d2)))
         ;; c = ((p0 - 2p1 + p2) . (p0 - p)) + 2((p1-p0).(p1-p0)))
         ;;   = 3(d2 . d) + 2(d1 . d1)
         (s3 (* 2 (v2. d1 d1)))
         ;; d = (p0-p).(p0-p) = d.d
         (abcd (make-array 4 :element-type 'double-float
                             :initial-contents
                             (list s1
                                   s2
                                   (+ s3 (v2. d2 d))
                                   (v2. d1 d))))
         (roots (solve-cubic abcd))
         (rmin nil)
         (pmin nil)
         (dmin (coerce most-positive-single-float 'double-float)))
    (declare (type (and unsigned-byte fixnum) roots)
             (dynamic-extent abcd)
             (type (or null fixnum) rmin)
             (double-float dmin))
    (when *dump-distance*
      (format t "roots = ~s, abcd=~s~%" roots abcd))

    (flet ((eval-curve (at)
             (v2+ (v2+ (v2scale d2 (* at at))
                       (v2scale d1 (* 2 at)))
                  p0)))
      (declare (inline eval-curve))
      (loop for r double-float across abcd
            for i fixnum from 0
            repeat roots
            when (<= 0 r 1)
              do (let* ((e (eval-curve r))
                        (d (v2dist p e)))
                   (when (<= d dmin)
                     (setf dmin d
                           pmin e
                           rmin i)))
            when (< r 0)
              do (let ((d (v2dist p (p-dv (b2-p1 c)))))
                   (when (<= d dmin)
                     (setf dmin d
                           rmin i)))
            when (> r 1)
              do (let ((d (v2dist p (p-dv (b2-p2 c)))))
                   (when (<= d dmin)
                     (setf dmin d
                           rmin i)))))
    (when rmin
      (flet ((sf (x) (coerce x 'single-float))
             (ld (v1 v2)
               (dist/v2-line*/sf p v1 v2)))
        (declare (inline sf))
        (let ((a (sf dmin))
              (rmin (aref abcd rmin)))
          (cond
            ((<= 0 rmin 1)
             (let* ((tan (v2- (v2lerp (p-dv (b2-c1 c))
                                      (p-dv (b2-p2 c))
                                      rmin)
                              (v2lerp (p-dv (b2-p1 c))
                                      (p-dv (b2-c1 c))
                                      rmin)))
                    (dir (v2x tan (v2- p pmin)))
                    (s (if (minusp dir) -1 1)))
               (- (* a s))))
            ((< rmin 0)
             (ld (p-dv (b2-p1 c)) (p-dv (b2-c1 c))))
            ((> rmin 1)
             (ld (p-dv (b2-c1 c)) (p-dv (b2-p2 c))))))))))



(declaim (inline eval-at/s/fast eval-at/b2/fast))

(defun eval-at/s/fast (s at)
  (let* ((p0 (p-dv (s-p1 s)))
         (p1 (p-dv (s-p2 s))))
    (v2lerp p0 p1 at)))

(defun eval-at/b2 (b at)
  (let* ((p0 (p-rv (b2-p1 b)))
         (p1 (p-rv (b2-c1 b)))
         (p2 (p-rv (b2-p2 b)))
         (d1 (rv2- p1 p0))
         (d2 (rv2+ p0 (rv2+ (rv2scale p1 -2) p2))))
    (rv2+ (rv2+ (rv2scale d2 (* at at))
                (rv2scale d1 (* 2 at)))
          p0)))


(defun eval-at/b2/fast (b at)
  (cond
    ((zerop at) (p-dv (b2-p1 b)))
    ((= at 1) (p-dv (b2-p2 b)))
    (t (let* ((p0 (p-dv (b2-p1 b)))
              (p1 (p-dv (b2-c1 b)))
              (p2 (p-dv (b2-p2 b)))
              (d1 (v2- p1 p0))
              (d2 (v2+ p0 (v2+ (v2scale p1 -2) p2))))
         (v2+ (v2+ (v2scale d2 (* at at))
                   (v2scale d1 (* 2 at)))
              p0)))))

(defun eval-at (x at)
  (etypecase x
    (segment (eval-at/s/fast x at))
    (bezier2 (eval-at/b2/fast x at))))

(defvar *assert-range* t)
;; clip a curve to specified range
(defun trim-b2 (b2 t1 t2)
  (assert (< t1 t2))
  (when *assert-range*
    (assert (<= 0 t1))
    (assert (<= t2 1)))
  (let ((p0 (b2-dp1 b2))
        (p1 (b2-dc1 b2))
        (p2 (b2-dp2 b2)))
    (cond
      ((= t1 0)
       (make-bezier2/v2 p0 (v2lerp p0 p1 t2) (eval-at/b2/fast b2 t2)))
      ((= t2 1)
       (make-bezier2/v2 (eval-at/b2/fast b2 t1) (v2lerp p1 p2 t1) p2))
      (t
       (let ((a (v2lerp p0 p1 t1))
             (b (v2lerp p1 p2 t1)))
         (make-bezier2/v2 (v2lerp a b t1) (v2lerp a b t2)
                          (eval-at/b2/fast b2 t2)))))))

(defun %%trim-b2-p1 (b2 t1 t2)
  ;; just calculate the off-curve control point for when we know the
  ;; others due to sharing with other curves
  (let* ((p0 (p-dv (b2-p1 b2)))
         (p1 (p-dv (b2-c1 b2)))
         (p2 (p-dv (b2-p2 b2))))
    (cond
      ((or (and (= t1 0) (= t2 1))
           (and (= t1 1) (= t2 0)))
       p1)
      ((= t1 0) (v2lerp p0 p1 t2))
      ((= t2 1) (v2lerp p1 p2 t1))
      (t (v2lerp (v2lerp p0 p1 t1) (v2lerp p1 p2 t1) t2)))))

(declaim (inline %b2-find-t))
(defun %b2-find-t (x1 xc x2 x)
;;; calculate T value(s) for specified X for 1d quadratic bezier X1,XC,X2
  ;; x = (1-t)²x1+2t(1-t)xc+t²x2
  ;;   = (x1-2xc+x2)t²+(-2x1+2xc)t+x1
  (let ((a (+ x1 (* -2 xc) x2))
        (eps (* 32 double-float-epsilon
                (max (abs x1) (abs x2) (abs xc)))))
    (if (zerop a)
        ;; linear, solve from 2 points
        (cond
          ((/= x1 x2)
           (/ (- x x1) (- x2 x1)))
          (t (error "degenerate bezier?")))
        ;; do rest of quadratic formula
        (let* ((b (* 2 (- xc x1)))
               (c (- x1 x))
               (d (- (expt b 2) (* 4 a c))))
          (cond
            ((zerop d)
             (/ (- b) (* 2 a)))
            ((< d eps)
             ;; we sometimes calculate the extreme value of function,
             ;; then try to map back to T with this, but miss due to
             ;; FP error, so check that explicitly. and since we are
             ;; near a tangent, small changes in Y can lead to fairly
             ;; large changes in calculated T, so clamp back to
             ;; extreme on either side.
;;;
             ;; (1-t)x1+txc=(1-t)xc+tx2
             ;; x1-tx1+txc=xc-txc+tx2
             ;; x1-tx1+txc-xc+txc-tx2=0
             ;; x1-xc=tx1-2txc+tx2
             ;; t=(x1-xc)/(x1-2xc+x2)
             (let* ((at (/ (- x1 xc) a))
                    (y (+ (* (- 1 at) x1) (* at xc)))
                    (y2 (+ (* (- 1 at) xc) (* at x2))))
               (if (or (= x y)
                       (and (floatp x)
                            (or (< (abs (- (float y x) x)) eps)
                                (< (abs (- (float y2 x) x)) eps))))
                   (float at x)
                   nil)))
            (t
             (flet ((r1 (-b r 2a 2c)
                      (if (< -b 0)
                          (/ (- -b r) 2a)
                          (/ 2c (+ -b r))))
                    (r2 (-b r 2a 2c)
                      (if (< -b 0)
                          (/ 2c (- -b r))
                          (/ (+ -b r) 2a))))
               (let ((2a (* 2 a))
                     (2c (* 2 c))
                     (-b (- b))
                     (r (sqrt (coerce d 'double-float))))
                 (values (r1 -b r 2a 2c)
                         (r2 -b r 2a 2c))))))))))


(defun intersect-segment-segment (a b)
  (declare (optimize speed)
           (type segment a b))
  (let* ((x1 (s-dx1 a))
         (y1 (s-dy1 a))
         (x2 (s-dx2 a))
         (y2 (s-dy2 a))
         (x3 (s-dx1 b))
         (y3 (s-dy1 b))
         (x4 (s-dx2 b))
         (y4 (s-dy2 b))
         (x1-x2 (- x1 x2))
         (x3-x4 (- x3 x4))
         (y1-y2 (- y1 y2))
         (y3-y4 (- y3 y4))
         (d (- (* x1-x2 y3-y4)
               (* y1-y2 x3-x4)))
         (x1y2 (* x1 y2))
         (y1x2 (* y1 x2))
         (x1y2-y1x2 (- x1y2 y1x2))
         (x3y4 (* x3 y4))
         (y3x4 (* y3 x4))
         (x3y4-y3x4 (- x3y4 y3x4)))
    (declare (double-float x1 y1 x2 y2 x3 y3 x4 y4))
    (unless (zerop d)
      (let ((x (/ (- (* x1y2-y1x2 x3-x4)
                     (* x1-x2 x3y4-y3x4))
                  d))
            (y (/ (- (* x1y2-y1x2 y3-y4)
                     (* y1-y2 x3y4-y3x4))
                  d)))
        (v2 x y)))))

(defun intersect-segment-bezier2 (a b)
  (break "todo"))

(defun intersect-bezier2-bezier2 (a b)
  (break "todo"))

(defun intersect (a b)
  (flet ((segment (a b)
           (etypecase b
             (segment
              (intersect-segment-segment a b))
             (bezier2
              (intersect-segment-bezier2 a b))))
         (bezier2 (a b)
           (etypecase b
             (segment
              (intersect-segment-bezier2 b a))
             (bezier2
              (intersect-bezier2-bezier2 a b)))))
    (etypecase a
      (segment (segment a b))
      (bezier2 (bezier2 a b)))))


(defun intersect-segment-segment/range (a b at1 at2 bt1 bt2)
  (declare (optimize speed)
           (type segment a b)
           (double-float at1 at2 bt1 bt2))
  (let* ((x1 (s-dx1 a))
         (y1 (s-dy1 a))
         (x2 (s-dx2 a))
         (y2 (s-dy2 a))
         (x3 (s-dx1 b))
         (y3 (s-dy1 b))
         (x4 (s-dx2 b))
         (y4 (s-dy2 b))
         (x1-x2 (- x1 x2))
         (x3-x4 (- x3 x4))
         (y1-y2 (- y1 y2))
         (y3-y4 (- y3 y4))
         (d (- (* x1-x2 y3-y4)
               (* y1-y2 x3-x4)))
         (x1y2 (* x1 y2))
         (y1x2 (* y1 x2))
         (x1y2-y1x2 (- x1y2 y1x2))
         (x3y4 (* x3 y4))
         (y3x4 (* y3 x4))
         (x3y4-y3x4 (- x3y4 y3x4)))
    (declare (double-float x1 y1 x2 y2 x3 y3 x4 y4))
    (unless (zerop d)
      (let* ((x (/ (- (* x1y2-y1x2 x3-x4)
                      (* x1-x2 x3y4-y3x4))
                   d))
             (y (/ (- (* x1y2-y1x2 y3-y4)
                      (* y1-y2 x3y4-y3x4))
                   d))
             (at (if (< (abs x1-x2) (abs y1-y2))
                     (/ (- y1 y) y1-y2)
                     (/ (- x1 x) x1-x2)))
             (bt (if (< (abs x3-x4) (abs y3-y4))
                     (/ (- y3 y) y3-y4)
                     (/ (- x3 x) x3-x4))))
        (when (and (<= at1 at at2)
                   (<= bt1 bt bt2))
          (v4 x y at bt))))))

(defun intersect-segment-bezier2/range (a b at1 at2 bt1 bt2)
  (declare (optimize speed)
           (type segment a)
           (type bezier2 b)
           (double-float at1 at2 bt1 bt2))
  (let* ((a1 (p-dv (s-p1 a)))
         (a2 (p-dv (s-p2 a)))
         (b1 (p-dv (b2-p1 b)))
         (bc (p-dv (b2-c1 b)))
         (b2 (p-dv (b2-p2 b)))
         (da (v2- a2 a1))
         (al (v2mag da))
         ;; b, translated so a1 is origin
         #++(b1 (v2- (p-dv (b2-p1 b)) a1))
         #++(bc (v2- (p-dv (b2-c1 b)) a1))
         #++(b2 (v2- (p-dv (b2-p2 b)) a1))
         ;; rotation matrix
         (m (align a1 a2))
         ;; b, further rotated so a2 is on x axis
         #++(r1 (v2 (v2. m1 b1) (v2. m2 b1)))
         #++(rc (v2 (v2. m1 bc) (v2. m2 bc)))
         #++(r2 (v2 (v2. m1 b2) (v2. m2 b2)))
         (r1 (mv* m b1))
         (rc (mv* m bc))
         (r2 (mv* m b2))
         ;; only need y component of rotated curve? (no, need X to map
         ;; back to T in A)
         ;; (ry1 (v2. m2 b1))
         ;; (ryc (v2. m2 bc))
         ;; (ry2 (v2. m2 b2))
         ;; quadratic in t
         (ta (+ (vy r1) (* -2 (vy rc)) (vy r2)))
         (tb (* 2 (- (vy rc) (vy r1))))
         (tc (vy r1))
         (r (- (expt tb 2) (* 4 ta tc)))
         (eps (* 128 (max (the double-float (%bcs-eps a))
                          (the double-float (%bcs-eps b))))))
    (flet ((in (a b c)
             (if (<= a c) (<= a b c) (<= c b a)))
           (at (a b c)
             (if (= a b c)
                 0d0
                 (/ (- b a) (- c a)))))
      (cond
        ;; if line is vertical or horizontal and tangent to
        ;; an endpoint of curve, use that endpoint
        ((and (= (vx a1) (vx a2) (vx b1) (vx bc))
              (<= bt1 0 bt2)
              (in (vy a1) (vy b1) (vy a2)))
         (return-from intersect-segment-bezier2/range
           (v2->4 b1 (at (vy a1) (vy b1) (vy a2)) 0d0)))
        ((and (= (vx a1) (vx a2) (vx b2) (vx bc))
              (<= bt1 1 bt2)
              (in (vy a1) (vy b2) (vy a2)))
         (return-from intersect-segment-bezier2/range
           (v2->4 b2 (at (vy a1) (vy b2) (vy a2)) 1)))
        ((and (= (vy a1) (vy a2) (vy b1) (vy bc))
              (<= bt1 0 bt2)
              (in (vx a1) (vx b1) (vx a2)))
         (return-from intersect-segment-bezier2/range
           (v2->4 b1 (at (vx a1) (vx b1) (vx a2)) 0)))
        ((and (= (vy a1) (vy a2) (vy b2) (vy bc))
              (<= bt1 1 bt2)
              (in (vx a1) (vx b2) (vx a2)))
         (return-from intersect-segment-bezier2/range
           (v2->4 b2 (at (vx a1) (vx b2) (vx a2)) 1)))))
    (labels ((lerp (at a b)
               (+ (* (- 1d0 at) a) (* at b)))
             (ina (at) (and at
                            (cond
                              ((<= at1 at at2) at)
                              ((< (abs (- at at1)) eps) at1)
                              ((< (abs (- at at2)) eps) at2))))
             (inb (at) (and at
                            (cond
                              ((<= bt1 at bt2) at)
                              ((< (abs (- at bt1)) eps) bt1)
                              ((< (abs (- at bt2)) eps) bt2))))
             (ex (at) (lerp at
                            (lerp at (vx r1) (vx rc))
                            (lerp at (vx rc) (vx r2))))
             (snap1 (at)
               (if (< at eps)
                   0d0
                   (if (< (abs (- 1 at)) eps)
                       1d0
                       at)))
             (eval-snap (at bt)
               ;; return point of A evaluated at AT (snapped to
               ;; endpoints if close to 0 or 1)
               (when (and at bt)
                 (locally (declare (type double-float at bt))
                   (cond
                     ;; fixme: pass in a real epsilon
                     ((< at eps)
                      (v2->4 (p-dv (s-p1 a)) 0d0 (snap1 bt)))
                     ((< (abs (- 1 at)) eps)
                      (v2->4 (p-dv (s-p2 a)) 1d0 (snap1 bt)))
                     ;; didn't snap AT yet, so don't need to check for these
                     ((< bt eps)
                      (v2->4 (p-dv (b2-p1 b)) at 0))
                     ((< (abs (- 1 bt)) eps)
                      (v2->4 (p-dv (b2-p2 b)) at 1))
                     (t
                      (v2->4 (eval-at/s/fast a at) at bt)))))))
      (declare (inline lerp ina inb ex snap1 eval-snap))
      (cond
        ((zerop ta)
         ;; linear, shouldn't happen, but easy to solve anyway so do so
         (if (zerop tb)
             ;; horizontal line, no zeros (or same line)
             (if (zerop tc)
                 (break "coincident lines?")
                 nil)
             (let ((bt (inb (- (/ tc tb)))))
               (when bt
                 (let* ((x (ex bt))
                        (at (ina (/ x al))))
                   (declare (type (or null double-float) at bt))
                   (eval-snap at bt))))))
        ((minusp r)
         ;; no (real) solutions
         nil)
        ((< r eps)                      ; (zerop r)
         ;; one (tangent) solution (or 2 solutions too close to
         ;; distinguish reliably)
         (let ((bt (inb (/ (- tb) (* 2 ta)))))
           (when bt
             (let* ((x (ex bt))
                    (at (ina (/ x al))))
               (declare (type (or null double-float) at bt))
               (eval-snap at bt)))))
        (t
         ;; from https://people.csail.mit.edu/bkph/articles/Quadratics.pdf
         (flet ((r1 (-b r 2a 2c)
                  (if (< -b 0)
                      (/ (- -b r) 2a)
                      (/ 2c (+ -b r))))
                (r2 (-b r 2a 2c)
                  (if (< -b 0)
                      (/ 2c (- -b r))
                      (/ (+ -b r) 2a))))
           (let* ((-b (- tb))
                  (2a (* 2 ta))
                  (2c (* 2 tc))
                  (r (sqrt r))
                  ;; T values of intersections, in B
                  #++(t1 (inb (/ (+ -b r) 2a)))
                  #++(t2 (inb (/ (- -b r) 2a)))
                  (t1 (inb (r1 -b r 2a 2c)))
                  (t2 (inb (r2 -b r 2a 2c)))
                  (x1 (when t1 (ex t1)))
                  (x2 (when t2 (ex t2)))
                  (t3 (when x1 (ina (/ x1 al))))
                  (t4 (when x2 (ina (/ x2 al)))))
             (declare (type (or null double-float) t1 t2 t3 t4))
             (cond
               ((and t3 t4)
                ;; 2 good solutions
                (values (eval-snap t3 t1) (eval-snap t4 t2)))
               (t3
                (eval-snap t3 t1))
               (t4
                (eval-snap t4 t2))
               (t ;; no solutions in ranges we care about
                nil)))))))))

(defun nl (n)
  (etypecase n
    (point (list (p-dx n) (p-dy n)))
    (segment (list (list (s-dx1 n) (s-dy1 n))
                   (list (s-dx2 n) (s-dy2 n))))
    (bezier2 (list (list (b2-dx1 n) (b2-dy1 n))
                   (list (b2-dxc n) (b2-dyc n))
                   (list (b2-dx2 n) (b2-dy2 n))))))

(defun %intersect/tan (p1 pc a2 b2)
  ;; find 2nd intersection if any for 2 bezier with same first 2
  ;; control points P1,PC and final control point A2 and B2
  (let* ((p1 (p-dv p1))
         (pc (p-dv pc))
         (a2 (p-dv a2))
         (b2 (p-dv b2))
;;; todo: rewrite this using rationals?
         ;; (instead of a rotation, pick axis so p1->pc is within
         ;; 45deg, then shear pc to that axis)
         (m (align p1 pc))
         ;; rotate/translate curves so first edge is x axis
         (a3 (mv* m a2))
         (b3 (mv* m b2))
         (c2 (mv* m pc))
         ;; early out for opposite signs since they can't intersect more
         (r1 (when (or (zerop (vy a3))
                       (/= (signum (vy a3)) (signum (vy b3))))
               (return-from %intersect/tan nil)))
         ;; flip so y is up if needed
         (a4 (if (minusp (vy a3)) (v2 (vx a3) (- (vy a3))) a3))
         (b4 (if (minusp (vy a3)) (v2 (vx b3) (- (vy b3))) b3))
         ;; shear so leftmost endpoint is above pc (and possibly swap
         ;; them, so that point is a5)
         (da (/ (- (vx c2) (vx a4)) (vy a4)))
         (db (/ (- (vx c2) (vx b4)) (vy b4)))
         (ref (if (> da db) :a :b))
         (m2 (%m3 1d0 (max da db)
                  0d0
                  0d0 1d0 0d0
                  0d0 0d0 1d0))
         (a5 (mv* m2 (if (eql ref :a) a4 b4)))
         (b5 (mv* m2 (if (eql ref :a) b4 a4)))
         ;; early out if outer curve has lower y, no more
         ;; intersections in that case
         (r2 (when (<= (vy b5) (vy a5))
               (return-from %intersect/tan nil)))
         ;; scale so one curve is 0,0 1,0 1,1
         (m3 (scale (/ (vx a5)) (/ (vy a5))))
         (a6 (mv* m3 a5))
         (b6 (mv* m3 b5)))
    (declare (ignore r1 r2))
    ;; x₁ = 2t₁-t₁²
    ;; y₁ = t₁²
    ;; x₂ = t₂+((b₂ₓ-1)*t₂-t₂)*t₂=(b₂ₓ-2)t₂²+t₂
    ;; y2 = b₂yt₂²
    ;; y₁ = y₂ = t₁² = b₂yt₂²
    ;; x₁ = x₂ = 2t₁-t₁² = (b₂ₓ-2)t₂²+t₂
    ;; x = b₂ₓ, y = b₂y
    ;; t₁² = yt₂²
    ;; 2t₁-t₁² = (x-2)t₂²+t₂

    ;; x² = by²
    ;; 2x-x² = (a+1)y-y²

    ;; x=t₁,y=t₂,a=x,b=y
    ;; x² = by² , 2x-x² = (a-2)y²+y


    ;; a=b2.x, b=b2.y
    ;; x₁ = t₁+(1-t₁)t₁ = 2t₁-t₁²
    ;; y₁ = 0+1*t₁*t₁ = t₁²
    ;; x₂ = t₂+((a-1)*t₂+1-t₂)t₂ = (a-2)t₂²+2t₂
    ;; y₂ = 0+bt₂² = bt₂²

    ;; x=t₁,y=t₂
    ;; xx = 2x-x² = (a-2)y+-2y
    ;; yy = x² = by²


    ;; 2x-x² = (a-2)y²+2y
    ;; x² = by²
    ;; x = y√b, x²=by²
    ;; 2y√b-by² = (a-2)y²+2y
    #++
    (let* ((a (vx b6))
           (b (vy b6))
           (a+1^2b (* b (expt (+ a 1) 2)))
           (b-1 (- b 1))
           (b-1^2 (expt b-1 2))
           (r (sqrt (/ a+1^2b b-1^2)))
           (t1 (/ (+ (* b (- r))
                     r
                     (* 2 b))
                  b-1))
           #++(t1 (/ (+ (* r (- 1 b))
                        (* 2 b))
                     b-1))

           (t2 (sqrt (/ (expt (- (/ (* 2 b) b-1)
                                 r)
                              2)
                        b))))
      (if (eql ref :a)
          (values a6 b6 t1 t2)
          (values b6 a6 t2 t1)))
    #++
    (let* ((a (vx b6))
           (b (vy b6))
           (a+b-2 (+ a b -2))
           (r (sqrt (/ b (expt a+b-2 2))))
           (t1 (/ (* 2 (- (* a+b-2 r) b))
                  a+b-2))
           (t2 (sqrt (/ (expt (- (/ (* 2 b)
                                    a+b-2)
                                 (* 2 r))
                              2)
                        b)))
           (h (+ a (* 2 (sqrt (- a 1))))))
      (if (eql ref :a)
          (values a6 b6 t1 t2 h)
          (values b6 a6 t2 t1 h)))
    (when (<= (vy b6) (+ (vx b6) (* 2 (sqrt (- (vx b6) 1)))))
      (let* ((a (vx b6))
             (b (vy b6))
             (t2 (/ (* 2 (- (sqrt b) 1))
                    (+ a b -2)))
             (t1 (* t2 (sqrt b))))
        (flet ((e (p2 at)
                 (let ((d1 (v2- pc p1))
                       (d2 (v2+ p1 (v2+ (v2scale pc -2) p2))))
                   (v2+ (v2+ (v2scale d2 (* at at))
                             (v2scale d1 (* 2 at)))
                        p1))))
          (if (eql ref :a)
              (v2->4 (e b2 t2) t1 t2)
              (v2->4 (e a2 t2) t2 t1)))))))



(defun %intersect/tan2 (p1 .ac .bc ae be)
  ;; find 2nd intersection if any for 2 bezier with same first 2
  ;; control points P1,PC and final control point AE and BE
  (let* ((p1 (p-dv p1))
         (swap (> (v2dist p1 (p-dv .ac))
                  (v2dist p1 (p-dv .bc))))
         (ac (p-dv (if swap .bc .ac)))
         (bc (p-dv (if swap .ac .bc)))
         (a2 (p-dv (if swap be ae)))
         (b2 (p-dv (if swap ae be)))
;;; todo: rewrite this using rationals?
         ;; (instead of a rotation, pick axis so p1->pc is within
         ;; 45deg, then shear pc to that axis)
         (m (align p1 ac))
         ;; rotate/translate curves so first edge is x axis
         (ac2 (mv* m ac))
         (a3 (mv* m a2))
         (bc2 (mv* m bc))
         (b3 (mv* m b2))
         ;; early out for opposite signs since they can't intersect more
         (r1 (when (or (zerop (vy a3))
                       (/= (signum (vy a3)) (signum (vy b3))))
               (return-from %intersect/tan2 nil)))
         ;; flip so y is up if needed
         (a4 (if (minusp (vy a3)) (v2 (vx a3) (- (vy a3))) a3))
         (b4 (if (minusp (vy a3)) (v2 (vx b3) (- (vy b3))) b3))
         ;; shear so leftmost endpoint is above pc (and possibly swap
         ;; them, so that point is a5)
         (da (/ (- (vx ac2) (vx a4)) (vy a4)))
         (m2 (%m3 1d0 da 0d0
                  0d0 1d0 0d0
                  0d0 0d0 1d0))
         ;; control points should be on axis, so don't need sheared
         (a5 (mv* m2 a4))
         (b5 (mv* m2 b4))
         ;; scale so curve a is 0,0 1,0 1,1
         (m3 (scale (/ (vx a5)) (/ (vy a5))))
         (b6 (mv* m3 b5))
         (bc6 (mv* m3 bc2))
         ;; only used for debugging?
         (a6 (mv* m3 a5))
         (ac6 (mv* m3 ac2)))
    (declare (ignore r1) (ignorable a6 ac6))
    ;; a=b2.x, b=b2.y, c=bc.x
    ;; x₁ = t₁+(1-t₁)t₁ = 2t₁-t₁²
    ;; y₁ = 0+1*t₁*t₁ = t₁²

    ;; x₂ = 2(1-t₂)t₂c+t₂²a = (a-2c)t₂²+2ct₂²
    ;; y₂ = 0+bt₂² = bt₂²

    ;; x=t₁,y=t₂
    ;; x₂ = 2c(1-y)y+ay²
    ;; xx = 2x-x² = (a-2c)y²+2cy
    ;; yy = x² = by²
    ;;
    ;; 2x-x² = (a-2c)y²+2cy
    ;; x² = by²
    ;; x = y√b, x²=by²
    ;; 2y√b-by² = (a-2c)y²+2cy

    (let* ((a (vx b6))
           (b (vy b6))
           (c (vx bc6))
           (-a+2r1-a+2 (+ (- a) (* 2 (sqrt (- 1 a))) 2))
           (-a-2r1-a+2 (+ (- a) (* -2 (sqrt (- 1 a))) 2))
           (a+b-2c (+ a b (* -2 c))))
      (labels ((in (x)
                 (when (and x (< 0 x) (<= x 1)) x))
               (e0 (d1 d2 at)
                 (v2+ (v2scale d2 (* at at))
                      (v2scale d1 (* 2 at))))
               (e (pc p2 at)
                 (let ((d1 (v2- pc p1))
                       (d2 (v2+ p1 (v2+ (v2scale pc -2) p2))))
                   (v2+ (e0 d1 d2 at) p1))))

        (unless (zerop a+b-2c)
          (let* ((a+b-2c^2 (expt a+b-2c 2))
                 (r (sqrt (/ b a+b-2c^2)))
                 ;; a≤0   , b>-a+2√(1-a)+2 , 1<c<√b
                 ;; 0<a<1 , b>-a+2√(1-a)+2 , 1<c<√b
                 ;; a≥1, b>1, 1<c<√b
                 (n1 (- (* a+b-2c r) c))
                 (t1b (/ (* 2 n1) a+b-2c))
                 (t1a (when (in t1b)
                        (in (* t1b (sqrt b)))))
                 ;; a≤0, 0<b≤1,c>1
                 ;; a≤0, 1<b<-a+2√(1-a)+2,c>√b
                 ;; 0<a<1, -a-2√(1-a)+2<b≤1,c>1
                 ;; 0<a<1,1<b<-a+2√(1-a)+2,c>√b
                 (n2 (+ (* a+b-2c r) c))
                 (t2b (/ (* -2 n2) a+b-2c))
                 (t2a (when (in t2b)
                        (in (* t2b (sqrt b)))))
                 (√b (sqrt b)))
            #++
            (let ((nc (realp -a+2r1-a+2))
                  (nc2 (realp -a-2r1-a+2)))
              (format t "flags~%")
              (format t "  (or (& ~s ~s ~s), (& ~s ~s ~s)) = ~s~%"
                      (>= a 1) (> b 1) (< 1 c √b)
                      (< a 1) (and nc (> b -a+2r1-a+2)) (< 1 c √b)
                      (or (and (>= a 1) (> b 1) (< 1 c √b))
                          (and (< a 1) (and nc (> b -a+2r1-a+2)) (< 1 c √b))))
              (when (and t1a t1b)
                (format t " 1= ~s~%   ~s~%" (e ac a2 t1a) (e bc b2 t1b))
                (format t " r= ~s~%   ~s~%" (e0 ac6 a6 t1a) (e0 bc6 b6 t1b)))
              (format t "  (or (&~s ~s ~s ~s) (& ~s ~s ~s) (& ~s ~s ~s ~s) (& ~s ~s ~s)) = ~s~%"
                      (<= a 0) (< 0 b) (<= b 1) (> c 1)
                      (<= a 0) (and nc(< 1 b -a+2r1-a+2)) (> c √b)
                      (< 0 a 1) (and nc2 (< -a-2r1-a+2 b)) (<= b 1) (> c 1)
                      (< 0 a 1) (and nc (< 1 b -a+2r1-a+2)) (> c √b)
                      (or (and (<= a 0) (< 0 b) (<= b 1) (> c 1))
                          (and (<= a 0) (< 1 b -a+2r1-a+2) (> c √b))
                          (and (< 0 a 1) (< -a-2r1-a+2 b) (<= b 1) (> c 1))
                          (and (< 0 a 1) (< 1 b -a+2r1-a+2) (> c √b))))
              (when (and t2a t2b)
                (format t " 2= ~s~% ~s~%" (e ac a2 t2a) (e bc b2 t2b))
                (format t " r= ~s~%   ~s~%" (e0 ac6 a6 t2a) (e0 bc6 b6 t2b))))

            ;; filter out some invalid solutions

            ;; fixme: do this sooner to avoid calculating the values
            ;; fixme: refactor these to reduce duplicate compares
            (unless (or (and (< a 1) (> b -a+2r1-a+2) (< 1 c √b))
                        (and (>= a 1) (> b 1) (< 1 c √b)))
              (setf t1a nil
                    t1b nil))
            (unless (or (and (<= a 0) (< 0 b) (<= b 1) (> c 1))
                        (and (<= a 0) (< 1 b -a+2r1-a+2) (> c √b))
                        (and (< 0 a 1) (< -a-2r1-a+2 b) (<= b 1) (> c 1))
                        (and (< 0 a 1) (< 1 b -a+2r1-a+2) (> c √b)))
              (setf t2a nil
                    t2b nil))


            (when swap
              (rotatef ac bc)
              (rotatef a2 b2)
              (rotatef t1a t1b)
              (rotatef t2a t2b))
            (cond
              ((and t1a t1b t2a t2b)
               (break "2 solutions?")
               (values (v2->4 (e bc b2 t1b) t1a t1b)
                       (v2->4 (e bc b2 t2b) t2a t2b)))
              ((and t1a t1b)
               (v2->4 (e bc b2 t1b) t1a t1b))
              ((and t2a t2b)
               (v2->4 (e bc b2 t2b) t2a t2b)))))))))


(defun intersect-bezier2-bezier2/range (a b at1 at2 bt1 bt2)
  (declare (optimize debug #++ speed)
           (type bezier2 a b)
           (double-float at1 at2 bt1 bt2))

  ;; handle some cases of tangent at end point specially
  (labels ((in (i)
             (when (and i
                        (<= at1 (aref i 2) at2)
                        (<= bt1 (aref i 3) bt2))
               i))
           (r (i1 ra rb i2 &optional i3)
             (when (or ra rb)
               ;; might have swapped order of one or both curves to
               ;; calculate 2nd intersection, so flip it back if
               ;; needed
               (when ra
                 (when i2 (setf (aref i2 2) (- 1 (aref i2 2))))
                 (when i3 (setf (aref i3 2) (- 1 (aref i3 2)))))
               (when rb
                 (when i2 (setf (aref i2 3) (- 1 (aref i2 3))))
                 (when i3 (setf (aref i3 3) (- 1 (aref i3 3))))))
             (let ((in1 (in i1))
                   (in2 (in i2))
                   (in3 (in i3)))
               (when in2
                 (let ((aa (eval-at a (aref i2 2)))
                       (bb (eval-at b (aref i2 3))))
                   ;; todo: run tests with lower cutoff
                   (assert (< (v2dist aa bb) (expt 2d0 -3)))))
               (when in3
                 (let ((aa (eval-at a (aref i3 2)))
                       (bb (eval-at b (aref i3 3))))
                   ;; todo: run tests with lower cutoff
                   (assert (< (v2dist aa bb) (expt 2d0 -3)))))
               ;; should only be 2 possible intersections if we got
               ;; here, so if both are out of range we should return
               ;; NIL
               (return-from intersect-bezier2-bezier2/range
                 (cond
                   ((and in2 in3)
                    ;; not sure if this should be able to find more
                    ;; than 1 intersection aside from shared endpoint?
                    (error "3 solutions?"))
                   ((and in1 in2)
                    (if (< (aref i2 1) (aref i1 1))
                        (values i2 i1)
                        (values i1 i2)))
                   ((and in1 in3)
                    (if (< (aref i3 1) (aref i1 1))
                        (values i3 i1)
                        (values i1 i3)))
                   (in1 i1)
                   (in2 i2)
                   (in3 i3))))))
    (let ((a1 (p-dv (b2-p1 a)))
          (b1 (p-dv (b2-p1 b)))
          (ac (p-dv (b2-c1 a)))
          (bc (p-dv (b2-c1 b)))
          (a2 (p-dv (b2-p2 a)))
          (b2 (p-dv (b2-p2 b)))
          (split nil))
      ;; shared endpoint, shared control point
      (when (v2= ac bc)
        (cond
          ((and (v2= a1 b1) (v2= a2 b2))
           ;; identical curves, just return any endpoints in range, if
           ;; ranges are the same
           (if (and (= at1 bt1) (= at2 bt2))
               (return-from intersect-bezier2-bezier2/range
                 (cond
                   ;; not sure if this should return an intersection
                   ;; for at1/at2 if they aren't at 0,1 or not?
                   ((<= at1 0 1 at2)
                    (values (v2->4 a1 0 0)
                            (v2->4 a2 1 1)))
                   ((<= at1 0 at2)
                    (values (v2->4 a1 0 0)))
                   ((<= at1 1 at2)
                    (values (v2->4 a2 1 1)))))
               (setf split t)))
          ((and (v2= a1 b2)
                (v2= a2 b1))
           ;; identical but reversed curves, return both endpoints if
           ;; ranges are the same
           (if (and (= at1 bt2) (= at2 bt1))
               (return-from intersect-bezier2-bezier2/range
                 (values (v2->4 a1 0 1)
                         (v2->4 a2 1 0)))
               (setf split t)))
          ((v2= a1 b1)
           (r (v2->4 a1 0 0) nil nil
              (%intersect/tan (b2-p1 a) (b2-c1 a) (b2-p2 a) (b2-p2 b))))
          ((v2= a1 b2)
           (r (v2->4 a1 0 1) nil t
              (%intersect/tan (b2-p1 a) (b2-c1 a) (b2-p2 a) (b2-p1 b))))
          ((v2= a2 b1)
           (r (v2->4 a2 1 0) t nil
              (%intersect/tan (b2-p2 a) (b2-c1 a) (b2-p1 a) (b2-p2 b))))
          ((v2= a2 b2)
           (r (v2->4 a2 1 1) t t
              (%intersect/tan (b2-p2 a) (b2-c1 a) (b2-p1 a) (b2-p1 b))))))
      ;; shared endpoint and both control point all on same line
      (flet ((ld (a b c)
               (let ((db (- (vy b) (vy a)))
                     (dc (- (vy c) (vy a))))
                 (and (= (signum db) (signum dc))
                      (if (zerop db)
                          (= (signum (- (vx c) (vx a)))
                             (signum (- (vx b) (vx a))))
                          (= (/ (- (vx c) (vx a)) dc)
                             (/ (- (vx b) (vx a)) db)))))))
        ;; if either curve has control point at endpoint, skip this test
        (unless (or (v2= a1 ac) (v2= a2 ac) (v2= b1 bc) (v2= b2 bc)
                    split)
          (cond

            ((and (v2= a1 b1) (ld a1 ac bc))
             (multiple-value-call #'r (v2->4 a1 0 0) nil nil
               (%intersect/tan2 (b2-p1 a)
                                (b2-c1 a) (b2-c1 b)
                                (b2-p2 a) (b2-p2 b))))
            ((and (v2= a1 b2) (ld a1 ac bc))
             (multiple-value-call #'r (v2->4 a1 0 1) nil t
               (%intersect/tan2 (b2-p1 a)
                                (b2-c1 a) (b2-c1 b)
                                (b2-p2 a) (b2-p1 b))))
            ((and (v2= a2 b1) (ld a2 ac bc))
             (multiple-value-call #'r (v2->4 a2 1 0) t nil
               (%intersect/tan2 (b2-p2 a)
                                (b2-c1 a) (b2-c1 b)
                                (b2-p1 a) (b2-p2 b))))
            ((and (v2= a2 b2) (ld a2 ac bc))
             (multiple-value-call #'r (v2->4 a2 1 1) t t
               (%intersect/tan2 (b2-p2 a)
                                (b2-c1 a) (b2-c1 b)
                                (b2-p1 a) (b2-p1 b)))))))))

  (let* ((qa (sdf/quadratic-intersect/int::b2 (b2-dx1 a) (b2-dy1 a)
                                       (b2-dxc a) (b2-dyc a)
                                       (b2-dx2 a) (b2-dy2 a)))
         (qb (sdf/quadratic-intersect/int::b2 (b2-dx1 b) (b2-dy1 b)
                                       (b2-dxc b) (b2-dyc b)
                                       (b2-dx2 b) (b2-dy2 b)))
         (i (sdf/quadratic-intersect::intersect
             (sdf/quadratic-intersect::make-bcs qa :t1 at1 :t2 at2)
             (sdf/quadratic-intersect::make-bcs qb :t1 bt1 :t2 bt2))))
    ;; sdf/quadratic-intersect should be checking range, verify it
    (loop for x in i
          for xat1 double-float = (sdf/quadratic-intersect::at0 x)
          for xat2 double-float = (sdf/quadratic-intersect::at1 x)
          for xbt1 double-float = (sdf/quadratic-intersect::bt0 x)
          for xbt2 double-float = (sdf/quadratic-intersect::bt1 x)
          for xt1 = (a:lerp 0.5d0 xat1 xat2)
          for xt2 = (a:lerp 0.5d0 xbt1 xbt2)
          do ;; got curves back in same order we passed them, so T
             ;; values should match
             (assert (eql qa (sdf/quadratic-intersect::a x)))
             (assert (eql qb (sdf/quadratic-intersect::b x)))
             ;; reported t value evaluates to something close to
             ;; reported intersection
             (let* ((xyat (sdf/quadratic-intersect::xyat x))
                    (xy (subseq xyat 0 2))
                    (ata (aref xyat 2))
                    (atb (aref xyat 3)))
               (assert (< (v2dist xy (eval-at/b2/fast a ata)) 0.1))
               (assert (< (v2dist xy (eval-at/b2/fast b atb)) 0.1)))

          unless (and (<= at1 xt1 at2)
                      (<= bt1 xt2 bt2))
            do (error "out of range? ~s~% a= ~s~% b = ~s~%"
                      (list :a at1 xt1 at2 :b bt1 xt2 bt2)
                      (eval-at a xt2) (eval-at b xt1)))
    (cond
      ((not i)
       nil)
      ((not (cdr i))
       (sdf/quadratic-intersect::xyat (car i)))
      ((not (cddr i))
       (values (sdf/quadratic-intersect::xyat (first i))
               (sdf/quadratic-intersect::xyat (second i))))
      (t
       (values-list (mapcar 'sdf/quadratic-intersect::xyat i))))))

(defun intersect/range (a b at1 at2 bt1 bt2)
  (declare (notinline intersect-segment-bezier2/range
                      intersect-segment-segment/range
                      intersect-bezier2-bezier2/range
                      intersect-bezier2-bezier2/range)
           (optimize (debug 3)))
  ;; should possibly store these in a dx vector or inline the
  ;; intersect functions to avoid boxing?
  (let ((at1 (coerce at1 'double-float))
        (at2 (coerce at2 'double-float))
        (bt1 (coerce bt1 'double-float))
        (bt2 (coerce bt2 'double-float)))
    (when (< at2 at1) (rotatef at1 at2))
    (when (< bt2 bt1) (rotatef bt1 bt2))

    ;; check for intersection at end points, since that is common case
    (multiple-value-bind (a1 a2)
        (etypecase a
          (segment (values (p-dv (s-p1 a)) (p-dv (s-p2 a))))
          (bezier2 (values (p-dv (b2-p1 a)) (p-dv (b2-p2 a)))))
      (multiple-value-bind (b1 b2)
          (etypecase b
            (segment (values (p-dv (s-p1 b)) (p-dv (s-p2 b))))
            (bezier2 (values (p-dv (b2-p1 b)) (p-dv (b2-p2 b)))))
        (let* ((i1 (or (when (v2= a1 b1) 0d0) (when (v2= a1 b2) 1d0)))
               (i2 (or (when (v2= a2 b1) 0d0) (when (v2= a2 b2) 1d0)))
               (in1 (and i1 (<= at1 0 at2) (<= bt1 i1 bt2)))
               (in2 (and i2 (<= at1 1 at2) (<= bt1 i2 bt2))))
          (cond
            ;; intersecting 2 parts of same curve
            ((eql a b)
             (cond
               ((or (= at1 bt1) (= at1 bt2))
                (return-from intersect/range (v2->4 (eval-at a at1) at1 at1)))
               ((or (= at2 bt1) (= at2 bt2))
                (return-from intersect/range (v2->4 (eval-at a at2) at2 at2)))
               ((or (< at1 bt1 at2) (< at1 bt2 at2))
                (error "intersecting overlapping parts of 1 curve?"))
               (t (error "?"))))
            ;; we found all possible intersections, return valid ones
            #++((and i1 i2
                     ;; todo: only valid for seg+bez2 (or 2 identical segments)
                     )
                (cond
                  ((and in1 in2)
                   (return-from intersect/range (values a1 a2)))
                  (in1
                   (return-from intersect/range (values a1)))
                  (in2
                   (return-from intersect/range (values a2)))
                  (t
                   ;; we found 2 intersections, but both are out of
                   ;; ranges we care about
                   (return-from intersect/range nil))))
            ;; found 1 intersection. return it for some cases where
            ;; that can be ruled out easily
            ((and (or i1 i2)
                  ;; 2 segments only intersect once
                  (or (and (typep a 'segment) (typep b 'segment)))
                  ;; todo: if segment is tangent to bezier2 there is
                  ;; only 1 intersection
                  )
             (cond
               (in1
                (return-from intersect/range (values (v2->4 a1 0 i1))))
               (in2
                (return-from intersect/range (values (v2->4 a2 1 i2))))
               (t
                ;; found only intersection, but it is out of range
                (return-from intersect/range nil))))))))
    (flet ((segment (a b)
             (etypecase b
               (segment
                (intersect-segment-segment/range a b at1 at2 bt1 bt2))
               (bezier2
                (intersect-segment-bezier2/range a b at1 at2 bt1 bt2))))
           (bezier2 (a b)
             (etypecase b
               (segment
                (multiple-value-bind (r1 r2)
                    (intersect-segment-bezier2/range b a bt1 bt2 at1 at2)
                  (when r1 (rotatef (aref r1 2) (aref r1 3)))
                  (when r2 (rotatef (aref r2 2) (aref r2 3)))
                  (if r2
                      (values r1 r2)
                      r1)))
               (bezier2
                (intersect-bezier2-bezier2/range a b at1 at2 bt1 bt2)))))
      (etypecase a
        (segment (segment a b))
        (bezier2 (bezier2 a b))))))

(defun %bcs-eps (b)
  (etypecase b
    (bezier2
     (* double-float-epsilon
        (max 1d0 ;; used to compare T values in [0,1], so that's min epsilon
             (reduce 'max (p-dv (b2-p1 b)) :key 'abs)
             (reduce 'max (p-dv (b2-c1 b)) :key 'abs)
             (reduce 'max (p-dv (b2-p2 b)) :key 'abs))))
    (segment
     (* double-float-epsilon
        (max 1d0 ;; used to compare T values in [0,1], so that's min epsilon
             (reduce 'max (p-dv (s-p1 b)) :key 'abs)
             (reduce 'max (p-dv (s-p2 b)) :key 'abs))))))

;; variants of point, bezier and segment with some extra slots for
;; optimizing distance calcs
(defstruct (pointb
            (:include point)
            (:conc-name pb-)
            (:constructor %make-pointb (rv dv bc br &optional
                                           (rm 0) (gm 0) (bm 0) n)))
  ;; bounding circle radius and center
  (br 0d0 :type double-float)
  (bc (v2 0d0 0d0) :type v2)
  ;; msdf masks
  (rm T :type (or t nil))
  (gm T :type (or t nil))
  (bm T :type (or t nil))
  ;; original node
  (n (make-point) :type point))

(defun pointb (p &optional (r 0) (g 0) (b 0))
  (let* ((dv (p-dv p))
         (rv (p-rv p)))
    (%make-pointb rv dv dv 0d0 r g b p)))

(defstruct (segmentb
            (:include segment)
            (:conc-name sb-)
            (:constructor %make-segmentb (p1 p2 bc br &optional
                                             (rm 0) (gm 0) (bm 0) n)))
  ;; bounding circle radius and center
  (br 0d0 :type double-float)
  (bc (v2 0d0 0d0) :type v2)
  ;; msdf masks
  (rm T :type (or t nil))
  (gm T :type (or t nil))
  (bm T :type (or t nil))
  ;; original node
  (n (error "required slot") :type segment))

(defun segmentb (s &optional (r 0) (g 0) (b 0))
  (let* ((p1 (s-p1 s))
         (p2 (s-p2 s))
         (v1 (p-dv p1))
         (v2 (p-dv p2)))
    (%make-segmentb p1 p2 (v2lerp v1 v2 0.5) (/ (v2dist v1 v2) 2) r g b s)))

(defstruct (bezier2b
            (:include bezier2)
            (:conc-name b2b-)
            (:constructor %make-bezier2b (p1 c1 p2 bc br &optional
                                             (rm 0) (gm 0) (bm 0) n)))
  ;; bounding circle radius and center
  (br 0d0 :type double-float)
  (bc (v2 0d0 0d0) :type v2)
  ;; msdf masks
  (rm T :type (or t nil))
  (gm T :type (or t nil))
  (bm T :type (or t nil))
  ;; original node
  (n (error "required slot") :type bezier2))

(defun bezier2b (b2 &optional (r 0) (g 0) (b 0))
  (flet ((c2 (a b c)
           (let ((p (v2lerp a b 0.5))
                 (r (/ (v2dist a b) 2)))
             (when (<= (v2dist p c) r)
               (list p r))))
         (c3 (a b c)
           (let* ((b (v2- b a))
                  (c (v2- c a))
                  (1/d (/ (* 2 (v2x b c))))
                  (bx2+by2 (+ (expt (vx b) 2) (expt (vy b) 2)))
                  (cx2+cy2 (+ (expt (vx c) 2) (expt (vy c) 2)))
                  (x (* 1/d (- (* (vy c) bx2+by2)
                               (* (vy b) cx2+cy2))))
                  (y (* 1/d (- (* (vx b) cx2+cy2)
                               (* (vx c) bx2+by2))))
                  (p (v2 x y)))
             (list (v2+ a p) (v2mag p)))))
    (let* ((p1 (b2-p1 b2))
           (c1 (b2-c1 b2))
           (p2 (b2-p2 b2))
           (v1 (p-dv p1))
           (v2 (p-dv c1))
           (v3 (p-dv p2))
           (bounds (or (c2 v1 v2 v3)
                       (c2 v1 v3 v2)
                       (c2 v3 v2 v1)
                       (c3 v1 v2 v3))))
      (%make-bezier2b p1 c1 p2 (first bounds) (second bounds) r g b b2))))
