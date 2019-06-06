(in-package #:sdf)

(defclass point ()
  ((p0 :accessor p0 :initarg :p0)
   (d :accessor d :initarg :d)
   (corner :accessor corner :initarg :corner)
   (c1 :accessor c1 :initarg :c1)
   (c2 :accessor c2 :initarg :c2)
   (t1 :accessor t1 :initarg :t1)
   (t2 :accessor t2 :initarg :t2)
   (color :accessor color :initform #(t t t))
   (start :accessor start :initform nil :initarg :start)))

(defmethod c ((p point))
  (p0 p))
(defmethod r ((p point))
  0.0)

(defun make-point (p start c1 c2)
  (let* ((f (equalp p (p0 c1)))
         (n1 (v2n (normal-at-end c1 p)))
         (n2 (v2n (normal-at-end c2 p)))
         (t1 (tangent-at-end c1 p))
         (t2 (tangent-at-end c2 p))
         (s (signum (if f
                        (v2x n1 n2)
                        (v2x n1 n2))))
         (d (v2+ n1 n2))
         (l (v2mag d))
         (corner nil))
    (unless (or (and (equalp p (p0 c1))) (equalp p (p2 c2))
                (and (equalp p (p2 c1))) (equalp p (p0 c2)))
      (format t "~s: ~s ~s~%" p (p0 c1) (equalp p (p0 c1)))
      (format t "~s: ~s ~s~%" p (p2 c1) (equalp p (p2 c1)))
      (format t "~s: ~s ~s~%" p (p0 c2) (equalp p (p0 c2)))
      (format t "~s: ~s ~s~%" p (p2 c2) (equalp p (p2 c2)))
      (break "br,sd"))
    (when (> (abs (v2x t1 t2)) 0.1)
      (setf corner t))
    (cond
      ((zerop s)
       (if f
           (setf d (v2scale n1 1))
           (setf d (v2scale n2 1))))
      ((and (not (zerop s))
            (not (zerop l)))
       (setf d (v2n d)))

      (t (break "scvabj")))
    (make-instance 'point
                   :p0 p
                   :d d
                   :c1 c1 :c2 c2
                   :t1 t1
                   :t2 t2
                   :start start
                   :corner corner)))

(defmethod sign-at ((p point) at n)
  (if (zerop at)
      1
      (if (minusp at)
          -1 1)))

(defclass line ()
  ((p0 :accessor p0 :initarg :p0)
   (p2 :accessor p2 :initarg :p2)
   (d :accessor d :initarg :d)
   (l :accessor l :initarg :l)
   (c :accessor c :initarg :c)
   (r :accessor r :initarg :r)
   (n :accessor n :initarg :n)
   (color :accessor color :initform #(t t t))))

(defmethod eval-at ((line line) at)
  (v2+ (p0 line) (v2scale (d line) at)))

(defmethod normal-at-end ((line line) at)
  (n line))
(defmethod tangent-at-end ((line line) at)
  (v2n (d line)))

(defun make-line (p0 p2)
  (let ((d (v2- p2 p0))
        (c (v2scale (v2+ p0 p2) 0.5)))
    (make-instance 'line :p0 p0 :p2 p2 :d d :l (v2. d d)
                         :c c :r (v2dist p0 c)
                         :n (v2n (v2scale (v2tx d) -1)))))

(defclass quadratic ()
  ((p0 :accessor p0 :initarg :p0)
   (p1 :accessor p1 :initarg :p1)
   (p2 :accessor p2 :initarg :p2)
   (d1 :accessor d1 :initarg :d1)
   (d2 :accessor d2 :initarg :d2)
   (|2d1| :accessor |2d1| :initarg :2d1)
   (s1 :accessor s1 :initarg :s1)
   (s2 :accessor s2 :initarg :s2)
   (s3 :accessor s3 :initarg :s3)
   (p2-2p1+p0 :accessor p2-2p1+p0 :initarg :p2-2p1+p0)
   (c :accessor c :initarg :c)
   (r :accessor r :initarg :R)
   (n1 :accessor n1 :initarg :n1)
   (n2 :accessor n2 :initarg :n2)
   (t1 :accessor t1 :initarg :t1)
   (t2 :accessor t2 :initarg :t2)
   (color :accessor color :initform #(t t t))))

(defun make-quadratic (p0 p1 p2)
  (let* ((d1 (v2- p1 p0))
         (d2 (v2- p2 (v2- (v2scale p1 2) p0)))
         (c (v2scale (v2+ (v2+ p0 p1) p2) 1/3))
         (t1 (v2n (v2- p1 p0)))
         (t2 (v2n (v2- p2 p1))))
    (make-instance 'quadratic :p0 p0 :p1 p1 :p2 p2
                              :d1 d1 :d2 d2
                              :2d1 (v2scale d1 2)
                              :s1 (v2. d2 d2)
                              :s2 (* 3 (v2. d1 d2))
                              :s3 (* 2 (v2. d1 d1))
                              :p2-2p1+p0 (v2+ (v2- p2 (v2scale p1 2))
                                              p0)
                              :c c
                              :r (max (v2dist c p0)
                                      (v2dist c p1)
                                      (v2dist c p2))
                              :t1 t1 :t2 t2
                              :n1 (v2tx t1)
                              :n2 (v2tx t2))))

(defmethod eval-at ((q quadratic) at)
  (v2+ (v2+ (v2scale (d2 q) (* at at))
            (v2scale (d1 q) (* 2 at)))
       (p0 q)))

(defmethod normal-at-end ((q quadratic) at)
  (cond
    ((equalp at (p0 q))
     (n1 q))
    ((equalp at (p2 q))
     (n2 q))
    (t
     (error "not endpoint"))))
(defmethod tangent-at-end ((q quadratic) at)
  (cond
    ((equalp at (p0 q))
     (t1 q))
    ((equalp at (p2 q))
     (t2 q))
    (t
     (error "not endpoint"))))


(defmethod sign-at ((q quadratic) at n)
  ;; separate sign calc so we don't need to calculate it if we know it
  ;; is too far away anyway
  (let ((db (v2+ (v2scale (p2-2p1+p0 q) (* 2 at))
                 (|2d1| q))))
    (if (plusp (v2x db n)) 1 -1)))

(defun dist/line* (p p0 n)
  (let* ((l (v2. n n))
         (tt (/ (v2. (v2- p p0) n) l))
         (pp (v2+ p0 (v2scale n tt)))
         (s (v2x n (v2- pp p))))
    (* (if (plusp s) 1 -1)
       (v2dist p pp))))

(defun dist/line (p l)
  (let* ((p0 (p0 l))
         (d (d l)))
    (let* ((tt (/ (v2. (v2- p p0) d)
                  (l l)))
           (pp (when (< 0 tt 1)
                 (eval-at l tt)))
           (s (when pp
                (v2x d (v2- pp p)))))
      (if pp
          (* (if (plusp s) 1 -1)
             (v2dist p pp))
          most-positive-single-float))))

(defun solve-cubic (a b c d)
  (declare (notinline expt))
  (let* ((a (coerce a 'double-float))
         (b (coerce b 'double-float))
         (c (coerce c 'double-float))
         (d (coerce d 'double-float))
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
       (list (/ b (* -3 a))))
      ((zerop det)
       (list (/ (- (* 9 ad) bc)
                (* 2 det0))
             (/ (- (* 4 abc) (* 9 a ad) b3)
                (* a det0))))
      (t
       (let* ((det1 (+ (* 2 b3) (* -9 abc) (* 27 a ad)))
              (pm (sqrt (* -27 a a det)))
              (cc (expt
                   (/ (if (= det1 pm)
                          (+ det1 pm)
                          (- det1 pm))
                      2)
                   1/3))
              (xi (+ -1/2 (/ #c(0 #.(sqrt 3)) 2))))
         (loop for k upto 2
               for r = (* (/ (* -3 a))
                          (+ b
                             (* (expt xi k) cc)
                             (/ det0 (* (expt xi k) cc))))
               when (and (< (abs (imagpart r)) 0.001)
                         (< 0 (realpart r) 1))
                 collect (realpart r)))))))

(defun dist/curve (p q)
  (let* ((d (v2- p (p0 q)))
         (d1 (d1 q))
         (d2 (d2 q))
         (roots (solve-cubic
                 (s1 q)
                 (s2 q)
                 (- (s3 q) (v2. d2 d))
                 (* -1 (v2. d1 d)))))
    (loop with dd = most-positive-single-float
          for r in roots
          for b = (eval-at q r)
          for d = (v2dist p b)
          when (< d (abs dd))
            do (let* ((s (sign-at q r (v2- b p))))
                 (setf dd (* s d)))
          finally (return dd))))

(defun dist/point (p s)
  (let ((tt (v2. (v2- p (p0 s)) (d s))))
    (assert (< 0.9 (v2mag (d s)) 1.1))
    (* (sign-at s tt 0)
       (v2dist p (p0 s))))
  (v2dist p (p0 s)))

(defun dist/s (p s)
  (etypecase s
    (point (dist/point p s))
    (quadratic (dist/curve p s))
    (line (dist/line p s))))

(defun cull (s p max)
  (unless (typep s 'point)
    (assert (plusp (r s)))
    (let ((c (> (abs (v2dist (c s) p)
                     )
                (+ (abs max) (r s)))))

      ;; cull to control points in addition to bounding circle
      #++ ;; seems counterproductive currently, try again when optimizing
      (when (and (typep s 'quadratic) (not c))
        (setf c
              (and (not (minusp (v2. p (d1 s))))
                   ;; fixme: precalc these v2-
                   (not (minusp (v2. p (v2- (p2 s) (p1 s)))))
                   (not (minusp (v2. p (v2- (p0 s) (p2 s))))))))
      c)))

(defclass shape ()
  ((points :accessor points :initarg :points)
   (lines :accessor lines :initarg :lines)
   (curves :accessor curves :initarg :curves)
   (qtree :reader qtree :initarg :qtree)))

(defun pseudo-distance/p (p s dist)
  (declare (ignore dist))
  ;; d1/2 is signed pseudo-distance from c1/c2 intersecting at curve
  (let ((d1 (dist/line* p (p0 s) (v2scale (t1 s) 1)))
        (d2 (dist/line* p (p0 s) (v2scale (t2 s) 1)))
        (coloring (vector nil nil nil))
        (ret (vector 1000000 1000000 1000000)))
    ;; smooth edges just return actual distance
    (unless (corner s)
      ;; d1/d2 should be nearly equal
      (return-from pseudo-distance/p
        (if (<= (abs d1) (abs d2))
            (vector d1 d1 d1)
            (vector d2 d2 d2))))

    ;; due to way colors are assigned, at a corner we have 3 cases we
    ;; care about for the 3 color channels at corners: both edges on,
    ;; edge 1 on, edge 2 on. assign those in that order to COLORING
    (loop for a across (color (c1 s))
          for b across (color (c2 s))
          for i from 0
          do (setf (aref coloring
                         (cond ((and a b) 0) (a 1) (b 2) (t (error "?"))))
                   i))
    ;; assign value for channel affected by both edges
    (let* (;;pick whichever is closest to orthogonal to edge
          (d (v2- p (p0 s)))
          (x1 (v2x d (t2 s)))
          (x2 (v2x d (t1 s))))
     (setf (aref ret (aref coloring 0))
           (if (<= (abs x1) (abs x2))
               d1 d2)))
    ;; assign single-edge channels
    (setf (aref ret (aref coloring 1)) d1)
    (setf (aref ret (aref coloring 2)) d2)

    ret))


(defun pseudo-distance (p s d)
  (if (typep s 'point)
      (pseudo-distance/p p s d)
      (coerce (loop for c across (color s)
                    collect (if c d nil))
              'vector)))

(defun dist/c (p shape)
  (let ((d (vector most-positive-single-float
                   most-positive-single-float
                   most-positive-single-float
                   ;; max of channel distances, for culling
                   most-positive-single-float))
        (pd (vector most-positive-single-float
                    most-positive-single-float
                    most-positive-single-float)))
    (loop for s in (append (subseq (points shape) 0 )
                           (lines shape)
                           (curves shape))
          for cull = (cull s p (aref d 3))
          for dp = (unless cull
                     (dist/s p s))
          for pdp = (when dp
                      (pseudo-distance p s dp))

          when (complexp dp)
            do (break "complex ~s?" dp)
          do (when dp
               (loop for i below 3
                     when (and
                               (aref pdp i)
                               (< (abs dp) (abs (aref d i))))
                       do (setf (aref d i) dp)
                          (setf (aref pd i) (aref pdp i))
                          (setf (aref d 3) 0)
                          (loop for x across d
                                when (> (abs x) (abs (aref d 3)))
                                  do (setf (aref d 3) x)))))
    (list (aref pd 0) (aref pd 1) (aref pd 2))))

(defun scale-point (p s)
  (when p
    (v2 (* s (zpb-ttf:x p))
        (* s (zpb-ttf:y p)))))

(defun translate-glyph (glyph scale)
  (let ((points (make-hash-table :test 'equalp))
        (lines)
        (curves)
        (first)
        (start (make-hash-table :test 'equalp)))
    (zpb-ttf:do-contours (c glyph)
      (setf first nil)
      (zpb-ttf:do-contour-segments (p0 p1 p2) c
        (let ((p0 (scale-point p0 scale))
              (p1 (scale-point p1 scale))
              (p2 (scale-point p2 scale)))
          (unless first
            (setf (gethash p0 start) t)
            (setf first p0))
          (let ((s (if p1
                       (make-quadratic p0 p1 p2)
                       (make-line p0 p2))))
            (if p1
                (push s curves)
                (push s lines))
            (push s (gethash p0 points))
            (push s (gethash p2 points)))))
      (setf (gethash first points)
            (reverse (gethash first points)))
      (setf first nil))
    (make-instance 'shape
                   :points
                   (loop for i from 0
                         for k being the hash-key of points using (hash-value v)
                         do (assert (= (length v) 2))
                         collect (apply 'make-point k (gethash k start)
                                        v))
                   :lines lines
                   :curves curves)))

(defvar *f* (list nil))
(defun sdf (font glyph font-scale ms-scale spread)
  (declare (ignore ms-scale font))
  (let* ((scale font-scale)
         (spread (/ spread))
         (gw (- (xmax glyph) (xmin glyph)))
         (gh (- (ymax glyph) (ymin glyph)))
         (padding spread)
         ;(aspect (/ gh gw))
         (dw (ceiling (+ 2 (* 2 padding) (* gw scale))))
         (dh (ceiling (+ 2 (* 2 padding) (* gh scale)))))
    (format t "~s / ~s ~sx~s~%" (zpb-ttf:postscript-name glyph)
            (zpb-ttf:code-point glyph) dw dh)
    (when (or (zerop gw) (zerop gh))
      (return-from sdf (values (aa-misc:make-image 4 4 #(0 0 0)) 2)))
    (let* ((segments nil))
      (setf segments (translate-glyph glyph scale))
      (setf (car *f*) segments)
      (let* ((dest (aa-misc:make-image (ceiling dw) (ceiling dh) #(0 0 0)))
             (write (aa-misc:image-put-pixel dest #(255 255 255))))
        (declare (ignorable write))
        (loop for y below (array-dimension dest 0)
              do (loop for x below (array-dimension dest 1)
                       for fx = (- x (- (/ (- dw (* scale gw) 1)
                                           2)
                                        (* (xmin glyph) scale)))
                       for fy =  (- y (- (/ (- dh (* scale gh) 1)
                                            2)
                                         (* (ymin glyph) scale)))
                       for d = (dist/c (v2 fx fy) segments)
                       for dy = y
                       do (funcall write x dy
                                   (max 0 (+ 128 (* 128
                                                    (/ (first d) spread)))))))
        (aa-misc:save-image "/tmp/font2a.pnm" dest :pnm)
        #++(aa-misc:save-image "/tmp/font2h.pnm" image :pnm)
        (values dest padding)))))

#++
(require 'sdf)

#++
(zpb-ttf:with-font-loader (ttf "georgia.ttf")
  (let ((g (zpb-ttf:find-glyph
            (print (char "kSWA*OXI5" 0))
            ;;(print (alexandria:random-elt *default-characters*))
            ttf))
        (scale (print (float (/ 64 (- (zpb-ttf:ascender ttf)
                                       (zpb-ttf:descender ttf)))))))
    (time (msdf ttf g
              scale nil 0.1
              ;;(print (+ 10 (random 564))) (/ (print (+ 3 (random 10))))
               ))
    nil))
#++
(time
 (let ((a (time (make-atlas "georgia.ttf" 32 :spread 0.1
                            :width 512 :height 300))))
   (time (save-atlas a "/tmp/sdf3.png" "sdf1.met"))))
#++
(time
 (let ((a (time (make-atlas "georgia.ttf" 62 :spread 0.4
                            :width 512 :height 512
                            :backend :ms))))
   (time (save-atlas a "/tmp/sdf4.png" "sdf1.met"))))
