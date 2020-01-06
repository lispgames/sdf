(in-package #:sdf)
#++
(require 'sdf)

(defvar *backend* :sdf)
(defvar *corner-angle-threshhold* (* 1 (/ pi 180)))

(declaim (inline s-p0 s-p2 s-t1 s-t2 s-channels s-start s-c s-r))
(defstruct (segment (:conc-name s-))
  (p0 (v2 0 0) :type v2)
  (p2 (v2 0 0) :type v2)
  (t1 (v2 1 0) :type v2)
  (t2 (v2 1 0) :type v2)
  ;; flags for presense in each of R,G,B
  (channels #(t t t) :type (vector (member t nil) 3))
  ;; starts a contour
  (start nil :type (member t nil))
  ;; bounding circle
  (c (v2 0 0) :type v2)
  (r 0.0 :type double-float))

(declaim (inline l-p0 l-p2 l-t1 l-t2 l-channels l-start l-c l-r
                 l-dir l-l %make-line))
(defstruct (line (:conc-name l-) (:include segment)
                 (:constructor %make-line))
  (dir (v2 0 0) :type v2)
  (d.d 0.0 :type double-float))

(declaim (inline c-p0 c-p2 c-t1 c-t2 c-channels c-start c-c c-r
                 c-p1 l-d1 l-d2 l-2d1 l-s1 l-s2 l-s3 l-p2-2p1+p0
                 %make-curve))
(defstruct (curve (:conc-name c-) (:include segment)
                  (:constructor %make-curve))
  (p1 (v2 0 0) :type v2)
  (d1 (v2 0 0) :type v2)
  (d2 (v2 0 0) :type v2)
  (\2d1 (v2 0 0) :type v2)
  (s1 0.0 :type double-float)
  (s2 0.0 :type double-float)
  (s3 0.0 :type double-float)
  (p2-2p1+p0 (v2 0 0) :type v2)
  (p2-p1 (v2 0 0) :type v2)
  (p0-p2 (v2 0 0) :type v2))

(declaim (inline p-p0 p-e1 p-e2 p-t1 p-t2))

(defstruct (point (:conc-name p-) (:constructor %make-point))
  (p0 (v2 0 0) :type v2)
  (e1 nil :type (or line curve null))
  (e2 nil :type (or line curve null))
  (t1 (v2 1 0) :type v2)
  (t2 (v2 1 0) :type v2)
  (corner nil :type (member t nil)))


(defclass shape ()
  ((points :accessor points :initarg :points)
   (lines :accessor lines :initarg :lines)
   (curves :accessor curves :initarg :curves)
   (contours :accessor contours :initarg :contours)
   (%next :reader %next :initform (make-hash-table))
   (%prev :reader %prev :initform (make-hash-table))
   (qtree :reader qtree :initarg :qtree)))

(defun next (shape element)
  (gethash element (%next shape)))
(defun (setf next) (new shape element)
  (setf (gethash element (%next shape)) new))

(defun prev (shape element)
  (gethash element (%prev shape)))
(defun (setf prev) (new shape element)
  (setf (gethash element (%prev shape)) new))


(defun make-point (p edge1 edge2)
  (check-type p v2)
  (check-type edge1 segment)
  (check-type edge2 segment)
  ;; make sure e1 and e2 are in order (p0 at end of e1, beginning of e2)
  ;; (but we need original order for color assignment code)
  (when (equalp (s-p0 edge1) p)
    (assert (equalp (s-p2 edge2) p))
    (rotatef edge1 edge2))
  (let ((t1 (v2n (s-t2 edge1)))
        (t2 (v2n (s-t1 edge2)))
        (corner nil))
    (let ((dot (v2. t1 t2)))
     (when (or #++(<= dot 0)
               (< dot (cos *corner-angle-threshhold*)))
       (setf corner t)))
    (%make-point :p0 p
                 :e1 edge1
                 :e2 edge2
                 :t1 t1
                 :t2 (s-t1 edge2)
                 :corner corner)))

(defun make-line (a b start)
  (declare (optimize speed))
  (check-type a v2)
  (check-type b v2)
  (let* ((d (v2- b a))
         (l (v2mag d))
         (t1 (v2n d)))
    (%make-line :p0 a
                :p2 b
                :t1 t1
                :t2 t1
                :start start
                :c (v2scale (v2+ a b) 0.5)
                :r (/ l 2.0)
                :dir d
                :d.d (v2. d d))))

(defun make-curve (p0 p1 p2 start)
  (declare (optimize speed))
  (check-type p0 v2)
  (check-type p1 v2)
  (check-type p2 v2)
  (let* ((d1 (v2- p1 p0))
         (d2 (v2- p2 (v2- (v2scale p1 2) p0)))
         (c (v2scale (v2+ (v2+ p0 p1) p2) #.(coerce 1/3 'double-float)))
         (t1 (v2n (v2- p1 p0)))
         (t2 (v2n (v2- p2 p1))))
    (%make-curve :p0 p0 :p1 p1 :p2 p2
                 :d1 d1 :d2 d2
                 :2d1 (v2scale d1 2)
                 :s1 (v2. d2 d2)
                 :s2 (* 3 (v2. d1 d2))
                 :s3 (* 2 (v2. d1 d1))
                 :p2-2p1+p0 (v2+ (v2- p2 (v2scale p1 2))
                                 p0)
                 :p0-p2 (v2- p0 p2)
                 :p2-p1 (v2- p2 p1)
                 :t1 t1 :t2 t2
                 :start start
                 :c c
                 :r (max (v2dist c p0)
                         (v2dist c p1)
                         (v2dist c p2)))))




(defun dist/line* (p p0 n)
  (declare (type v2 p p0 n)
           (optimize speed))
  (let* ((l (v2. n n))
         (tt (/ (v2. (v2- p p0) n) l))
         (pp (v2+ p0 (v2scale n tt)))
         (s (v2x n (v2- pp p))))
    (* (if (plusp s) 1 -1)
       (v2dist p pp))))

(declaim (inline dist/p))
(defun dist/p (p s)
  (declare (optimize speed))
  (check-type s point)
  (check-type p v2)
  (v2dist p (p-p0 s)))

(declaim (inline eval-line))
(defun eval-line (line at)
  (declare (type line line) (type double-float at))
  (v2+ (l-p0 line) (v2scale (l-dir line) at)))

(defun dist/l (p line)
  (declare (optimize speed))
  (check-type line line)
  (check-type p v2)
  (let* ((p0 (l-p0 line))
         (d (l-dir line)))
    (let* ((tt (/ (v2. (v2- p p0) d)
                  (l-d.d line)))
           (pp (when (< 0d0 tt 1d0)
                 (eval-line line tt)))
           (s (if pp
                  (v2x d (v2- pp p))
                  0d0)))
      (if pp
          (* (if (plusp s) 1 -1)
             (v2dist p pp))
          most-positive-double-float))))

(defun solve-cubic (a b c d)
  (declare (type double-float a b c d)
           (optimize speed))
  (let* ((bc (* b c))
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
       (let ((r (make-array 1 :element-type 'double-float)))
         (setf (aref r 0)
               (if (zerop a)
                   0d0
                   (/ b (* -3 a))))
         r))
      ((zerop det)
       (let ((r (make-array 2 :element-type 'double-float)))
         (setf (aref r 0) (/ (- (* 9 ad) bc)
                             (* 2 det0))
               (aref r 1) (/ (- (* 4 abc) (* 9 a ad) b3)
                             (* a det0)))
         r))
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
         (coerce
          (loop for k upto 2
                for r = (* (/ (* -3 a))
                           (+ b
                              (* (expt xi k) cc)
                              (/ det0 (* (expt xi k) cc))))
                when (and (< (abs (imagpart r)) 0.001)
                          (< 0 (realpart r) 1))
                  collect (realpart r))
          '(simple-array double-float 1)))))))

(defun eval-curve (c at)
  (declare (type curve c)
           (double-float at))
  (v2+ (v2+ (v2scale (c-d2 c) (* at at))
            (v2scale (c-d1 c) (* 2 at)))
       (c-p0 c)))


(defun sign-at-curve (q at n)
  (declare (type curve q)
           (type double-float at)
           (type v2 n))
  ;; separate sign calc so we don't need to calculate it if we know it
  ;; is too far away anyway
  (let ((db (v2+ (v2scale (c-p2-2p1+p0 q) (* 2 at))
                 (c-2d1 q))))
    (if (plusp (v2x db n)) 1 -1)))

(defun dist/c (p c)
  (declare (optimize speed))
  (check-type c curve)
  (check-type p v2)
  (let* ((d (v2- p (c-p0 c)))
         (d1 (c-d1 c))
         (d2 (c-d2 c))
         (roots (solve-cubic
                 (c-s1 c)
                 (c-s2 c)
                 (- (c-s3 c) (v2. d2 d))
                 (* -1 (v2. d1 d)))))
    (declare (type (simple-array double-float (*)) roots))
    (loop with dd of-type double-float = most-positive-double-float
          for r across roots
          for b = (eval-curve c r)
          for d = (v2dist p b)
          when (< d (abs dd))
            do (let* ((s (sign-at-curve c r (v2- b p))))
                 (declare (fixnum s))
                 (setf dd (* s d)))
          finally (return dd))))

(defun dist/* (p s)
  (etypecase s
    (point (dist/p p s))
    (line (dist/l p s))
    (curve (dist/c p s))))

(defun cull (s p max)
  (declare (optimize speed)
           (type double-float max))
  (unless (typep s 'point)
    (assert (plusp (s-r s)))
    (let ((c (> (abs (v2dist (s-c s) p))
                (+ (abs max) (s-r s)))))

      ;; cull to control points in addition to bounding circle
      ;; seems counterproductive currently, try again when optimizing
      #++
      (when (and (typep s 'curve) (not c))
        (setf c
              (and (not (minusp (v2. p (c-d1 s))))
                   ;; fixme: precalc these v2-
                   (not (minusp (v2. p (c-p2-p1 s))))
                   (not (minusp (v2. p (c-p0-p2 s)))))))
      c)))

(defun pseudo-distance/p (p s)
  ;; d1/2 is signed pseudo-distance from c1/c2 intersecting at curve
  (let ((d1 (dist/line* p (p-p0 s) (v2scale (p-t1 s) 1)))
        (d2 (dist/line* p (p-p0 s) (v2scale (p-t2 s) 1)))
        (coloring (vector nil nil nil))
        (ret (vector 1000000d0 1000000d0 1000000d0)))
    ;; smooth edges just return actual distance
    (unless (p-corner s)
      ;; d1/d2 should be nearly equal
      (loop with d = (if (< (abs d1) (abs d2)) d1 d2)
            for a across (s-channels (p-e1 s))
            for i from 0
            when a do (setf (aref coloring i) d))
      (return-from pseudo-distance/p
        coloring))

    ;; due to way colors are assigned, at a corner we have 3 cases we
    ;; care about for the 3 color channels at corners: both edges on,
    ;; edge 1 on, edge 2 on. assign those in that order to COLORING
    (loop for a across (s-channels (p-e1 s))
          for b across (s-channels (p-e2 s))
          for i from 0
          do (setf (aref coloring
                         (cond ((and a b) 0) (a 1) (b 2) (t (error "?"))))
                   i))
    ;; assign value for channel affected by both edges
    (let* (;;pick whichever is closest to orthogonal to edge
           (d (v2- p (p-p0 s)))
           (x1 (v2x d (p-t2 s)))
           (x2 (v2x d (p-t1 s))))
      (setf (aref ret (aref coloring 0))
            (if (<= (abs x1) (abs x2))
                d1 d2)))
    ;; assign single-edge channels
    (setf (aref ret (aref coloring 1)) d1)
    (setf (aref ret (aref coloring 2)) d2)

    ret))

(defun pseudo-distance/p1 (p s)
  ;; for single channel sdf, always look at both
  (let ((d1 (dist/line* p (p-p0 s) (v2scale (p-t1 s) 1)))
        (d2 (dist/line* p (p-p0 s) (v2scale (p-t2 s) 1)))
        (d nil))
    (setf d
          (if (eql *backend* :psdf)
              (if (> (abs d1) (abs d2))
                  d1 d2)
              (let ((dx (v2dist p (p-p0 s))))
                (* dx (signum
                       (if (> (abs d1) (abs d2))
                           d1 d2))))))
    (vector d d d)))

(defun pseudo-distance (p s d multichannel)
  (if (typep s 'point)
      (if multichannel
          (pseudo-distance/p p s)
          (pseudo-distance/p1 p s))
      (coerce (loop for c across (s-channels s)
                    collect (if c d nil))
              'vector)))

(defun dist/s (p shape pdist)
  (let ((d (vector most-positive-double-float
                   most-positive-double-float
                   most-positive-double-float
                   ;; max of channel distances, for culling
                   most-positive-double-float))
        (pd (vector most-positive-double-float
                    most-positive-double-float
                    most-positive-double-float))
        (d1 (/ most-positive-double-float 256)))
    (loop for s in (append (points shape)
                           (lines shape)
                           (curves shape))
          for cull = (cull s p (aref d 3))
          for dp = (unless cull
                     (dist/* p s))
          for pdp = (when dp
                      (pseudo-distance p s dp pdist))
          when (complexp dp)
            do (break "complex ~s?" dp)
          do (when dp
               (loop for i below 3
                     when (and
                           (aref pdp i)
                           (< (abs dp) (abs (aref d i))))
                       do (setf (aref d i) dp)
                          (setf (aref pd i) (aref pdp i))
                          (setf (aref d 3)
                                (loop for i below 3
                                      maximize (aref d i)))
                          (when (< (abs dp) (abs d1))
                            (setf d1
                                  (aref (remove nil (pseudo-distance p s dp nil))
                                        0))))))
    (list (aref pd 0) (aref pd 1) (aref pd 2) d1)))

(defun scale-point (p s)
  (when p
    (v2 (* s (zpb-ttf:x p))
        (* s (zpb-ttf:y p)))))

(defun translate-glyph (glyph scale &key filter)
  (let ((points (make-hash-table :test 'equalp))
        (lines)
        (curves)
        (first)
        (shapes)
        (contours))
    (zpb-ttf:do-contours (c glyph)
      (setf first t)
      (zpb-ttf:do-contour-segments (p0 p1 p2) c
        (let ((p0 (scale-point p0 scale))
              (p1 (scale-point p1 scale))
              (p2 (scale-point p2 scale)))
          (let ((s (if p1
                       (make-curve p0 p1 p2 first)
                       (make-line p0 p2 first))))
            (setf first nil)
            (if p1
                (push s curves)
                (push s lines))
            (push s contours)
            (push s (gethash p0 points))
            (push s (gethash p2 points)))))
      (setf first nil)
      (setf contours (nreverse contours))
      (push
       (make-instance 'shape
                      :points
                      #++(loop for i from 0
                            for k being the hash-key of points using (hash-value v)
                            do (assert (= (length v) 2))
                            collect (apply 'make-point k v))

                      (loop for c in contours
                            collect (apply 'make-point (s-p0 c)
                                           (gethash (s-p0 c) points)))
                      :lines lines
                      :curves curves
                      :contours contours)
       shapes)
      (let ((s (car shapes)))
        (loop for p in (points s)
              do (setf (prev s p) (p-e1 p))
                 (setf (next s (p-e1 p)) p)

                 (setf (next s p) (p-e2 p))
                 (setf (prev s (p-e2 p)) p)))
      (setf lines nil
            curves nil
            contours nil)
      (clrhash points))
    (when filter
      (setf shapes (mapcar filter shapes)))
    (make-instance 'shape
                   :points (reduce 'append shapes :key 'points :from-end t)
                   :lines (reduce 'append shapes :key 'lines :from-end t)
                   :curves (reduce 'append shapes :key 'curves :from-end t)
                   :contours (reduce 'append (reverse shapes)
                                     :key 'contours :from-end t))))

(defvar *f* (list nil))
(defun sdf (font glyph font-scale ms-scale spread)
  (declare (ignore ms-scale font))
  (let* ((scale font-scale)
         (gw (- (xmax glyph) (xmin glyph)))
         (gh (- (ymax glyph) (ymin glyph)))
         (padding spread)
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
                       for d = (dist/s (v2 fx fy) segments nil)
                       for dy = (- dh 1 y)
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
  (let ((*backend* :direct)
        (g (zpb-ttf:find-glyph
            (print (char "@|kSWA*OXI5" 0))
            ttf))
        (scale (print (float (/ 64 (- (zpb-ttf:ascender ttf)
                                      (zpb-ttf:descender ttf)))))))
    (time (msdf ttf g
               scale 64 3
               ;;(print (+ 10 (random 564))) (/ (print (+ 3 (random 10))))
               ))
    nil))
