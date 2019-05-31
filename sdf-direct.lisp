(in-package #:sdf)

(require 'sb-cga)

(defclass point ()
  ((p0 :accessor p0 :initarg :p0)
   (d :accessor d :initarg :d)))
(defmethod c ((p point))
  (p0 p))
(defmethod r ((p point))
  0.0)

(defun make-point (p c1 c2)
  (let* ((t1 (tangent-at-end c1 p))
         (t2 (tangent-at-end c2 p))
         (d (v2+ t1 t2))
         (s (if (minusp (v2x (tangent-at-end c1 p)
                             (tangent-at-end c2 p)))
                -1 1))
         (l (v2mag d)))
    (unless (or (and (equalp p (p0 c1))) (equalp p (p2 c2))
                (and (equalp p (p2 c1))) (equalp p (p0 c2)))
      (format t "~s: ~s ~s~%" p (p0 c1) (equalp p (p0 c1)))
      (format t "~s: ~s ~s~%" p (p2 c1) (equalp p (p2 c1)))
      (format t "~s: ~s ~s~%" p (p0 c2) (equalp p (p0 c2)))
      (format t "~s: ~s ~s~%" p (p2 c2) (equalp p (p2 c2)))
      (break "br,sd"))
    (if (zerop l)
        (if (equalp p (p0 c1))
            (setf d (v2 (vy t1) (- (vx t1))))
            (setf d (v2 (vy t2) (- (vx t2)))))
        (setf d (v2scale
                 d
                 (* s (/ (if (equalp p (p0 c1))
                             (- l)
                             l))))))
    (make-instance 'point
                   :p0 p
                   :d d)))

(defmethod sign-at ((p point) at n)
  (if (minusp at)
      -1 1))

(defclass line ()
  ((p0 :accessor p0 :initarg :p0)
   (p2 :accessor p2 :initarg :p2)
   (d :accessor d :initarg :d)
   (l :accessor l :initarg :l)
   (c :accessor c :initarg :c)
   (r :accessor r :initarg :r)))

(defmethod eval-at ((line line) at)
  (v2+ (p0 line) (v2scale (d line) at)))

(defmethod tangent-at-end ((line line) at)
  (cond
    ((equalp at (p0 line))
     (d line))
    ((equalp at (p2 line))
     (v2- (p0 line) (p2 line)))
    (t
     (error "not endpoint"))))

(defun make-line (p0 p2)
  (let ((d (v2- p2 p0))
        (c (v2scale (v2+ p0 p2) 0.5)))
    (make-instance 'line :p0 p0 :p2 p2 :d d :l (v2. d d)
                         :c c :r (v2dist p0 c))))

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
   (r :accessor r :initarg :R)))

(defun make-quadratic (p0 p1 p2)
  (let ((d1 (v2- p1 p0))
        (d2 (v2- p2 (v2- (v2scale p1 2) p0)))
        (c (v2scale (v2+ (v2+ p0 p1) p2) 1/3)))
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
                                      (v2dist c p2)))))

(defmethod eval-at ((q quadratic) at)
  (v2+ (v2+ (v2scale (d2 q) (* at at))
            (v2scale (d1 q) (* 2 at)))
       (p0 q)))

(defmethod tangent-at-end ((q quadratic) at)
  (cond
    ((equalp at (p0 q))
     (d1 q))
    ((equalp at (p2 q))
     (v2- (p1 q) (p2 q)))
    (t
     (error "not endpoint"))))

(defmethod sign-at ((q quadratic) at n)
  ;; separate sign calc so we don't need to calculate it if we know it
  ;; is too far away anyway
  (let ((db (v2+ (v2scale (p2-2p1+p0 q) (* 2 at))
                 (|2d1| q))))
    (if (plusp (v2x db n)) 1 -1)))


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
;;;(format t "bc ~s~%" bc)
;;;(format t "ad ~s~%" ad)
;;;(format t "abc ~s~%" abc)
;;;(format t "abcd ~s~%" abcd)
;;;(format t "b3 ~s~%" b3)
;;;(format t "det ~s~%" det)
;;;(format t "det0 ~s~%" det0)
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
               when (and (< (abs (imagpart r)) 0.1)
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
    (* (sign-at s tt 0)
       (v2dist p (p0 s)))))

(defun dist/s (p s)
  (etypecase s
    (point (dist/point p s))
    (quadratic (dist/curve p s))
    (line (dist/line p s))))

(defun cull (s p max)
  (unless (typep s 'point)
    (let ((c (> (- (v2dist (c s) p) (r s))
                (abs max))))
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
  ((points :reader points :initarg :points)
   (lines :reader lines :initarg :lines)
   (curves :reader curves :initarg :curves)
   (qtree :reader qtree :initarg :qtree)))

(defun dist/c (p shape)
  (let ((d most-positive-single-float))
    (loop for s in (append (points shape)
                           (lines shape)
                           (curves shape))
          for dp = (unless (cull s p d)
                     (dist/s p s))
          when (complexp dp)
            do (break "complex ~s?" dp)
          do (when (and dp (< (abs dp) (abs d)))
               (setf d dp)))
    d)
  #++
  (let* ((pd 10000)
         (ld 10000)
         (cd 10000))
    (loop for s in (points shape)
          for dp = (unless (cull s p pd)
                     (dist/s p s))
          do (when (and dp (< (abs dp) (abs pd)))
               (setf pd dp)))
    (loop for s in (lines shape)
          for dp = (unless (cull s p ld #++(min pd ld))
                     (dist/line p s))
          do (when (and dp (< (abs dp) (abs ld)))
               (setf ld dp)))
    (loop for s in (curves shape)
          for dp = (unless (cull s p cd #++(min ld pd cd))
                     (dist/curve p s))
          do (when (and dp (< (abs dp) (abs cd)))
               (setf cd dp)))
    (list pd ld cd)))

(defun scale-point (p s)
  (when p
    (v2 (* s (zpb-ttf:x p))
        (* s (zpb-ttf:y p)))))

(defun translate-glyph (glyph scale)
  (let ((points (make-hash-table :test 'equalp))
        (lines)
        (curves))
    (zpb-ttf:do-contours (c glyph)
      (zpb-ttf:do-contour-segments (p0 p1 p2) c
        (let ((p0 (scale-point p0 scale))
              (p1 (scale-point p1 scale))
              (p2 (scale-point p2 scale)))
          (let ((s (if p1
                       (make-quadratic p0 p1 p2)
                       (make-line p0 p2))))
            (if p1
                (push s curves)
                (push s lines))
            (push s (gethash p0 points))
            (push s (gethash p2 points))))))
    (make-instance 'shape
                   :points
                   (loop for k being the hash-key of points using (hash-value v)
                         do (assert (= (length v) 2))
                         collect (apply 'make-point k (reverse v)))
                   :lines lines
                   :curves curves)))

(defvar *f*)
(defun sdf (font glyph scale spread)
  (let* ((gw (- (xmax glyph) (xmin glyph)))
         (gh (- (ymax glyph) (ymin glyph)))
         (padding (ceiling (* 1/2 spread scale (units/em font))))
         (dw (+ (* 2 padding) (ceiling (* scale gw))))
         (dh (+ (* 2 padding) (ceiling (* scale gh)))))
    (let* ((segments nil))
      (setf segments (translate-glyph glyph scale))
      (setf *f* segments)
      (let* ((dest (aa-misc:make-image dw dh #(0 0 0)))
             (write (aa-misc:image-put-pixel dest #(255 255 255))))
        (declare (ignorable write))
        (loop for y below (array-dimension dest 0)
              do (loop for x below (array-dimension dest 1)
                       for fx = (- x (- (/ (- dw (* scale gw)) 2)
                                        (* (xmin glyph) scale)))
                       for fy = (- y (- (/ (- dh (* scale gh)) 2)
                                        (* (ymin glyph) scale)))
                       for d = (dist/c (v2 fx fy) segments)
                       do (funcall write x (- dh y)
                                   (max 0 (+ 128 (* 128 (/ d 12)))))
                       #++
                        (let ((ex 467) (ey 483))
                          (when (and (<= (1- ex) x (1+ ex))
                                     (<= (1- ey) (- dh y 1) (1+ ey)))
                            (format t "~s ~s: ~s ~s ~s~%" x y d fx fy)
                            #++(when (and (= x ex) (= (- dh y 1) ey))
                                 (setf d (list 999 999 999)))))
                       #++
                        (progn
                          (when (< (car d) 1000)
                            (setf (aref dest (- dh y 1) x 0)
                                  (min 255 (max 0 (round (+ 128 (* 128 (/ (car d) 12))))))))
                          #++
                          (when (< (second d) 1000)
                            (setf (aref dest (- dh y 1) x 1)
                                  (min 255 (max 0 (round (+ 128 (* 128 (/ (second d) 12))))))))
                          #++
                          (when (< (third d) 1000)
                            (setf (aref dest (- dh y 1) x 2)
                                  (min 255
                                       (max 0 (round (+ 128 (* 128 (/ (third d) 12)))))))))
                       #++
                        (funcall write x (- dh y 1)
                                 (max 0 (+ 128 (* 128
                                                  (/ (reduce
                                                      (lambda (a b)
                                                        (if (< (abs a)
                                                               (abs b))
                                                            a b))
                                                      d)
                                                     12)))))))
        (aa-misc:save-image "/tmp/font2a.pnm" dest :pnm)
        #++(aa-misc:save-image "/tmp/font2h.pnm" image :pnm)
        (values dest padding)))))


#++
(require 'sdf)

#++
(zpb-ttf:with-font-loader (ttf "georgia.ttf")
  (let ((g (zpb-ttf:find-glyph
            (char "WA*SOX" 5)
            ;;(alexandria:random-elt *default-characters*)
            ttf)))
    (time (sdf ttf g 0.119108185 #++(print (+ 0.01 (random 0.3))) 0.6))
    nil))
