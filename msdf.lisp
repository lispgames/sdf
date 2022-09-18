(in-package #:sdf/base)

;; render msdf from a SHAPE

(defparameter *gen-ref* nil)
(defparameter *limit-msdf-range* t)

(defun collapse-short-edges (shape corner-angles min-length min-angle)
  ;; for any edges in shape estimated to be shorter than MIN-LENGTH
  ;; that are adjacent to a corner, add smaller of adjacent angles to
  ;; other angle, and zero the smaller one, and return hash of
  ;; adjusted values

  ;; for short curves, also add the angle between the ends of the
  ;; bezier, so a for example rounded L shape with 0 angle at either
  ;; end of curve would end up with a non-zero angle. (if short curve
  ;; has same angle at both sides, add to whichever end is closest to
  ;; control point)

  ;; need signed angles, since a Z shape should collapse to a line if
  ;; the middle segment is removed, so adding +n and -n would give 0
  ;; angle

  ;; possibly should try to do these in order by ascending length and
  ;; skip any groups that accumulate enough to cross min-length?
  ;; though shapes with lots of adjacent tiny segments/curves will
  ;; probably do odd things anyway
  (map-contour-segments
   shape
   (lambda (c n e)
     (declare (ignore c e))
     (etypecase n
       (point)
       (segment
        (let* ((l (v2dist (p-dv (s-p1 n)) (p-dv (s-p2 n))))
               (p1 (prev shape n))
               (p2 (next shape n))
               (a1 (car (gethash p1 corner-angles)))
               (a2 (car (gethash p2 corner-angles)))
               (to (if (< (abs a1) (abs a2)) p2 p1))
               (from (if (< (abs a1) (abs a2)) p1 p2)))
          (when (and (< l min-length)
                     (and (> (abs a1) min-angle)
                          (> (abs a2) min-angle)))
            (incf (car (gethash to corner-angles))
                  (car (gethash from corner-angles)))
            (setf (car (gethash from corner-angles)) 0.0))))
       (bezier2
        ;; just guess halfway between length of p1-p2 and p1-pc-p2
        #++
        (let ((l (/ (+ (v2dist (p-dv (b2-p1 n)) (p-dv (b2-pc n)))
                       (v2dist (p-dv (b2-pc n)) (p-dv (b2-p2 n)))
                       (v2dist (p-dv (b2-p1 n)) (p-dv (b2-p2 n))))
                    2)))
          (when (< l min-length)
            (setf (gethash n edge-lens) l)
            (push n short-edges)))))))
  corner-angles)

(defun assign-colors (shape corner-angles min-angle)
  ;; return hash of segment/bez -> color masks
  (let ((colors (make-hash-table))
        (groups (make-hash-table))
        (i 2)
        (group 2)
        (masks #((t t nil) (t nil t) (nil t t))))
    (flet ((set-point (n m)
             (assert (typep n 'point))
             (let ((old (gethash n colors)))
               (if (and (gethash n groups)
                        (/= (gethash n groups) group))
                   (setf (gethash n groups) group) ;; 0
                   (setf (gethash n groups) group))
               (setf (gethash n colors)
                     (list (or (first m) (first old))
                           (or (second m) (second old))
                           (or (third m) (third old)))))))
      ;; assign colors
      (map-contour-segments
       shape (lambda (c n e)
               (declare (ignore c))
               (unless (typep n 'point)
                 (setf (gethash n colors) (aref masks i))
                 (setf (gethash group groups) (aref masks i))
                 (setf (gethash n groups) group)
                 (set-point (prev shape n) (aref masks i))
                 (set-point (next shape n) (aref masks i))

                 #++(format t "~s = ~s~%" n (aref masks i))
                 (cond
                   (e
                    (incf group)
                    (setf i 2))
                   ((> (abs (car (gethash (next shape n) corner-angles)))
                       min-angle)
                    (incf group)
                    (setf i (mod (1+ i) 2)))))))
      ;; fix teardrops
      (map-contour-segments
       shape (lambda (c n e)
               (declare (ignore c e))
               ;; if we have a point that is a corner and has same color
               ;; assignment on both sides, it should be the only corner
               ;; of the contour. In that case, we should assign a
               ;; different color to part of the contour to preserve the
               ;; corner/
               (when (and (typep n 'point)
                          (> (abs (car (gethash n corner-angles)))
                             min-angle)
                          (eql (gethash (prev shape n) colors)
                               (gethash (next shape n) colors)))
                 #++(format t "------------- split teardrop~%")
                 (let* ((n2 (prev2 shape n))
                        (d2 (v2dist (p-dv n2) (p-dv n)))
                        (c1 (gethash (prev shape n) colors)))
                   #++(format t " start @ ~s~%" n)
                   #++(format t "  n2 = ~s~%   @ ~s~%" n2 d2)
                   (loop for x = (prev2 shape n2) then (prev2 shape x)
                         for e = (next shape x)
                         ;; if we have multiple colors on contour, but
                         ;; same colors at a corner, something is wrong
                         do (assert (equalp (gethash e colors) c1))
                            ;; X should be iterating POINTs, so if E is
                            ;; our original POINT, something is wrong
                            (assert (not (eql e n)))
                         #++(format t " -check ~s~%" x)
                         until (eql x n)
                         do (let ((d3 (v2dist (p-dv x) (p-dv n))))
                              ;; pick point on contour furthest from
                              ;; corner
                              (when (> d3 d2)
                                (setf n2 x
                                      d2 d3))))
                   (assert (not (eql d2 n)))
                   ;; split at furthest point
                   (incf group)
                   #++(format t "splitting at ~s~%" n2)
                   (loop with i = (mod (1+ (or (position c1 masks) 2)) 3)
                         for x = n2 then (next2 shape x)
                         for e = (next shape x)
                         until (eql x n)
                         do (setf (gethash e colors) (aref masks i))
                            (setf (gethash group groups) (aref masks i))
                            (setf (gethash e groups) group)
                            (set-point (prev shape e) (aref masks i))
                            (set-point (next shape e) (aref masks i))))))))
    (list colors groups)))

(defun determine-facing (shape edges)
  ;; figure out which way each contour in shape is oriented, so we can
  ;; calculate signed distances for partial contours (returns hash of
  ;; node -> +-1, to be multiplied by sign calculated by
  ;; pseudo-distance function)
  (let ((facing (make-hash-table)))
    (map-contour-segments
     shape (lambda (c n e)
             (declare (ignore c e))
             (unless (typep n 'point)
               (setf (gethash n facing) 1))))
    edges
    facing))

#++
(defun distance-to-shape/rgb (shape x y colors)
  ;; return as 3 values, nearest nodes of SHAPE for each color, and
  ;; actual distance as 4th value
  (let ((r most-positive-double-float)
        (re nil)
        (g most-positive-double-float)
        (ge nil)
        (b most-positive-double-float)
        (be nil)
        (a most-positive-double-float)
        (ae nil)
        (xy (v2 x y))
        (i 0))
    (labels ((mm (node dist)
               (destructuring-bind (rm gm bm) (gethash node colors)
                 (when dist
                   (let ((adist (abs dist)))
                     (when (< (abs dist) (abs a))
                       (setf a dist
                             ae node))
                     (when (and rm (< adist r))
                       (setf r adist
                             re node))
                     (when (and gm (< adist g))
                       (setf g adist
                             ge node))
                     (when (and bm (< adist b))
                       (setf b adist
                             be node))))))
             (dist (node)
               (etypecase node
                 (point
                  (dist/v2-point/sf xy node))
                 (segment
                  (dist/v2-segment/sf xy node))
                 (bezier2
                  (dist/v2-bezier2/sf xy node))
                 (null))))
      (map-contour-segments
       shape (lambda (c# node endp)
               (declare (ignorable c# endp))
               (progn                   ;unless (typep node 'point)
                 (mm node (dist node)))
               (incf i))))
    (values re ge be a ae)))


(defun %distance-to-shape/rgb (x y points segments curves)
  (declare (optimize speed))
  ;; return as 3 values, nearest nodes of SHAPE for each color, and
  ;; actual distance as 4th value
  (let ((r most-positive-single-float)
        (re nil)
        (g most-positive-single-float)
        (ge nil)
        (b most-positive-single-float)
        (be nil)
        (a most-positive-single-float)
        (ae nil)
        (amax most-positive-single-float)
        (xy (v2 x y)))
    (declare (type single-float r g b a amax))
    (labels ((mm (node dist rm gm bm)
               (declare (type (or null single-float) dist))
               (when dist
                 (let ((adist (abs dist))
                       (c nil))
                   (when (< adist (abs a))
                     (setf a dist
                           c t
                           ae node))
                   (when (and rm (< adist r))
                     (setf r adist
                           c t
                           re node))
                   (when (and gm (< adist g))
                     (setf g adist
                           c t
                           ge node))
                   (when (and bm (< adist b))
                     (setf b adist
                           c t
                           be node))
                   (when c
                     (setf amax (max (abs a) r g b)))))))
      (loop for p in points
            do (mm (pb-n p) (dist/v2-point/sf xy p)
                   (pb-rm p) (pb-gm p) (pb-bm p)))
      (loop for s in segments
            when (<= (v2dist xy (sb-bc s))
                     (+ amax (sb-br s)))
              do (mm (sb-n s) (dist/v2-segment/sf xy s)
                     (sb-rm s) (sb-gm s) (sb-bm s)))
      (loop for c in curves
            when (<= (v2dist xy (b2b-bc c))
                     (+ amax (b2b-br c)))
              do (mm (b2b-n c) (dist/v2-bezier2/sf xy c)
                     (b2b-rm c) (b2b-gm c) (b2b-bm c))))
    (values re ge be a ae)))

(defun %shape-to-parts-bounds/rgb (shape colors)
  (let ((points nil)
        (segments nil)
        (curves nil))
    (map-contour-segments
     shape (lambda (c# node endp)
             (declare (ignorable c# endp))
             (destructuring-bind (rm gm bm) (gethash node colors)
               (etypecase node
                 (point
                  (push (pointb node rm gm bm) points))
                 (segment
                  (push (segmentb node rm gm bm) segments))
                 (bezier2
                  (push (bezier2b node rm gm bm) curves))))))
    (values points segments curves)))

(defvar *last-edge-colors* nil)
(defvar *last-edge-groups* nil)
(defvar *last-sample-colors* nil)
(defun render-sdf/msdf (sdf &key mtsdf)
  (let* ((spread (spread sdf))
         (scale (pixel-scale sdf))
         (x1 (* scale (- (aref (origin sdf) 0) 1/2)))
         (y1 (* scale (- (aref (origin sdf) 1) 1/2)))
         (dx scale)
         (dy scale)
         (image (image sdf))
         (simage (make-array (subseq (array-dimensions image) 0 2)
                             :element-type 'single-float :initial-element 0.0))
         (pimage (make-array (subseq (array-dimensions image) 0 2)
                             :element-type 'single-float :initial-element 0.0))
         (sample-colors (make-array (array-dimensions image)
                                    :element-type 'fixnum :initial-element 0))
         (shape (simplified-shape sdf))
         (corners1 (corner-angles shape))
         (min-length (min-sharp-edge-length sdf))
         (corners (if (and min-length (not (zerop min-length)))
                      (collapse-short-edges shape corners1 min-length
                                            (min-angle sdf))
                      corners1))
         (tmp (assign-colors shape corners (min-angle sdf)))
         (colors (first tmp))
         (groups (second tmp))
         (samples/x (samples/x sdf))
         (samples/y (samples/y sdf))
         (signs (signs sdf))
         (edges/y (%make-edge-list shape samples/y))
         (facing (determine-facing shape edges/y))
         (edge-cells (make-array (array-dimensions pimage)
                                 :initial-element nil))
         (corner-cells (make-array (array-dimensions pimage)
                                   :initial-element nil)))

    (setf *last-edge-colors* colors)
    (setf *last-edge-groups* groups)
    (setf *last-sample-colors* sample-colors)

    (labels ((scale (d sign)
               (coerce
                (* (if (zerop sign) -1 1)
                   (/ (min 1000 (max -1000 d)) scale))
                'single-float))
             (select-edge-for-point-and-color (n x y c)
               (assert (typep n 'point))
               (let* ((d (v2- (v2 x y) (p-dv n)))
                      (s (- (v2x d (cdr (gethash n corners)))))
                      (prev (prev shape n))
                      (pmask (gethash prev colors))
                      (next (next shape n))
                      (nmask (gethash next colors)))
                 (cond
                   ((and (plusp s) (or (not c) (nth c pmask)))
                    prev)
                   ((and (not (plusp s)) (or (not c) (nth c nmask)))
                    next)
                   ((nth c pmask)
                    prev)
                   ((nth c nmask)
                    next)
                   ((equalp pmask nmask)
                    ;; not a sharp corner, just pick one (happens when
                    ;; we split a contour with only 1 corner)
                    prev)
                   (t (break "??")))))
             (pseudo-distance (x y n c)
               (etypecase n
                 (point
                  (pseudo-distance x y
                                   (select-edge-for-point-and-color n x y c)
                                   c))
                 (segment
                  (nth-value 1 (dist/v2-segment/sf* (v2 x y) n)))
                 (bezier2
                  (dist/v2-bezier2/sf* (v2 x y) n))))
             (facing (n x y c)
               (if (typep n 'point)
                   (facing (select-edge-for-point-and-color n x y c)
                           x y c)
                   (gethash n facing)))
             (assign-corners (j i n c)
               (unless (typep n 'point)
                 (return-from assign-corners n))
               (let ((normal (point-normal shape n))
                     (xy (v2- (v2 (aref samples/x i)
                                  (aref samples/y j))
                              (p-dv n)))
                     (pn (prev shape n))
                     (nn (next shape n)))
                 (if (plusp (v2x normal xy))
                     (if (nth c (gethash pn colors))
                         (prev shape n) (next shape n))
                     (if (nth c (gethash nn colors))
                         (next shape n) (prev shape n))))))

      (when *gen-ref*
        (time
         (progn
           (a:write-string-into-file
            (serialize-shape shape :allow-ratios nil :edge-colors colors)
            "c:/tmp/shapedesc1.txt"
            :if-exists :supersede)
           (uiop:run-program
            (print
             (format nil "/msys64/usr/bin/time msdfgen msdf -autoframe -pxrange ~a -scale ~a -size ~a ~a -o c:/tmp/shapedesc1.png -testrender c:/tmp/shapedesc1-r.png 256 256  -shapedesc c:/tmp/shapedesc1.txt " ;-errorcorrection auto-fast
                     (* 2 spread) (float (/ scale))
                     (array-dimension image 1)
                     (array-dimension image 0)))
            :output t
            :error-output t)
           (uiop:run-program
            (print
             (format nil "msdfgen -autoframe -pxrange ~a -scale ~a -size ~a ~a -o c:/tmp/shapedesc1ne.png -testrender c:/tmp/shapedesc1ne-r.png 256 256  -shapedesc c:/tmp/shapedesc1.txt -errorcorrection 0"
                     (* 2 spread) (float (/ scale))
                     (array-dimension image 1)
                     (array-dimension image 0)))
            :output t
            :error-output t)
           (a:write-string-into-file
            (serialize-shape shape :allow-ratios nil)
            "c:/tmp/shapedesc1nc.txt"
            :if-exists :supersede)
           (uiop:run-program
            (print
             (format nil "/msys64/usr/bin/time msdfgen  -autoframe -pxrange ~a -scale ~a -size ~a ~a -o c:/tmp/shapedesc1nc.png -testrender c:/tmp/shapedesc1nc-r.png 256 256  -shapedesc c:/tmp/shapedesc1nc.txt -printmetrics"
                     (* 2 spread) (float (/ scale))
                     (array-dimension image 1)
                     (array-dimension image 0)))
            :output t
            :error-output t)))
        (terpri))
      (multiple-value-bind (points segments curves) (%shape-to-parts-bounds/rgb shape colors)
        (loop with wy = (array-dimension image 0)
              for j below wy
              for rs = 0
              for gs = 0
              for bs = 0
              for y across samples/y
              for edge-row across edges/y
              do (loop for i below (array-dimension image 1)
                       for x across samples/x
                       do (multiple-value-bind (re ge be a ae)
                              (%distance-to-shape/rgb
                               x y points segments curves)
                              #++(distance-to-shape/rgb shape x y colors)
                            (if (and re ge be)
                                (let ((r (pseudo-distance x y re 0))
                                      (g (pseudo-distance x y ge 1))
                                      (b (pseudo-distance x y be 2)))
                                  (when (typep re 'point)
                                    (setf re (assign-corners j i re 0)))
                                  (when (typep ge 'point)
                                    (setf ge (assign-corners j i ge 1)))
                                  (when (typep be 'point)
                                    (setf be (assign-corners j i be 2)))

                                  (setf (aref image j i 0)
                                        (scale r (facing re x y 0)))
                                  (setf (aref image j i 1)
                                        (scale g (facing ge x y 1)))
                                  (setf (aref image j i 2)
                                        (scale b (facing be x y 2)))


                                  (setf (aref sample-colors j i 0)
                                        (gethash re groups))
                                  (setf (aref sample-colors j i 1)
                                        (gethash ge groups))
                                  (setf (aref sample-colors j i 2)
                                        (gethash be groups)))
                                (progn
                                  (setf (aref image j i 0)
                                        (scale (abs a) (aref signs j i)))
                                  (setf (aref image j i 1)
                                        (scale (abs a) (aref signs j i)))
                                  (setf (aref image j i 2)
                                        (scale (abs a) (aref signs j i)))

                                  (setf (aref sample-colors j i 0) 0)
                                  (setf (aref sample-colors j i 1) 0)
                                  (setf (aref sample-colors j i 2) 0)))
                            (setf (aref simage j i)
                                  (scale (abs a) (aref signs j i)))
                            (setf (aref pimage j i)
                                  (scale (abs (pseudo-distance x y ae nil))
                                         (aref signs j i)))
                            (when mtsdf
                              (setf (aref image j i 3)
                                    (scale (abs a) (aref signs j i))))))))

      (labels ((i (x)
                 (let ((i (floor (/ (+ x x1) dx))))
                   (cond
                     ((<= x (aref samples/x i))
                      (1- i))
                     ((and (> x (aref samples/x i))
                           (<= x (aref samples/x (1+ i))))
                      i)
                     ((> x (aref samples/x (1+ i)))
                      (+ i 1))
                     (t #++ (< y (aref samples/y (1+ j)))
                        (break "a")
                        #++(- (length samples/x) 2)))))
               (j (y)
                 (let ((j (floor (/ (+ y y1) dy))))
                   (cond
                     ((<= y (aref samples/y j))
                      (1- j))
                     ((and (> y (aref samples/y j))
                           (<= y (aref samples/y (1+ j))))
                      j)
                     ((> y (aref samples/y (1+ j)))
                      (+ j 1))
                     (t (break "b")))))
               (eval-at (n at)
                 (etypecase n
                   (segment (eval-at/s/fast n at))
                   (bezier2 (eval-at/b2/fast n at))))
               (ij= (a b)
                 (and (= (car a) (car b))
                      (= (cdr a) (cdr b))))
               (ij~ (a b)
                 (or (and (= (car a) (car b))
                          (<= (1- (cdr b)) (cdr a) (1+ (cdr b))))
                     (and (= (cdr a) (cdr b))
                          (<= (1- (car b)) (car a) (1+ (car b))))))
               (flat (i j)
                 (= (signum (aref pimage j i))
                    (signum (aref pimage (1+ j) i))
                    (signum (aref pimage j (1+ i)))
                    (signum (aref pimage (1+ j) (1+ i)))))
               (flag (i j)
                 (when (flat i j)
                   (setf (aref edge-cells j i) t)))
               (rsubdivide (n start end sij eij &optional (d 0))
                 (when (> d 26)
                   (progn
                     (flag (car sij) (cdr sij))
                     (flag (car eij) (cdr eij)))
                   (return-from rsubdivide t))
                 (if (ij~ sij eij)
                     (progn
                       (flag (car sij) (cdr sij))
                       (flag (car eij) (cdr eij)))

                     (let* ((mat (/ (+ start end) 2.0))
                            (mid (eval-at n mat))
                            (mij (cons (i (vx mid)) (j (vy mid)))))
                       (declare (dynamic-extent mij))
                       (unless (or (= mat start) (= mat end))
                         (unless (ij= mij sij)
                           (rsubdivide n start mat sij mij (incf d)))
                         (unless (ij= mij eij)
                           (rsubdivide n mat end mij eij (incf d)))))))
               (subdivide (n)
                 (let* ((sxy (eval-at n 0))
                        (sij (cons (i (vx sxy)) (j (vy sxy))))
                        (exy (eval-at n 1))
                        (eij (cons (i (vx exy)) (j (vy exy)))))
                   (declare (dynamic-extent sij eij))
                   (rsubdivide n 0.0 1.0 sij eij))))
        (declare (inline eval-at))
        (map-contour-segments shape
                              (lambda (c# n e)
                                (declare (ignore c# e))
                                (etypecase n
                                  (point
                                   (flag (i (p-dx n)) (j (p-dy n)))
                                   (when (> (abs (car (gethash n corners)))
                                            (min-angle sdf))
                                     (setf (aref corner-cells
                                                 (j (p-dy n))
                                                 (i (p-dx n)))
                                           t)))
                                  ((or segment bezier2)
                                   (subdivide n))))))

      (fix-msdf image pimage corner-cells)

      (when *limit-msdf-range*
        (loop for j below (array-dimension image 0)
              do (loop for i below (array-dimension image 01)
                       for p = (aref pimage j i)
                       do (loop for c below 3
                                for s = (aref image j i c)
                                when (>= (abs p) (- spread 0.0))
                                  do (setf (aref image j i c) p)))))

      (loop for j below (array-dimension image 0)
            do (loop for i below (array-dimension image 01)
                     for p = (aref pimage j i)
                     do (loop for c below (if mtsdf 4 3)
                              do (setf (aref image j i c)
                                       (/ (aref image j i c) spread))))))))
