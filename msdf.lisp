(in-package #:sdf/base)

;; render msdf from a SHAPE

(defun corner-angles (shape)
  ;; return hash of point -> (signed angle . bisector)
  (let ((corner-angles (make-hash-table)))
    (flet ((t1 (n)
             (etypecase n
               (segment (v2- (p-dv (s-p2 n)) (p-dv (s-p1 n))))
               (bezier2 (v2- (p-dv (b2-c1 n)) (p-dv (b2-p1 n))))))
           (t2 (n)
             (etypecase n
               (segment (v2- (p-dv (s-p2 n)) (p-dv (s-p1 n))))
               (bezier2 (v2- (p-dv (b2-p2 n)) (p-dv (b2-c1 n))))))
           (at (t1 t2)
             (let* ((a1 (atan (vy t1) (vx t1)))
                    (a2 (atan (vy t2) (vx t2)))
                    (a (- a1 a2)))
               (cond
                 ((< a (- pi))
                  (+ a (* 2 pi)))
                 ((> a pi)
                  (- a (* 2 pi)))
                 (t a))))
           (b (t1 t2)
             (let* ((t1 (v2n t1))
                    (t2 (v2n t2))
                    (s (v2+ t1 t2)))
               (if (zerop (v2mag s))
                   t1
                   (v2n (v2rx s))))))
      (map-contour-segments shape
                            (lambda (c# n e)
                              (declare (ignore c# e))
                              (when (typep n 'point)
                                (let* ((p (prev shape n))
                                       (n2 (next shape n))
                                       (a (t2 p))
                                       (b (t1 n2))
                                       (aa (at a b)))
                                  (assert (not (gethash p corner-angles)))
                                  (setf (gethash n corner-angles)
                                        (cons aa (b a b))))))))
    corner-angles))

(defun collapse-short-edges (shape corner-angles min-length)
  ;; for any edges in shape estimated to be shorter than MIN-LENGTH,
  ;; add smaller of adjacent angles to other angle, and zero the
  ;; smaller one, and return hash of adjusted values

  ;; need signed angles, since a Z shape should collapse to a line if
  ;; the middle segment is removed, so adding +n and -n would give 0
  ;; angle
;;; todo
  corner-angles)

(defun assign-colors (shape corner-angles min-angle)
  ;; return hash of segment/bez -> color masks
  (let ((colors (make-hash-table))
        (i 2)
        (masks #((t t nil) (t nil t) (nil t t))))
    (flet ((set-point (n m)
             (assert (typep n 'point))
             (let ((old (gethash n colors)))
               (setf (gethash n colors)
                     (list (or (first m) (first old))
                           (or (second m) (second old))
                           (or (third m) (third old)))))))
      (map-contour-segments
       shape (lambda (c n e)
               (declare (ignore c))
               (unless (typep n 'point)
                 (setf (gethash n colors) (aref masks i))
                 (set-point (prev shape n) (aref masks i))
                 (set-point (next shape n) (aref masks i))

                 (format t "~s = ~s~%" n (aref masks i))
                 (cond
                   (e
                    (setf i 2))
                   ((> (abs (car (gethash (next shape n) corner-angles)))
                       min-angle)
                    (setf i (mod (1+ i) 2))))))))
    colors))

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
               #++(format t "  ~s~%" node)
               (progn                   ;unless (typep node 'point)
                 (mm node (dist node)))
               (incf i))))
    #++(format t "distance from ~s = ~s:~%" xy (list best r g b))
    (values re ge be a ae)))

(defun fix-msdf (image simage pimage)
  ;; compare msdf to sdf and psdf, removing extra channels from msdf
  ;; where it is obviously wrong
  (let ((wx (array-dimension image 1))
        (wy (array-dimension image 0)))
    (flet ((median (a b c)
             (max (min a b) (min (max a b) c))))
      (loop for j below wy
            do (loop for i below wx
                     for m = (median (aref image j i 0)
                                     (aref image j i 1)
                                     (aref image j i 2))
                     for s = (aref simage j i)
                     for p = (aref pimage j i)
                     when (> (abs (- p m)) #.(/ 1024.0))
                       do (setf (aref image j i 0) p
                                (aref image j i 1) p
                                (aref image j i 2) p))))))

(defvar *last-edge-colors* nil)
(defun render-sdf/msdf (sdf &key mtsdf)
  (let* ((spread (spread sdf))
         (scale (pixel-scale sdf))
         (image (image sdf))
         (simage (make-array (subseq (array-dimensions image) 0 2)
                             :element-type 'single-float :initial-element 0.0))
         (pimage (make-array (subseq (array-dimensions image) 0 2)
                             :element-type 'single-float :initial-element 0.0))
         (shape (cleaned-shape sdf))
         (corners1 (corner-angles shape))
         (min-length (min-sharp-edge-length sdf))
         (corners (if (and min-length (not (zerop min-length)))
                      (collapse-short-edges shape corners1 min-length)
                      corners1))
         (colors (assign-colors shape corners (min-angle sdf)))
         (samples/x (samples/x sdf))
         (samples/y (samples/y sdf))
         (signs (signs sdf))
         (edges (make-edge-list sdf))
         (facing (determine-facing shape edges)))
    (setf *last-edge-colors* colors)
    (labels ((scale (d sign)
               (coerce
                (* (if (zerop sign) -1 1)
                   (/ (min 1000 (max -1000 d)) (* scale spread)))
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
                   (gethash n facing))))
      (a:write-string-into-file
       (print
        (serialize-shape shape :allow-ratios nil :edge-colors colors))
       "c:/tmp/shapedesc1.txt"
       :if-exists :supersede)
      (uiop:run-program
       (print
        (format nil "msdfgen -autoframe -pxrange ~a -scale ~a -size ~a ~a -o c:/tmp/shapedesc1.png -testrender c:/tmp/shapedesc1-r.png 256 256  -shapedesc c:/tmp/shapedesc1.txt"
                (* 2 spread) (/ scale)
                (array-dimension image 1)
                (array-dimension image 0)))
       :output t)
      (a:write-string-into-file
       (print
        (serialize-shape shape :allow-ratios nil))
       "c:/tmp/shapedesc1nc.txt"
       :if-exists :supersede)
      (uiop:run-program
       (print
        (format nil "msdfgen -autoframe -pxrange ~a -scale ~a -size ~a ~a -o c:/tmp/shapedesc1nc.png -testrender c:/tmp/shapedesc1nc-r.png 256 256  -shapedesc c:/tmp/shapedesc1nc.txt"
                (* 2 spread) (/ scale)
                (array-dimension image 1)
                (array-dimension image 0)))
       :output t)

      (loop with wy = (array-dimension image 0)
            for j below wy
            for rs = 0
            for gs = 0
            for bs = 0
            for y across samples/y
            for edge-row across edges
            do #++(setf (values rs gs bs) (init-signs edge-row))
               (format t "at ~s: ~s ~s ~s~%" j rs gs bs)
               (loop for i below (array-dimension image 1)
                     for x across samples/x
                     do #++ (setf (values rs gs bs edge-row)
                                  (update-signs rs gs bs x edge-row))
                        (multiple-value-bind (re ge be a ae)
                            (distance-to-shape/rgb shape x y colors)
                          (if (and re ge be)
                              (let ((r (pseudo-distance x y re 0))
                                    (g (pseudo-distance x y ge 1))
                                    (b (pseudo-distance x y be 2)))

                                (setf (aref image j i 0)
                                      (scale r (facing re x y 0)))
                                (setf (aref image j i 1)
                                      (scale g (facing ge x y 1)))
                                (setf (aref image j i 2)
                                      (scale b (facing be x y 2))))
                              (progn
                                (setf (aref image j i 0)
                                      (scale (abs a) (aref signs j i)))
                                (setf (aref image j i 1)
                                      (scale (abs a) (aref signs j i)))
                                (setf (aref image j i 2)
                                      (scale (abs a) (aref signs j i)))
))
                          (setf (aref simage j i)
                                (scale (abs a) (aref signs j i)))
                          (setf (aref pimage j i)
                                (scale (abs (pseudo-distance x y ae nil))
                                       (aref signs j i)))
                          (when mtsdf
                            (setf (aref image j i 3)
                                  (scale (abs a) (aref signs j i)))))))
      (fix-msdf image simage pimage))))
