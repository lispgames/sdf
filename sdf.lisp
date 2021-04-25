(in-package #:sdf/base)

;; render sdf/psdf from a SHAPE



(defun calculate-size (shape spread scale)
  (if (zerop (length (contours shape)))
      ;; not sure what this should do for empty shapes yet?
      (list 2 2)
      (let* ((bounds (bounding-box shape))
             (dx (- (aabb-x2 bounds) (aabb-x1 bounds)))
             (dy (- (aabb-y2 bounds) (aabb-y1 bounds)))
             (wx (/ dx scale))
             (wy (/ dy scale)))
        (list (ceiling (+ 1 wx (* spread 2)))
              (ceiling (+ 1 wy (* spread 2)))))))

(defun channels-for-type (type)
  (ecase type
    ;; return nil for 1 channel, since we don't have a 3rd dimension for that?
    ((:sdf :psdf) 1)))


(defun distance-sign-at-node (xy node)
  1)

(defun distance-to-shape (shape x y)
  (let ((best most-positive-double-float)
        (best-node nil)
        (xy (v2 x y))
        (i 0))
    #++(format t "distance from ~s:~%" xy)
    (labels ((mm (node dist)
               (when (and dist (< (abs dist) (abs best)))
                 (setf best dist
                       best-node node)))
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
               (mm node (dist node))
               (incf i))))
    #++(format t "distance from ~s = ~s:~%" xy best)
    best))

(defparameter *dump-mask* nil)
#++ (setf *dump-mask* t)
(defun mask-sdf (sdf x0 dx y0 dy)
  (let ((edges (make-array (array-dimension (image sdf) 0)
                           :initial-element nil))
        (shape (shape sdf))
        (image (image sdf)))
    (when *dump-mask*
      (format t "masking, ~s+~s, ~s+~s~%" x0 dx y0 dy)
      (format t " ~,2f,~,2f -> ~,2f,~,2f~%"
              x0 y0
              (+ x0 (* dx (array-dimension image 1)))
              (+ y0 (* dy (array-dimension image 0)))))

    (labels ((addp (p)
               (let* ((x (p-x p))
                      (y (p-y p))
                      (w (gethash p (windings shape))))
                 (when *dump-mask*
                   (format t "add pt: ~s ~s,~s, p=~s,n=~s~%"
                           w
                           (p-x p) (p-y p)
                           (let ((x (prev shape p)))
                             (etypecase x
                               (segment (s-p1 x))
                               (bezier2 (list (b2-yc x)
                                              (b2-y1 x)))))
                           (let ((x (next shape p)))
                             (etypecase x
                               (segment (s-p2 x))
                               (bezier2 (list (b2-yc x)
                                              (b2-y2 x)))))))
                 (when (or (eql w :ccw) (eql w :cw))
                   (multiple-value-bind (j rem) (floor (/ (- y y0) dy))
                     (when (zerop rem)
                       (push (list w x) (aref edges j)))))))
             (adds (s)
               ;; if edge is horizontal, ignore it, otherwise ignore
               ;; endpoints and let addp handle them so we don't add
               ;; twice
               (let* ((w (gethash s (windings (shape sdf))))
                      (j1 (/ (- (s-y1 s) y0) dy))
                      (j2 (/ (- (s-y2 s) y0) dy)))
                 (when *dump-mask*
                   (format t "add seg ~s, ~s,~s -> ~s,~s = ~s,~s~%"
                           w
                           (s-x1 s) (s-y1 s)
                           (s-x1 s) (s-y2 s)
                           (ceiling (min j1 j2))
                           (floor (max j1 j2))))
                 (loop with x1 = (s-x1 s)
                       with y1 = (s-y1 s)
                       with dx = (/ (- (s-x2 s) (s-x1 s))
                                    (- (s-y2 s) (s-y1 s)))
                       ;; be careful to start after first point and
                       ;; end before last point even if they are
                       ;; exactly on grid, since we want them added
                       ;; once by point code
                       for j from (1+ (floor (min j1 j2))) below (ceiling (max j1 j2))
                       for y = (+ y0 (* j dy))
                       for x = (+ x1 (* dx (- y y1)))
                       do (when (array-in-bounds-p edges j)
                            (push (list w x y :s) (aref edges j))))))
             (addb-1 (v1 vc v2 w)
               ;; todo: write this (currently draws as segment)
               (declare (ignore vc))
               (let* ((j1 (/ (- (vy v1) y0) dy))
                      (j2 (/ (- (vy v2) y0) dy)))
                 (when *dump-mask*
                   (format t "  add b1 ~s, ~s,~s -> ~s,~s = ~s,~s~%"
                           w
                           (vx v1) (vy v1)
                           (vx v2) (vy v2)
                           j1 j2))

                 (loop with x1 = (vx v1)
                       with y1 = (vy v1)
                       with dx = (/ (- (vx v2) (vx v1))
                                    (- (vy v2) (vy v1)))
                       ;; be careful to start after first point and
                       ;; end before last point even if they are
                       ;; exactly on grid, since we want them added
                       ;; once by point code
                       for j from (1+ (floor(min j1 j2))) below (ceiling (max j1 j2))
                       for y = (+ y0 (* j dy))
                       for x = (+ x1 (* dx (- y y1)))
                       do (when (array-in-bounds-p edges j)
                            (push (list w x :b1) (aref edges j))))))
             (addb (b)
               (let* ((w (gethash b (windings (shape sdf)))))
                 (when *dump-mask*
                   (format t "add b2 ~s: ~s,~s @ ~s,~s -> ~s,~s~%"
                           w
                           (b2-x1 b) (b2-y1 b)
                           (b2-xc b) (b2-yc b)
                           (b2-x2 b) (b2-y2 b)))
                 (ecase w
                   ((:cw :ccw)
                    (addb-1 (p-v (b2-p1 b)) (b2-c1 b) (p-v (b2-p2 b))
                            w))
                   (:both
                    #++(break"todo")
                    (addb-1 (p-v (b2-p1 b)) (b2-c1 b) (p-v (b2-p2 b))
                            (if (>= (b2-y2 b) (b2-y1 b)) :cw :ccw))))))
             (add (n)
               (typecase n
                 (point (addp n))
                 (segment (adds n))
                 (bezier2 (addb n)))))
      ;; build edge list
      (map-contour-segments
       (shape sdf)
       (lambda (c# node endp)
         (declare (ignore c# endp))
         (add node)))
      ;; sort it
      (loop for i below (length edges)
            do (when (aref edges i)
                 (setf (aref edges i)
                       (sort (aref edges i) '<
                             :key 'second))))
      (when *dump-mask*
        (format t "edges =~%")
        (loop for j below (length edges)
              for y = y0 then (+ y dy)
              for e = (aref edges j)
              for cw = (count :cw e :key 'car)
              for ccw = (count :ccw e :key 'car)
              do (format t "  ~s = ~s = ~s ~s ~s?~%" j y cw ccw (- cw ccw))
                 (when e (format t "     = ~s~%" e))))

      ;; convert it to in/out flags
      (loop with signs = (signs sdf)
            for e across edges
            for j from 0
            for count = 0
            for i = 0
            do (loop for (d ex) in e
                     do (loop for x = (+ x0 (* i dx))
                              while (and (< x ex)
                                         (< i (array-dimension signs 1)))
                              for in = (if (zerop count) 0 1)
                              do (setf (aref signs j i) in)
                                 (incf i))
                        (ecase d
                          ((:both nil)
                           ;; do nothing
                           )
                          (:cw (incf count))
                          (:ccw (decf count))))
               (loop with in = (if (zerop count) 0 1)
                     for x from i below (array-dimension signs 1)
                     do (setf (aref signs j x) in))))))

(defun render-sdf (sdf)
  (let* ((origin (origin sdf))
         (spread (spread sdf))
         (scale (pixel-scale sdf))
         (image (image sdf))
         (shape (shape sdf))
         (signs (signs sdf)))
    (time
     (mask-sdf sdf
               (* (- 0.5 (vx origin)) scale) scale
               (* (- (vy origin) 0.5) scale) (- scale)))
    (time
     (loop with wy = (array-dimension image 0)
           for j below wy
           ;; sample sdf at pixel centers
           for y = (* (- (vy origin) 0.5 j)
                      scale)
           do (loop for i below (array-dimension image 1)
                    for x = (* (- (+ i 0.5) (vx origin))
                               scale)
                    do (setf (aref image j i 0)
                             (float
                              (* (if (zerop (aref signs j i)) -1 1)
                                 (/ (distance-to-shape shape x y)
                                    (* scale spread)))
                              1.0)))))))


(defun make-sdf (type shape &key (spread 2.5) (scale 1) integer-offset)
  (when integer-offset
    (error "todo: integer-offset"))
  (setf scale (float scale 1d0))
  (destructuring-bind (wx wy) (calculate-size shape spread scale)
    (let* ((bounds (bounding-box shape))
           ;; find center of shape bounding box, in image units
           (cx (/ (+ (aabb-x1 bounds) (aabb-x2 bounds)) (* 2 scale)))
           (cy (/ (+ (aabb-y1 bounds) (aabb-y2 bounds)) (* 2 scale)))
           ;; find origin in image coords (0,0 = upper left corner of image)
           (ox (- (/ wx 2) cx))
           (oy (+ (/ wy 2) cy))
           (image (make-array (list* wy wx (a:ensure-list
                                            (channels-for-type type)))
                              :element-type 'single-float
                              :initial-element 0.0))
           (signs (make-array (list wy wx) :element-type 'bit
                                           :initial-element 0)))
      (format t "make image ~s x ~s x ~s~%" wx wy (channels-for-type type))
      (format t "bounds = ~s~%" (bounding-box shape))
      (format t "origin = ~s~%     ~s~%" (list ox oy) (list cx cy))
      #++(loop for i below (min wx wy)
               do (setf (aref image i i 0) 0.2))
      (loop for c across (sdf/base::contours shape)
            do (loop with n = c
                     do (typecase n
                          (sdf/base::point
                           (let* ((p (v2+ (v2scale  (v2h* (p-v n) (v2 1 -1))
                                                    (/ scale))
                                          (v2 ox oy)))
                                  (x (floor (vx p)))
                                  (y (floor (vy p))))
                             (when (array-in-bounds-p image y x 0)
                               (incf (aref image y x 0) 0.3)))))
                        (setf n (sdf/base::next shape n))
                     until (eql n c)))
      (let ((sdf (make-instance 'sdf :spread spread :sdf-type type
                                     :shape shape
                                     :pixel-scale scale :origin (v2 ox oy)
                                     :image image :signs signs)))
        (render-sdf sdf)
        sdf))))
