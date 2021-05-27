(in-package #:sdf/base)

;; render sdf/psdf from a SHAPE

(defparameter *dump-mask* nil)
#++ (setf *dump-mask* t)


(defun calculate-size (shape spread scale)
  (if (zerop (length (contours shape)))
      ;; not sure what this should do for empty shapes yet?
      (list 2 2)
      (let* ((bounds (rbounding-box shape))
             (dx (- (raabb-x2 bounds) (raabb-x1 bounds)))
             (dy (- (raabb-y2 bounds) (raabb-y1 bounds)))
             (wx (/ dx scale))
             (wy (/ dy scale)))
        (when *dump-mask*
          (format t "calculate size ~s, ~s: ~% ~s~% = ~s~%"
                  spread scale
                  bounds
                  (list (ceiling (+ 1 wx (* spread 2)))
                        (ceiling (+ 1 wy (* spread 2))))))
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
    (values best best-node)))

(defun render-sdf/sdf (sdf)
  (let* ((spread (spread sdf))
         (scale (pixel-scale sdf))
         (image (image sdf))
         (shape (cleaned-shape sdf))
         (signs (signs sdf)))
    (loop with wy = (array-dimension image 0)
          for j below wy
          for y across (samples/y sdf)
          do (loop for i below (array-dimension image 1)
                   for x across (samples/x sdf)
                   do (setf (aref image j i 0)
                            (float
                             (* (if (zerop (aref signs j i)) -1 1)
                                (/ (distance-to-shape shape x y)
                                   (* scale spread)))
                             1.0))))))


(defun render-sdf/psdf (sdf)
  (let* ((spread (spread sdf))
         (scale (pixel-scale sdf))
         (image (image sdf))
         (shape (cleaned-shape sdf))
         (signs (signs sdf)))
    (loop with wy = (array-dimension image 0)
          for j below wy
          for y across (samples/y sdf)
          do (loop for i below (array-dimension image 1)
                   for x across (samples/x sdf)
                   for d = (multiple-value-bind (d n)
                               (distance-to-shape shape x y)
                             (typecase n
                               (point
                                (let ((xy (v2 x y)))
                                  (flet ((d (node dir)
                                           (etypecase node
                                             (segment
                                              (dist/v2-line/sf xy node))
                                             (bezier2
                                              (if dir
                                                  (dist/v2-line*/sf
                                                   xy
                                                   (p-dv (b2-p1 node))
                                                   (p-dv (b2-c1 node)))
                                                  (dist/v2-line*/sf
                                                   xy
                                                   (p-dv (b2-c1 node))
                                                   (p-dv (b2-p2 node))))))))
                                    (let* ((next (next shape n))
                                           (prev (prev shape n))
                                           (a (d prev nil))
                                           (b (d next t)))
                                      (max (abs a) (abs b))))))
                               (t d)))
                   do (setf (aref image j i 0)
                            (float
                             (* (if (zerop (aref signs j i)) -1 1)
                                (/ d
                                   (* scale spread)))
                             1.0))))))

(defun render-sdf (sdf &key (render t))
  (when render
   (ecase (sdf-type sdf)
     (:sdf (render-sdf/sdf sdf))
     (:psdf (render-sdf/psdf sdf)))))

(defun make-mask (wx wy sx sy edge-list)
  (declare (ignorable sx sy))
  (let ((mask (make-array (list wy wx) :element-type 'bit :initial-element 0)))
    (when *dump-mask*
      (format t "make mask, ~sx~s~%" wx wy)
      (format t "edges = ~s~%" edge-list)
      (format t "xsamples = ~s~%" sx))
    ;; convert it to in/out flags
    (loop with edges = edge-list
          for e across edges
          for j from 0
          for count = 0
          for i = 0
          do (when *dump-mask*
               (format t "edge ~s~%" e))
             (loop for (d ex) in e
                   while (< i wx)
                   do (loop for x = (aref sx i)
                            while (< x ex)
                            for in = (if (zerop count) 0 1)
                            do (setf (aref mask j i) in)
                               (incf i)
                            while (< i wx))
                      (ecase d
                        ((:both nil)
                         ;; do nothing
                         )
                        (:up (incf count))
                        (:down (decf count))))
             (loop with in = (if (zerop count) 0 1)
                   for x from i below wx
                   do (setf (aref mask j x) in)))
    mask))

(defun make-transpose-mask (wx wy sx sy edge-list)
  ;; mostly for testing, don't think this will be needed in practice
  ;; (only calculate the vertical edge list to detect undersampled
  ;; stems/gaps)
  (let ((tmask (make-mask wy wx (reverse sy) (reverse sx) edge-list))
        (mask (make-array (list wy wx) :element-type 'bit :initial-element 0)))
    (loop for j below wy
          do (loop for i below wx
                   do (setf (aref mask j i)
                            (aref tmask i (- wy j 1)))))
    mask))

(defun make-transpose-mask-for-sdf (sdf)
  (let* ((mask1 (signs sdf))
         (s2 (transpose-shape (cleaned-shape sdf)))
         (wy (array-dimension mask1 0))
         (wx (array-dimension mask1 1)))
    (make-transpose-mask wx wy
                         (samples/y sdf)
                         (samples/y sdf)
                         (%make-edge-list s2 (samples/x sdf)))))

(defun make-sdf (type shape &key (spread 2.5) (scale 1) integer-offset
                              (render t) origin wx wy)
  (when integer-offset
    ;; when true, calculate origin etc as (fixed-point?) integers instead
    ;; of doubles so we can store integer values in bmfont files
    ;; without rounding
    (error "todo: integer-offset"))
  (destructuring-bind (cwx cwy) (calculate-size shape spread scale)
    (when (or (and wx (> cwx wx))
              (and wy (> cwy wy)))
      ;; todo: restarts for "clip to specified size" and "use calculated size"
      (error "calculated minimum size of sdf ~sx~s larger than specified size of ~sx~s~% bounds ~s, scale ~s, spread ~s~%"
             cwx cwy wx wy
             (list (aabb-p1 (bounding-box shape))
                   (aabb-p2 (bounding-box shape)))
             scale spread))
    (unless wx (setf wx cwx))
    (unless wy (setf wy cwy))
    (let* (#++(bounds (bounding-box shape))
           (rbounds (rbounding-box shape))
           ;; find center of shape bounding box, in image units
           (cx (/ (+ (or (raabb-x1 rbounds) 0)
                     (or (raabb-x2 rbounds) 0))
                  (* 2 scale)))
           (cy (/ (+ (or (raabb-y1 rbounds) 0)
                     (or (raabb-y2 rbounds) 0))
                  (* 2 scale)))
           ;; find origin in image coords (0,0 = upper left corner of image)
           (ox (- (/ wx 2) cx))
           (oy (+ (/ wy 2) cy))
           (image (make-array (list* wy wx (a:ensure-list
                                            (channels-for-type type)))
                              :element-type 'single-float
                              :initial-element 0.0))
           (samples/x (make-array wx :element-type 'real
                                     :initial-element 0))
           (samples/y (make-array wy :element-type 'real
                                     :initial-element 0))
           (clean-shape (clean-shape shape)))
      (when origin
        (setf ox (aref origin 0))
        (setf oy (aref origin 1)))
      (loop for j below (length samples/y)
            do (setf (aref samples/y j)
                     (+ (* (- oy 1/2) scale)
                        (* j (- scale)))))

      (loop for i below (length samples/x)
            do (setf (aref samples/x i)
                     (+ (* (- 1/2 ox) scale)
                        (* i scale))))
      (when *dump-mask*
        (format t "make image ~s x ~s x ~s~%" wx wy (channels-for-type type))
        (format t "bounds = ~s~%" (bounding-box shape))
        (format t "rbounds = ~s~%" (rbounding-box shape))
        (format t "origin = ~s~%     ~s~%" (list ox oy) (list cx cy))
        #++
        (format t "samples at y = (oy= ~s / scale=~s):~%" oy scale)
        #++
        (loop for y across samples/y
              for j from 0
              do (format t "  ~s = ~s = ~s~%"
                         j (+ (* (- oy 1/2) scale)
                              (* j (- scale)))
                         y)))

      (let ((sdf (make-instance 'sdf :spread spread :sdf-type type
                                     :shape shape
                                     :cleaned-shape clean-shape
                                     :pixel-scale scale :origin (rv2 ox oy)
                                     :image image
                                     :signs (make-mask
                                             wx wy
                                             samples/x samples/y
                                             (%make-edge-list clean-shape
                                                              samples/y))
                                     :samples/x samples/x
                                     :samples/y samples/y)))
        (unless (zerop (length (contours (cleaned-shape sdf))))
          (render-sdf sdf :render render))
        sdf))))
