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
               (to (if (< (abs a1) (abs a2)) p1 p2))
               (from (if (< (abs a1) (abs a2)) p2 p1)))
          (when (and (< l min-length)
                     (or (> (abs a1) min-angle)
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
                    (setf i (mod (1+ i) 2))))))))
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

(defun fix-msdf (image pimage edge-cells)
  ;; compare msdf to sdf and psdf, removing extra channels from msdf
  ;; where it is obviously wrong
  (let ((wx (array-dimension image 1))
        (wy (array-dimension image 0)))
    (labels ((median3 (a b c)
               (max (min a b) (min (max a b) c)))
             (median (j i)
               (median3 (aref image j i 0)
                        (aref image j i 1)
                        (aref image j i 2)))
             #++(nneg (x)
                  (> x -0.05))
             #++(npos (x)
                  (< x 0.05))
             (channel-crosses (j i c)

               (not (= (signum (aref image j i c))
                       (signum (aref image j (1+ i) c))
                       (signum (aref image (1- j) i c))
                       (signum (aref image (1- j) (1+ i) c)))))
             (l3 (d a b f)
               (setf (aref d 0) (a:lerp f (aref a 0) (aref b 0))
                     (aref d 1) (a:lerp f (aref a 1) (aref b 1))
                     (aref d 2) (a:lerp f (aref a 2) (aref b 2))))
             (ca (j i)
               (make-array 3 :element-type 'single-float
                             :initial-contents (list (aref image j i 0)
                                                     (aref image j i 1)
                                                     (aref image j i 2))))
             (cell-crosses (j i s)
               (let ((a (ca j i))
                     (b (ca j (1+ i)))
                     (c (ca (1- j) i))
                     (d (ca (1- j) (1+ i)))
                     (t1 (ca j i))
                     (t2 (ca j i))
                     (t3 (ca j i)))
                 (declare (dynamic-extent a b c d t1 t2 t3))
                 #++(format t "~&check cell crosses @ ~s,~s ~s ~s~% ~s~% ~s~% ~s~% ~s~%"
                            i j s (aref edge-cells j i)
                            a b c d)
                 (loop for x upto 1 by 0.0125
                       do (l3 t1 a b x)
                          (l3 t2 c d x)
                          (loop for y upto 1 by 0.0125
                                do (l3 t3 t1 t2 y)
                                   (let ((m (median3 (aref t3 0)
                                                     (aref t3 1)
                                                     (aref t3 2))))
                                     #++(format t " ~2,' d" (round (signum m)))
                                     #++(format t "~,3f " m)
                                     #++(format t "~s,~s~%  = ~s, ~s~%  -> ~s~%"
                                                x y t1 t2 t3)
                                     #++(format t "~s,~s->~s~%" x y  t3)
                                     (when (/= (signum m) s)
                                       #++(format t "!!~%")
                                       (return-from cell-crosses t))))
                       #++(format t "~%"))
                 nil)))
      (declare (inline l3 ca))
      ;; clear msdf for any sample where it doesn't match psdf
      (loop for j below wy
            do (loop for i below wx
                     for m = (median j i)
                     for p = (aref pimage j i)
                     when (> (abs (- p m)) #.(/ 1024.0))
                       do (setf (aref image j i 0) p
                                (aref image j i 1) p
                                (aref image j i 2) p)))

      #++(let ((i 14)
               (j 32))
           (setf (aref image j i 0) 0.0
                 (aref image j i 2) 0.0
                 (aref image j i 1) 0.0
                 (aref image j i 3) -10.0))
      #++
      (loop for j below wy
            do (loop for i below wx
                     do (setf (aref image j i 3)
                              (if (aref edge-cells j i)
                                  0.2 -10.0))))
      #++
      (loop for j below wy
            do (loop for i below wx
                     do (setf (aref image j i 3)
                              (if (aref corner-cells j i)
                                  0.2 -10.0))))


      ;; find any cells where median is all above/below 0, with at
      ;; least 2 different channels on opposite side

      #++
      (loop for j from 1 below wy
            do (loop for i below (1- wx)
                     for m1 = (median j i)
                     for m2 = (median j (1+ i))
                     for m3 = (median (1- j) i)
                     for m4 = (median (1- j) (1+ i))
                     when (and (not (aref edge-cells j i))
                               (= (signum m1) (signum m2)
                                  (signum m3) (signum m4))
                               (> (loop for c below 3
                                        count (channel-crosses j i c))
                                  1)
                               (cell-crosses j i (signum m1)))
                       do (loop
                            with s = (signum m1)
                            for m in (list m1 m2 m3 m4)
                            for d in '((0 0) (0 -1) (1 0) (1 -1))
                            for (x y) = d
                            for i2 = (+ x i)
                            for j2 = (+ j y)
                            when
                            (and (not (aref edge-cells j2 i2))
                                 (or (/= (signum (aref image j2 i2 0)) s)
                                     (/= (signum (aref image j2 i2 1)) s)
                                     (/= (signum (aref image j2 i2 2)) s)))

                            do (let* ((p (aref pimage j i)))
                                 (setf (aref image j2 i2 0) p
                                       (aref image j2 i2 1) p
                                       (aref image j2 i2 2) p
                                       (aref image j2 i2 3) -100.0)))))

      (loop for j from 1 below wy
            do (loop for i below (1- wx)
                     for m1 = (median j i)
                     for m2 = (median j (1+ i))
                     for m3 = (median (1- j) i)
                     for m4 = (median (1- j) (1+ i))
                     for safe = nil
                     for risky = nil
                     when (and (not (aref edge-cells j i))
                                        ;(not (aref corner-cells j i))
                               (= (signum m1) (signum m2)
                                  (signum m3) (signum m4))
                               (> (loop for c below 3
                                        count (channel-crosses j i c))
                                  1)
                               (cell-crosses j i (signum m1)))
                       do (loop
                            with s = (signum m1)
                            for m in (list m1 m2 m3 m4)
                            for d in '((0 0) (0 -1) (1 0) (1 -1))
                            for (x y) = d
                            for i2 = (+ x i)
                            for j2 = (+ j y)
                            when
                            (or (/= (signum (aref image j2 i2 0)) s)
                                (/= (signum (aref image j2 i2 1)) s)
                                (/= (signum (aref image j2 i2 2)) s))

                            do (if (aref edge-cells (1- j2) (1- i2))
                                   (push (list i2 j2) risky)
                                   (push (list i2 j2) safe)))
                          (progn
                            (setf safe (sort safe '>
                                             :key (lambda (a)
                                                    (destructuring-bind (i j) a
                                                      (abs (aref pimage j i))))))
                            (format t "~s,~s: safe = ~s~%  risky = ~s~%"
                                    i j safe risky)
                            (loop for (i j) in (append safe risky)
                                  do (format t "~s,~s = ~s~%"
                                             i j (aref pimage j i))))
                     #++(loop for (i2 j2) in safe
                              for p = (aref pimage j2 i2)
                              #++unless (aref corner-cells j2 i2)
                              #++unless (aref edge-cells j2 i2)
                              do (setf (aref image j2 i2 0) p
                                       (aref image j2 i2 1) p
                                       (aref image j2 i2 2) p
                                       (aref image j2 i2 3) -100.0)
                                 (format t "update ~s ~s safe~%" i2 j2))
                        (loop for (i2 j2) in (append safe risky)
                              for p = (aref pimage j2 i2)
                              while (and
                                     #++(not (aref corner-cells j2 i2))
                                     #++(not
                                         (or (aref edge-cells j2 i2)
                                             (aref edge-cells (1+ j2) i2)
                                             (aref edge-cells j2 (1- i2))
                                             (aref edge-cells (1+ j2) (1- i2))))

                                     (> (loop for c below 3
                                              count (channel-crosses j i c))
                                        1)
                                     (cell-crosses j i (signum m1)))
                              when (not
                                    (or (aref edge-cells j2 i2)
                                        (aref edge-cells (1+ j2) i2)
                                        (aref edge-cells j2 (1- i2))
                                        (aref edge-cells (1+ j2) (1- i2))))
                                do (setf (aref image j2 i2 0) p
                                         (aref image j2 i2 1) p
                                         (aref image j2 i2 2) p
                                         (aref image j2 i2 3) -100.0)
                                   (format t "update ~s ~s~%" i2 j2))))

      #++
      (loop for j from 1 below wy
            do (loop for i below (1- wx)
                     for m1 = (median j i)
                     for m2 = (median j (1+ i))
                     for m3 = (median (1- j) i)
                     for m4 = (median (1- j) (1+ i))
                     when (and (not (aref edge-cells j i))
                               (= (signum m1) (signum m2)
                                  (signum m3) (signum m4))
                               (> (loop for c below 3
                                        count (channel-crosses j i c))
                                  1)
                               (cell-crosses j i (signum m1)))
                       do (loop
                            with s = (signum m1)
                            for m in (list m1 m2 m3 m4)
                            for d in '((0 0) (0 -1) (1 0) (1 -1))
                            for (x y) = d
                            for i2 = (+ x i)
                            for j2 = (+ j y)
                            when
                            (and (not (aref edge-cells j2 i2))
                                 (or (/= (signum (aref image j2 i2 0)) s)
                                     (/= (signum (aref image j2 i2 1)) s)
                                     (/= (signum (aref image j2 i2 2)) s)))

                            do (let* ((p (aref pimage j i)))
                                 (setf (aref image j2 i2 0) p
                                       (aref image j2 i2 1) p
                                       (aref image j2 i2 2) p
                                       (aref image j2 i2 3) -100.0))))))))

#++
(defun fix-msdf2 (image pimage groups sample-colors)
  (let ((wx (array-dimension image 1))
        (wy (array-dimension image 0)))
    (labels ((median3 (a b c)
               (max (min a b) (min (max a b) c)))
             (median (j i)
               (median3 (aref image j i 0)
                        (aref image j i 1)
                        (aref image j i 2)))
             (cgroups (j i c)
               #++(make-array 4 :element-type 'fixnum
                                :initial-contents
                                (list (aref sample-colors j i c)
                                      (aref sample-colors j (1+ i) c)
                                      (aref sample-colors (1- j) i c)
                                      (aref sample-colors (1- j) (1+ i) c)))
               (list (aref sample-colors j i c)
                     (aref sample-colors j (1+ i) c)
                     (aref sample-colors (1- j) i c)
                     (aref sample-colors (1- j) (1+ i) c)))
             (channel-conflict (j i col)
               #++
               (multiple-value-bind (a b c d) (cgroups j i col)
                 (cond
                   ((= a b c d) nil)
                   ()))
               (let ((x (delete-duplicates (remove 0 (cgroups j i col)))))
                 #++(format t "~s ~s ~s = ~s~%" i j c x)
                 (> (length x)
                    1))))
      (declare (inline cgroups))
      ;; clear msdf for any sample where it doesn't match psdf
      (loop for j below wy
            do (loop for i below wx
                     for m = (median j i)
                     for p = (aref pimage j i)
                     when (> (abs (- p m)) #.(/ 1024.0))
                       do (setf (aref image j i 0) p
                                (aref image j i 1) p
                                (aref image j i 2) p
                                (aref image j i 3) (* -100 (aref image j i 3)))))
      ;; in any cells where more than 1 edge group affects a channel,
      ;; clear all but nearest group
      #++(loop
           for j from 1 below wy
           do (loop
                for i below (1- wx)
                do (loop
                     for c below 3
                     when (channel-conflict j i c)
                       do
                          (let* ((p (aref pimage j i))
                                 (o (aref image j i 3)))
                            (setf (aref image j i c) p
                                  (aref sample-colors j i c) 0
                                  (aref image j i 3)
                                  (cond
                                    ((<= -10 o 0) 100.0)
                                    ((<= 0 o 10) -100.0)
                                    (t o))))
                     #++(loop
                          for d in '((0 0) (0 -1) (1 0) (1 -1))
                          for (x y) = d
                          for i2 = (+ x i)
                          for j2 = (+ j y)
                          do (let* ((p (aref pimage j2 i2))
                                    (o (aref image j2 i2 3)))
                               (setf    ;(aref image j2 i2 0) p
                                        ;(aref image j2 i2 1) p
                                        ;(aref image j2 i2 2) p
                                (aref image j2 i2 c) p
                                (aref image j2 i2 3)
                                (cond
                                  ((<= -10 o 0) 100.0)
                                  ((<= 0 o 10) -100.0)
                                  (t o))))))))

      (loop
        for j from 1 below wy
        do (loop
             for i below (1- wx)
             for n = (delete-duplicates (remove 0 (append (cgroups j i 0)
                                                          (cgroups j i 1)
                                                          (cgroups j i 2))))
             when (> (length n) 1)
               do (let* ((p (aref pimage j i))
                         (o (aref image j i 3))
                         #++(m (list t t t)))
                    #++(loop for i in n
                             for c = (gethash i groups)
                             do (setf m (loop for c in c
                                              for m in m
                                              collect (and m c))))
                    (when #++(> (count t m) 1)
                          (loop for (a b) on n
                                while b
                                  thereis (> (loop for a in (gethash a groups)
                                                   for b in (gethash b groups)
                                                   count (and a b))
                                             1))
                          (setf (aref image j i 0) p
                                (aref image j i 1) p
                                (aref image j i 2) p
                                        ;(aref sample-colors j i 0) 0
                                        ;(aref sample-colors j i 1) 0
                                        ;(aref sample-colors j i 2) 0
                                (aref image j i 3)
                                (cond
                                  ((<= -10 o 0) 100.0)
                                  ((<= 0 o 10) -100.0)
                                  (t o)))))
             #++(loop
                  for d in '((0 0) (0 -1) (1 0) (1 -1))
                  for (x y) = d
                  for i2 = (+ x i)
                  for j2 = (+ j y)
                  do (let* ((p (aref pimage j2 i2))
                            (o (aref image j2 i2 3)))
                       (setf            ;(aref image j2 i2 0) p
                                        ;(aref image j2 i2 1) p
                                        ;(aref image j2 i2 2) p
                        (aref image j2 i2 c) p
                        (aref image j2 i2 3)
                        (cond
                          ((<= -10 o 0) 100.0)
                          ((<= 0 o 10) -100.0)
                          (t o))))))))))

(defun fix-msdf3 (image pimage n edge-cells #++ #++ groups sample-colors)
  (let ((wx (array-dimension image 1))
        (wy (array-dimension image 0))
        (flags (make-array (array-dimensions pimage) :element-type 'bit
                                                     :initial-element 0)))
    (labels ((median3 (a b c)
               (max (min a b) (min (max a b) c)))
             (median (j i)
               (median3 (aref image j i 0)
                        (aref image j i 1)
                        (aref image j i 2)))
             #++(cgroups (j i c)
                  #++(make-array 4 :element-type 'fixnum
                                   :initial-contents
                                   (list (aref sample-colors j i c)
                                         (aref sample-colors j (1+ i) c)
                                         (aref sample-colors (1- j) i c)
                                         (aref sample-colors (1- j) (1+ i) c)))
                  (list (aref sample-colors j i c)
                        (aref sample-colors j (1+ i) c)
                        (aref sample-colors (1- j) i c)
                        (aref sample-colors (1- j) (1+ i) c)))
             #++(channel-conflict (j i col)
                  #++
                  (multiple-value-bind (a b c d) (cgroups j i col)
                    (cond
                      ((= a b c d) nil)
                      ()))
                  (let ((x (delete-duplicates (remove 0 (cgroups j i col)))))
                    #++(format t "~s ~s ~s = ~s~%" i j c x)
                    (> (length x)
                       1))))
      #++(declare (inline cgroups))
      ;; for each sample, calculate difference between it and adjacent
      ;; samples, and clear msdf if more than 2 channels have a max
      ;; difference of more than N
      (let ((d (make-array 3 :element-type 'single-float :initial-element 0.0))
            (n2 (* (sqrt 2) n)))
        (declare (dynamic-extent d))
        (loop for j below wy
              do (loop for i below wx
                       for count = 0
                       for p = (aref pimage j i)
                       do (fill d 0.0)
                          (flet ((d (s j i c)
                                   (if (array-in-bounds-p image j i c)
                                       (let ((s2 (aref image j i c)))
                                         (abs (- s2 s)))
                                       0.0)))
                            (loop for c below 3
                                  for s = (aref image j i c)
                                  for d = (max (d s j (1- i) c)
                                               (d s j (1+ i) c)
                                               (d s (1- j) i c)
                                               (d s (1+ j) i c))
                                  for d2 = (max (d s (1+ j) (1+ i) c)
                                                (d s (1+ j) (1- i) c)
                                                (d s (1- j) (1+ i) c)
                                                (d s (1- j) (1- i) c))
                                  when (or (> d n)
                                           (> d2 n2))
                                    do (incf count)))
                       when (>= count 2)
                         do (setf (aref flags j i) 1)))
        (loop for j below wy
              do (loop for i below wx
                       for p = (aref pimage j i)
                       when (and (plusp (aref flags j i))
                                 ;; only apply this heuristic to
                                 ;; texels more than 1 away from
                                 ;; actual edge, since other
                                 (not (aref edge-cells j i))
                                 #++(>= (abs p) 0.7))
                         do (setf (aref image j i 0) p
                                  (aref image j i 1) p
                                  (aref image j i 2) p
                                  (aref image j i 3) (* -100 (aref image j i 3))))))
      ;; clear msdf for any sample where it doesn't match psdf, or at edges
      #++
      (loop for j below wy
            do (loop for i below wx
                     for m = (median j i)
                     for p = (aref pimage j i)
                     when (or (zerop j)
                              (zerop i)
                              #++(= j (1- wy))
                              #++(= i (1- wx))
                              (> (abs (- p m)) #.(/ 1024.0)))
                       do (setf (aref image j i 0) p
                                (aref image j i 1) p
                                (aref image j i 2) p
                                (aref image j i 3) (* -100 (aref image j i 3))))))))


(defun fix-msdf4 (image pimage groups)
  (let ((wx (array-dimension image 1))
        (wy (array-dimension image 0))
        (flags (make-array (array-dimensions image) :element-type 'bit
                                                    :initial-element 0)))
    ;; find any pairs of samples that are from different groups, and
    ;; flag whichever is further from edge (unless other is already
    ;; flagged)
    (flet ((check (j1 i1 j2 i2 c)
             (let ((a (aref image j1 i1 c))
                   (b (aref image j2 i2 c)))
               (when (and
                      ;; haven't already flagged one
                      (not (or (plusp (aref flags j1 i1 c))
                               (plusp (aref flags j2 i2 c))))
                      ;; at least one differs from actual value
                      (or (/= a (aref pimage j1 i1))
                          (/= b (aref pimage j2 i2)))
                      ;; and they are from different groups
                      (/= (aref groups j1 i1 c)
                          (aref groups j2 i2 c))
                      (not (zerop (aref groups j1 i1 c)))
                      (not (zerop (aref groups j2 i2 c))))
                 ;; flag whichever is further
                 (progn                 ;if (> (abs a) (abs b))
                   (setf (aref flags j1 i1 c) 1)
                   (setf (aref flags j2 i2 c) 1))))))
      (loop for j below (1- wy)
            do (loop for i below (1- wx)
                     do (loop for c below 3
                              do (check j i (1+ j) i c)
                                 (check j i j (1+ i) c)
                              #++(check j i (1+ j) (1+ i) c))))

      (loop for j below wy
            do (loop for i below wx
                     for p = (aref pimage j i)
                     for n = 0
                     do #++(loop for c below 3
                                 when (plusp (aref flags j i c))
                                   do (setf (aref image j i c) p
                                            (aref image j i 3) (* -100 (aref image j i 3))))
                        (loop for c below 3
                              when (plusp (aref flags j i c))
                                do (incf n))
                        (when (> n 1)
                          (setf (aref image j i 0) p
                                (aref image j i 1) p
                                (aref image j i 2) p
                                (aref image j i 3) (* -100 (aref image j i 3)))))))))

(defvar *last-edge-colors* nil)
(defvar *last-sample-colors* nil)
(defun render-sdf/msdf (sdf &key mtsdf)
  (let* ((spread (spread sdf))
         (scale (pixel-scale sdf))
         (x1 (* scale (- (aref (origin sdf) 0) 1/2)))
         (y1 (* scale (- (aref (origin sdf) 1) 1/2)))
         (dx scale)
         (dy (- scale))
         (image (image sdf))
         (simage (make-array (subseq (array-dimensions image) 0 2)
                             :element-type 'single-float :initial-element 0.0))
         (pimage (make-array (subseq (array-dimensions image) 0 2)
                             :element-type 'single-float :initial-element 0.0))
         (sample-colors (make-array (array-dimensions image)
                                    :element-type 'fixnum :initial-element 0))
         (shape (cleaned-shape sdf))
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
         #++(edges/x (%make-edge-list shape samples/x :transpose t))
         (edges/y (%make-edge-list shape samples/y))
         (facing (determine-facing shape edges/y))
         (edge-cells (make-array (array-dimensions pimage)
                                 :initial-element nil))
         #++
         (corner-cells (make-array (array-dimensions pimage)
                                   :initial-element nil)))

    (setf *last-edge-colors* colors)
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

      (time
       (progn
         (a:write-string-into-file
          (serialize-shape shape :allow-ratios nil :edge-colors colors)
          "c:/tmp/shapedesc1.txt"
          :if-exists :supersede)
         (uiop:run-program
          (print
           (format nil "/msys64/usr/bin/time msdfgen -autoframe -pxrange ~a -scale ~a -size ~a ~a -o c:/tmp/shapedesc1b.png -testrender c:/tmp/shapedesc1-r.png 256 256  -shapedesc c:/tmp/shapedesc1.txt"
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
           (format nil "/msys64/usr/bin/time msdfgen -autoframe -pxrange ~a -scale ~a -size ~a ~a -o c:/tmp/shapedesc1nc.png -testrender c:/tmp/shapedesc1nc-r.png 256 256  -shapedesc c:/tmp/shapedesc1nc.txt -printmetrics"
                   (* 2 spread) (float (/ scale))
                   (array-dimension image 1)
                   (array-dimension image 0)))
          :output t
          :error-output t)))
      (terpri)
      (loop with wy = (array-dimension image 0)
            for j below wy
            for rs = 0
            for gs = 0
            for bs = 0
            for y across samples/y
            for edge-row across edges/y
            do #++(setf (values rs gs bs) (init-signs edge-row))
            #++(format t "at ~s: ~s ~s ~s~%" j rs gs bs)
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
                                #++
                                (let ((m scale))
                                  (when (> (abs r) m)
                                    (setf r a))
                                  (when (> (abs g) m)
                                    (setf g a))
                                  (when (> (abs b) m)
                                    (setf b a)))
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
                                  (scale (abs a) (aref signs j i)))))))

      (labels ((i (x)
                 #++(incf x (/ scale 2))
                 (let ((i (floor (/ (+ x x1) dx))))
                   #++(if (< x (aref samples/x i))
                          (1- i)
                          i)
                   (cond
                     ((<= x (aref samples/x i))
                      (1- i))
                     ((and (< (aref samples/x i) x)
                           (<= x (aref samples/x (1+ i))))
                      i)
                     ((> x (aref samples/x (1+ i)))
                      (+ i 1))
                     (t #++ (< y (aref samples/y (1+ j)))
                        (break "a")
                        #++(- (length samples/x) 2)))))
               (j (y)
                 #++(incf y (/ scale -2))
                 (let ((j (floor (/ (- y y1) dy))))
                   #++(if (> y (aref samples/y j))
                          0             ;(1+ j)
                          j)
                   (cond
                     ((> y (aref samples/y j))
                      (1- j))
                     ((and (<= (aref samples/y (1+ j)) y)
                           (<= y (aref samples/y j)))
                      (1+ j))
                     (t #++ (< y (aref samples/y (1+ j)))
                        (+ 2 j)
                        #++(1- (length samples/y))))))
               (eval-at (n at)
                 (etypecase n
                   (segment (eval-at/s/fast n at))
                   (bezier2 (eval-at/b2/fast n at))))
               (ij= (a b)
                 (and (= (car a) (car b))
                      (= (cdr a) (cdr b))))
               (ij~ (a b)
                 #++(and (<= (1- (cdr b)) (cdr a) (1+ (cdr b)))
                         (<= (1- (car b)) (car a) (1+ (car b))))

                 (or (and (= (car a) (car b))
                          (<= (1- (cdr b)) (cdr a) (1+ (cdr b)))
                          #++(or (= (cdr a) (cdr b))
                                 (= (cdr a) (1+ (cdr b)))
                                 (= (cdr a) (1- (cdr b)))))
                     (and (= (cdr a) (cdr b))
                          (<= (1- (car b)) (car a) (1+ (car b)))
                          #++(or (= (cAr a) (car b))
                                        ;(= (cAr a) (1+ (car b)))
                                        ;(= (car a) (1- (car b)))
                                 ))))
               #++(nneg (x)
                    (> x -0.0001))
               #++(npos (x)
                    (< x 0.0001))
               (flat (i j)
                 #++
                 (let ((a (aref pimage j i))
                       (b (aref pimage (1- j) i))
                       (c (aref pimage j (1+ i)))
                       (d (aref pimage (1- j) (1+ i))))
                   #++(or (and (nneg a)
                               (nneg b)
                               (nneg c)
                               (nneg d))
                          (and (npos a)
                               (npos b)
                               (npos c)
                               (npos d)))
                   (or (and (not (plusp a))
                            (not (plusp b))
                            (not (plusp c))
                            (not (plusp d)))
                       (and (not (minusp a))
                            (not (minusp b))
                            (not (minusp c))
                            (not (minusp d)))))
                 (= (signum (aref pimage j i))
                    (signum (aref pimage (1- j) i))
                    (signum (aref pimage j (1+ i)))
                    (signum (aref pimage (1- j) (1+ i)))))
               (flag (i j)
                 (when (flat i j)
                   (setf (aref edge-cells j i) t)))
               (rsubdivide (n start end sij eij &optional (d 0))
                 (when (> d 26)
                   (progn
                     (flag (car sij) (cdr sij))
                     (flag (car eij) (cdr eij)))
                   (break "~s?" d)
                   (return-from rsubdivide t))
                 (if (ij~ sij eij)
                     #++(setf (aref edge-cells (cdr sij) (car sij)) t
                              (aref edge-cells (cdr eij) (car eij)) t)
                     (progn
                       (flag (car sij) (cdr sij))
                       (flag (car eij) (cdr eij)))

                     (let* ((mat (/ (+ start end) 2.0))
                            (mid (eval-at n mat))
                            (mij (cons (i (vx mid)) (j (vy mid)))))
                       (declare (dynamic-extent mij))
                       (when (or (= mat start) (= mat end))
                         (break "~s" d))
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
                                #++(when (typep n 'point)
                                     (setf (aref edge-cells
                                                 (j (p-dy n))
                                                 (i (p-dx n)))
                                           t))
                                (etypecase n
                                  (point
                                   (flag (i (p-dx n)) (j (p-dy n)))
                                   #++(setf (aref edge-cells
                                                  (j (p-dy n))
                                                  (i (p-dx n)))
                                            t)
                                   #++
                                   (setf (aref corner-cells
                                               (j (p-dy n))
                                               (i (p-dx n)))
                                         t)
                                   #++(loop
                                        for di in '(-0.5 0.0 0.5)
                                        for i = (i (+ (* scale di) (p-dx n)))
                                        do (loop
                                             for dj in '(-0.5 0.0 0.5)
                                             for j = (j (+ (* scale dj) (p-dy n)))
                                             when (flat i j)
                                               do (setf (aref corner-cells j i)
                                                        t))))
                                  ((or segment bezier2)
                                   (subdivide n)))))
        #++(loop
             for e across edges/x
             for x across samples/x
             for i from 0
             do (loop for (nil y) in e
                      do (setf (aref edge-cells (j y) i) t)))
        #++(loop
             for e across edges/y
             for y across samples/y
             for j from 0
             do (loop for (nil x) in e
                      do (setf (aref edge-cells j (i x)) t))))
      #++
      (fix-msdf image pimage edge-cells)
      #++
      (fix-msdf2 image pimage groups sample-colors)

      (progn ;; debug prints
        (format t "rc = ~%")
        (loop for j below (array-dimension sample-colors 0)
              do (loop for i below (array-dimension sample-colors 1)
                       do (format t " ~2,' d" (aref sample-colors j i 0)))
                 (format t "~%"))
        (format t "gc = ~%")
        (loop for j below (array-dimension sample-colors 0)
              do (loop for i below (array-dimension sample-colors 1)
                       do (format t " ~2,' d" (aref sample-colors j i 1)))
                 (format t "~%"))
        (format t "bc = ~%")
        (loop for j below (array-dimension sample-colors 0)
              do (loop for i below (array-dimension sample-colors 1)
                       do (format t " ~2,' d" (aref sample-colors j i 2)))
                 (format t "~%")))

      (progn ;; fix3
        (let ((m (* 0.8 (sqrt 2))))
          #++
          (progn ;; debug prints
            (format t "dc = ~3f ~3f~%" spread m)
            (loop for j below (array-dimension image 0)
                  do (loop for i below (array-dimension image 1)
                           for dm = 0
                           do (flet ((d (s j i c)
                                       (if (array-in-bounds-p image j i c)
                                           (let ((s2 (aref image j i c)))
                                             (abs (- s2 s)))
                                           0.0)))
                                (loop for c below 3
                                      for s = (aref image j i c)
                                      for d = (max (d s j (1- i) c)
                                                   (d s j (1+ i) c)
                                                   (d s (1- j) i c)
                                                   (d s (1+ j) i c))
                                      do (a:maxf dm d)))
                              (format t " ~3f" dm))
                     (format t "~%"))
            (loop for c below 3
                  do (loop for j below (array-dimension image 0)
                           do (loop for i below (array-dimension image 1)
                                    for dm = 0
                                    do (flet ((d (s j i c)
                                                (if (array-in-bounds-p image j i c)
                                                    (let ((s2 (aref image j i c)))
                                                      (abs (- s2 s)))
                                                    0.0)))
                                         (let* ((s (aref image j i c))
                                                (mm (max (d s j (1- i) c)
                                                         (d s j (1+ i) c)
                                                         (d s (1- j) i c)
                                                         (d s (1+ j) i c))))
                                           (cond
                                             ((> mm (* 1.02 (sqrt 2)))
                                              (format t " #"))
                                             ((> mm m)
                                              (format t " *"))
                                             ((> mm 1)
                                              (format t " ~~"))
                                             (t
                                              (format t " ."))))))
                              (format t "~%"))
                     (format t "~%"))
            (loop for j below (array-dimension image 0)
                  do (loop for i below (array-dimension image 1)
                           for count = 0
                           do (flet ((d (s j i c)
                                       (if (array-in-bounds-p image j i c)
                                           (let ((s2 (aref image j i c)))
                                             (abs (- s2 s)))
                                           0.0)))
                                (loop for c below 3
                                      for s = (aref image j i c)
                                      for d = (max (d s j (1- i) c)
                                                   (d s j (1+ i) c)
                                                   (d s (1- j) i c)
                                                   (d s (1+ j) i c))
                                      when (> d m) do (incf count)))
                              (format t " ~s" count))
                     (format t "~%"))
            (loop for j below (array-dimension image 0)
                  do (loop for i below (array-dimension image 1)
                           for dm = 0
                           do (flet ((d (s j i c)
                                       (if (array-in-bounds-p image j i c)
                                           (let ((s2 (aref image j i c)))
                                             (abs (- s2 s)))
                                           0.0)))
                                (loop for c below 1
                                      for s = (aref image j i c)
                                      for d = (max (d s j (1- i) c)
                                                   (d s j (1+ i) c)
                                                   (d s (1- j) i c)
                                                   (d s (1+ j) i c))
                                      do (a:maxf dm (abs s))))
                              (format t " ~4f" dm))
                     (format t "~%")))
          #++
          (fix-msdf3 image pimage m edge-cells)))
      #++(fix-msdf4 image pimage sample-colors)

      (loop for j below (array-dimension image 0)
            do (loop for i below (array-dimension image 01)
                     for p = (aref pimage j i)
                     do (loop for c below 3
                              for s = (aref image j i c)
                              when (>= (abs p) (- spread 0.0))
                                do (setf (aref image j i c) p))))

      (loop for j below (array-dimension image 0)
            do (loop for i below (array-dimension image 01)
                     for p = (aref pimage j i)
                     do (loop for c below 4
                              do (setf (aref image j i c)
                                       (/ (aref image j i c) spread)))
                        (setf (aref image j i 3)
                              (/ p spread)))))))
