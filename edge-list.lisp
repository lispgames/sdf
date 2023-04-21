(in-package #:sdf/base)

(defparameter *dump-mask* nil)
#++ (setf *dump-mask* t)

(defmacro with-tranposed-readers ((flag &rest pairs) &body body)
  (a:with-gensyms (x)
    `(flet (,@ (loop for (a b) in pairs
                     collect `(,a (,x) (if ,flag (,b ,x) (,a ,x)))
                     collect `(,b (,x) (if ,flag (,a ,x) (,b ,x)))))
       (declare (ignorable ,@(loop for (a b) in pairs
                                   collect `(function ,a)
                                   collect `(function ,b))))
       ,@body)))


(defun %make-edge-list (shape scans &key transpose)
  (let ((edges (make-array (length scans) :initial-element nil))
        ;; while walking shape, we add to tmp-edges between extrema,
        ;; then when we hit another extremum we clean it up and move
        ;; it to EDGES, so we can make sure we get exactly 1 sample
        ;; per scan in a non-(increasing/decreasing) section of the
        ;; edge, even with FP error in quadratic solutions
        (tmp-edges (make-array (length scans) :initial-element nil))
        ;; start of contour is probably not at an extreme, so instead
        ;; of adding first section directly, we copy it and store it
        ;; to combine with final (partial) section at end of contour
        (start-edges nil)
        (last-j nil))
    (with-tranposed-readers (transpose
                             (vx vy)
                             (p-rx p-ry)
                             (s-rx1 s-ry1)
                             (s-rx2 s-ry2)
                             (b2-rx1 b2-ry1)
                             (b2-rxc b2-ryc)
                             (b2-rx2 b2-ry2))
      (labels ((add-edges (add-edges)
                 (when *dump-mask*
                   (format t "add edges ~s~%" add-edges))
                 ;; if we have multiple samples on a scan, just average
                 ;; them for now (assuming they are reasonably close)
                 (loop for e across add-edges
                       for y across scans
                       for j from 0
                       for c = (length e)
                       for d = (delete-duplicates (mapcar 'car e))
                       for x = (second (first e))
                       do (when *dump-mask*
                            (format t "~s: ~s ~s -> ~s ~s~%"
                                    j e y
                                    d (when e
                                        (reduce (ecase (car d)
                                                  (:up 'min)
                                                  (:down 'max))
                                                (mapcar 'cadr e)))))
                       when e
                         do (unless (eql 1 (length d))
                              (error "edge section with multiple directions @ ~s?~%~s"
                                     e edges))

                            (push (list* (car d)
                                         ;; might be a wide gap between
                                         ;; samples if we have a
                                         ;; horizontal edge, so pick
                                         ;; leftmost on upwards edges
                                         ;; or rightmost on downwards
                                         ;; edges
                                         (cdr
                                          (reduce (ecase (car d)
                                                    (:up (lambda (a b)
                                                           (if (< (second a) (second b))
                                                               a b)))
                                                    (:down (lambda (a b)
                                                             (if (< (second a) (second b))
                                                                 b a))))
                                                  e)))
                                  (aref edges j))))

               (finish-section (extreme end)
                 (when *dump-mask*
                   (format t "  finish section ~s ~s #tmp=~s #start=~s~%"
                           extreme end
                           (count nil tmp-edges :test-not 'eql)
                           (count nil start-edges :test-not 'eql)))
                 ;;if this is first section, move it to start-edges to
                 ;;finish later
                 (when (not start-edges)
                   ;; if we hit end of contour without changing
                   ;; directions, we should have rejected the contour
                   ;; during construction or clean-shape
                   #++(assert (not end))
                   (when end
                     ;; actually might be valid now, happens if we have
                     ;; a horizontal segment at extreme, since we call
                     ;; this at both ends?
                     (add-edges tmp-edges)
                     (fill tmp-edges nil)
                     (return-from finish-section nil))
                   (assert extreme)
                   (when *dump-mask*
                     (format t "-- save edges for later ~s~%" tmp-edges))
                   (setf last-j nil)
                   (setf start-edges (copy-seq tmp-edges))
                   (fill tmp-edges nil)
                   (return-from finish-section nil))
                 ;; if we hit an extreme, add tmp-edges to main edge list
                 (when extreme
                   (add-edges tmp-edges)
                   (fill tmp-edges nil))
                 (when end
                   (setf last-j nil)
                   ;; combine leftover partial section from start-edges
                   ;; with any partial section from end, and add to main list
                   (when *dump-mask*
                     (format t "-- use saved edges ~s~%" start-edges))
                   (loop for i below (length tmp-edges)
                         do (setf (aref tmp-edges i)
                                  (append (aref start-edges i)
                                          (aref tmp-edges i))))
                   (add-edges tmp-edges)
                   (setf start-edges nil)
                   (fill tmp-edges nil)))
               (next-y (n0 y0)
                 (loop for n = (next shape n0) then (next shape n)
                       do (etypecase n
                            (point
                             (when (/= (p-ry n) y0)
                               (return-from next-y (p-ry n))))
                            (segment
                             (when (/= (s-ry2 n) y0)
                               (return-from next-y (s-ry2 n))))
                            (bezier2
                             (when (/= (b2-ryc n) y0)
                               (return-from next-y (b2-ryc n)))
                             (when (/= (b2-ryc n) y0)
                               (return-from next-y (b2-ry2 n)))))
                       when (eq n0 n)
                         do (error "couldn't find next Y value?")))
               (prev-y (n0 y0)
                 (loop for n = (prev shape n0) then (prev shape n)
                       do (etypecase n
                            (point
                             (when (/= (p-ry n) y0)
                               (return-from prev-y (p-ry n))))
                            (segment
                             (when (/= (s-ry1 n) y0)
                               (return-from prev-y (s-ry1 n))))
                            (bezier2
                             (when (/= (b2-ryc n) y0)
                               (return-from prev-y (b2-ryc n)))
                             (when (/= (b2-ryc n) y0)
                               (return-from prev-y (b2-ry1 n)))))
                       when (eq n0 n)
                         do (error "couldn't find previous Y value?")))
               (add-x (j x dir node at)
                 (when *dump-mask*
                   (format t " add-x ~s ~s j=~s (~s)~%" x dir j last-j))
                 (push (list dir x node at) (aref tmp-edges j))
                 (setf last-j j))
               (dir (y1 y2)
                 (when (= y1 y2)
                   (error "tried to calculate direction of horizontal span?"))
                 (if (< y1 y2) :up :down))
               (addp (n end)
                 (let* ((y1 (p-ry n))
                        (py (prev-y n y1))
                        (ny (next-y n y1)))
                   ;; might finish-section a few extra times if we have
                   ;; a horizontal segment at extreme, but shouldn't matter?
                   (unless (or (< py y1 ny)
                               (> py y1 ny))
                     (finish-section t end))))
               (adds (n end)
                 (let ((y1 (s-ry1 n))
                       (y2 (s-ry2 n)))
                   (when *dump-mask*
                     (format t "add-segment ~s,~s -> ~s,~s ~@[end ~s~]~%"
                             (s-rx1 n) (s-ry1 n)
                             (s-rx2 n) (s-ry2 n)
                             end))
                   (cond

                     ((= y1 y2)
                      ;; horizontal segment, see if it is an extreme or not
                      (let ((py (prev-y n y1))
                            (ny (next-y n y1)))
                        (cond
                          ((or (< py y1 ny)
                               (> py y1 ny))
                           ;; not an extreme, assume end points are
                           ;; added by adjacent non-horizontal parts
                           )
                          (t ;; extreme: assume end points are added by
                           ;; adjacent parts, and finish section
                           (finish-section t end)))))
                     (t
                      ;; normal segment, add samples for all spans it crosses
                      (let ((dir (dir y1 y2))
                            (l (1- (length scans))))
                        (destructuring-bind (j1 dj j2)
                            (if (= (signum (- (aref scans 0) (aref scans 1)))
                                   (signum (- y1 y2)))
                                (list 0 1 l)
                                (list l -1 0))
                          (when *dump-mask*
                            (format t "  scans = ~,3f, ~,3f,...~%"
                                    (aref scans 0) (aref scans 1))
                            (format t "  j1 = ~s, dj = ~s, j2 = ~s~%" j1 dj j2))
                          (loop with x1 = (s-rx1 n)
                                with x2 = (s-rx2 n)
                                with s = (/ (- x2 x1)
                                            (- y2 y1))
                                ;; fixme: don't search entire range
                                for j = j1 then (+ j dj)
                                for y = (aref scans j)
                                when (or (<= y1 y y2)
                                         (>= y1 y y2))
                                  do (add-x j (+ x1 (* s (- y y1))) dir n
                                            (/ (- y y1)
                                               (- y2 y1)))
                                until (= j j2))))))
                   (when end
                     (finish-section nil end))))
               (split-b (b)
                 (let* ((v1 (p-rv (b2-p1 b)))
                        (vc (p-rv (b2-c1 b)))
                        (v2 (p-rv (b2-p2 b)))
                        (y1 (vy v1))
                        (yc (vy vc))
                        (y2 (vy v2))
                        (yc-y1 (- yc y1)))
                   ;; return T of extreme point on curve
                   (/ yc-y1 (- yc-y1 (- y2 yc)))))
               (addb (n end)
                 (when *dump-mask*
                   (format t "addb ~@[end=~s ~]~s,~s -> ~s,~s -> ~s,~s~%"
                           end
                           (b2-rx1 n) (b2-ry1 n)
                           (b2-rxc n) (b2-ryc n)
                           (b2-rx2 n) (b2-ry2 n)))
                 (let* ((x1 (b2-rx1 n))
                        (y1 (b2-ry1 n))
                        (xc (b2-rxc n))
                        (yc (b2-ryc n))
                        (x2 (b2-rx2 n))
                        (y2 (b2-ry2 n))
                        (a (+ y1 (* -2 yc) y2))
                        (b (* 2 (- yc y1)))
                        (-b (- b))
                        (b2 (expt b 2))
                        (2a (* 2 a))
                        (-4a (* -4 a))
                        (st (unless (or (<= y1 yc y2)
                                        (>= y1 yc y2))
                              (split-b n)))
                        (sy (when st
                              (lerp st
                                    (lerp st y1 yc)
                                    (lerp st yc y2))))
                        (samples nil))
                   (when *dump-mask*
                     (format t "  a=~s, st=~s, sy=~s~%" a st sy))
                   (loop
                     ;; fixme: don't search entire range
                     for y across scans
                     for j from 0
                     for up1 = (or (< y1 yc)
                                   (and (= y1 yc) (< y1 y2)))
                     for up2 = (or (< y2 yc)
                                   (and (= y2 yc) (> y1 y2)))
                     when (<= (min y1 yc y2) y (max y1 yc y2))
                       do (if (zerop a)
                              ;; simpler case, t->y is linear
                              (let* ((tt (/ (- y y1)
                                            (- y2 y1))))
                                (push (list tt :linear j) samples))
                              ;; full quadratic
                              (let* ((c (- y1 y))
                                     (disc (+ b2 (* -4a c)))
                                     (eps #.(* 32 double-float-epsilon)))
                                (cond
                                  ((zerop disc)
                                   ;; hit an extreme, need to
                                   ;; handle that specially later
                                   (push (list (/ -b 2a) :extreme j)
                                         samples))
                                  ((plusp disc)
                                   ;; 2 solutions, add both to list
                                   (let* ((rd (sqrt disc))
                                          (q (* -1/2 (if (minusp b)
                                                         (- b rd)
                                                         (+ b rd))))
                                          (t1 (/ q a))
                                          (t2 (/ c q)))
                                     (when *dump-mask*
                                       (unless (<= 0 t1 1)
                                         (format t " drop1 ~s @ ~s = ~s~%"
                                                 t1 j y))
                                       (unless (<= 0 t2 1)
                                         (format t " drop2 ~s @ ~s = ~s~%"
                                                 t2 j y)))
                                     (labels
                                         ((p (at)
                                            (push (list at :normal j) samples))
                                          (p? (at)
                                            (cond
                                              ((<= 0 at 1) (p at))
                                              ((and (< (- eps) at 0)
                                                    (if up1
                                                        (<= y1 y (+ y1 eps))
                                                        (<= (- y1 eps) y y1)))
                                               (p 0d0))
                                              ((and (< 1 at (+ 1 eps))
                                                    (if up2
                                                        (<= y2 y (+ y2 eps))
                                                        (<= (- y2 eps) y y2)))
                                               (p 1d0)))))
                                       (p? t1)
                                       (p? t2))))))))
                   (flet ((x (at)
                            (lerp at
                                  (lerp at x1 xc)
                                  (lerp at xc x2))))
                     (let ((s (sort samples '< :key 'car)))
                       (when *dump-mask*
                         (format t "  samples=~s~%" s))
                       ;; make sure we don't skip a sample at beginning
                       ;; due to FP loss (if endpoint is exactly on a
                       ;; sample, we calculate T just outside 0..1
                       ;; sometimes)
                       (when (and s last-j)
                         (let ((j1 (third (car s))))
                           (when (> (abs (- j1 last-j))
                                    1)
                             (when *dump-mask*
                               (format t "$$$$ add endpoint at skip: ~s - ~s~%"
                                       last-j j1))
                             (add-x (if (> j1 last-j) (1+ last-j) (1- last-j))
                                    x1 (if (= y1 yc) (dir y1 y2) (dir y1 yc))
                                    n 0))))
                       (cond
                         ((and st (not s))
                          ;; had an extreme point, but curve was
                          ;; entirely between samples, finish section
                          (when *dump-mask*
                            (format t "  finish section 1~%"))
                          (finish-section t end))
                         (st
                          ;; we have an extreme point, so need to finish
                          ;; segment in the middle
                          (loop with d1 = (if (= y1 sy)
                                              (dir sy y2)
                                              (dir y1 sy))
                                with d2 = (if (= sy y2)
                                              d1
                                              (dir sy y2))
                                for pt = -1 then tt
                                for ((tt flag j) . more) on s
                                do (when (or (< pt st tt)
                                             (= st tt))
                                     ;; we have more to add, so this
                                     ;; part isn't the 'end' yet
                                     (when *dump-mask*
                                       (format t "  finish section 2~%"))
                                     (finish-section t nil))
                                   (unless (eq flag :extreme)
                                     (progn ;when (<= 0 tt 1)
                                       (add-x j (x tt) (if (<= tt st) d1 d2)
                                              n tt)))
                                   (when (and (< tt st)
                                              (not more))
                                     (when *dump-mask*
                                       (format t "  finish 3 @  ~f ~f ~s~%" tt st more))
                                     (finish-section t nil))))
                         (t
                          ;; no extreme, just add all the points
                          (loop with dir = (dir y1 y2)
                                for (tt flag j) in s
                                for x = (x tt)
                                do (progn ;when (<= 0 tt 1)
                                     (assert (or (not (eq flag :extreme))
                                                 (= 0 tt)
                                                 (= 1 tt)))
                                     (add-x j (x tt) dir n tt))))))))
                 (when end
                   (when *dump-mask*
                     (format t "  finish section 4~%"))
                   (finish-section nil t)))

               (add (n e)
                 (typecase n
                   (point (addp n e))
                   (segment (adds n e))
                   (bezier2 (addb n e)))))
        ;; build edge list
        (map-contour-segments
         shape
         (lambda (c# node endp)
           (declare (ignorable c#))
           (add node endp)
           (when (and *dump-mask* endp)
             (format t "----- end contour ~s~%" c#))))
        ;; sort it
        (loop for i below (length edges)
              do (when (aref edges i)
                   (setf (aref edges i)
                         (sort (aref edges i) '<
                               :key 'second))))
        (when *dump-mask*
          (format t "edges =~%")
          (loop for j below (length edges)
                for y = (aref scans j)
                for e = (aref edges j)
                for cw = (count :up e :key 'car)
                for ccw = (count :down e :key 'car)
                do (format t "  ~s = ~s = ~s ~s~%" j y cw ccw)
                   (unless (zerop (- cw ccw))
                     (format t "!!!!!!! ~s~%" (- cw ccw)))
                   (when e (format t "     = ~s~%" e))))

        edges))))

;; returns vector of lists of edge crossings.

;; edge crossing = (dir pos node t), where DIR is :up or :down, pos is
;; point in opposite axis where NODE crosses, and T is fraction of
;; distance from start to end of NODE
(defun make-edge-list (sdf)
  (%make-edge-list (cleaned-shape sdf)
                   (samples/y sdf)))

(defun make-transpose-edge-list (sdf)
  (%make-edge-list (transpose-shape (cleaned-shape sdf)) (samples/x sdf)))
