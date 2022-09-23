(in-package #:sdf/base)


(defun shape-to-edit-shape (shape)
  (make-instance
   'edit-shape
   :contours (loop for c across (contours shape)
                   collect (make-edit-contour shape c))))

(defun %edit-shape-to-shape (contours &key metadata)
  (with-shape-builder (shape :metadata metadata)
    (loop for c in contours
          unless (degenerate-edit-contour c)
            do (when (typep c 'es-contour-edge)
                 (setf c (eprev c)))
               (assert (typep c 'es-contour-vertex))
               (start-contour (p-rx (point c)) (p-ry (point c)))
               (loop for n = c then (enext n)
                     do (etypecase n
                          (es-contour-segment
                           (let ((p2 (point (enext n))))
                             (line-to (p-rx p2) (p-ry p2))))
                          (es-contour-bezier2
                           (let ((p2 (point (enext n)))
                                 (pc (control-point n)))
                             (quadratic-to (p-rx pc) (p-ry pc)
                                           (p-rx p2) (p-ry p2))))
                          (es-contour-vertex))
                     until (eql (enext n) c))
               (end-contour))))

(defun edit-shape-to-shape (edit-shape &key metadata)
  (%edit-shape-to-shape (contours edit-shape) :metadata metadata))

(defun transpose-shape (in)
  (let ((start t))
    (with-shape-builder (shape)
      (map-contour-segments
       in (lambda (c n e)
            (declare (ignore c))
            (when start
              (setf start nil)
              ;; contour should always start with a point
              (start-contour (p-ry n) (p-rx n)))
            (etypecase n
              (point)
              (segment (line-to (s-ry2 n) (s-rx2 n)))
              (bezier2 (quadratic-to
                        (b2-ryc n) (b2-rxc n)
                        (b2-ry2 n) (b2-rx2 n))))
            (when e
              (setf start t)
              (end-contour)))))))


(defun coerce-shape (in element-type)
  (let ((start t))
    (flet ((d (x) (coerce x element-type)))
      (with-shape-builder (shape)
        (map-contour-segments
         in (lambda (c n e)
              (declare (ignore c))
              (when start
                (setf start nil)
                ;; contour should always start with a point
                (start-contour (d (p-rx n)) (d (p-ry n))))
              (etypecase n
                (point)
                (segment (line-to (d (s-rx2 n)) (d (s-ry2 n))))
                (bezier2 (quadratic-to
                          (d (b2-rxc n)) (d (b2-ryc n))
                          (d (b2-rx2 n)) (d (b2-ry2 n)))))
              (when e
                (setf start t)
                (end-contour))))))))

(defun translate-shape (in dx dy)
  ;; offset should probably be a option to parse-shape?
  (let ((start t))
    (with-shape-builder (shape)
      (map-contour-segments
       in (lambda (c n e)
            (declare (ignore c))
            (when start
              (setf start nil)
              ;; contour should always start with a point
              (start-contour (+ (p-rx n) dx) (+ (p-ry n) dy)))
            (etypecase n
              (point)
              (segment (line-to (+ (s-rx2 n) dx) (+ (s-ry2 n) dy)))
              (bezier2 (quadratic-to
                        (+ (b2-rxc n) dx) (+ (b2-ryc n) dy)
                        (+ (b2-rx2 n) dx) (+ (b2-ry2 n) dy))))
            (when e
              (setf start t)
              (end-contour)))))))

(defun scale-shape (in sx &optional (sy sx))
  ;; scale should probably be a option to parse-shape?
  (let ((start t))
    (with-shape-builder (shape)
      (map-contour-segments
       in (lambda (c n e)
            (declare (ignore c))
            (when start
              (setf start nil)
              ;; contour should always start with a point
              (start-contour (* (p-rx n) sx) (* (p-ry n) sy)))
            (etypecase n
              (point)
              (segment (line-to (* (s-rx2 n) sx) (* (s-ry2 n) sy)))
              (bezier2 (quadratic-to
                        (* (b2-rxc n) sx) (* (b2-ryc n) sy)
                        (* (b2-rx2 n) sx) (* (b2-ry2 n) sy))))
            (when e
              (setf start t)
              (end-contour)))))))

#++
(defun clean-shape-old (shape &key (verbose *dump*))
  ;; return a copy of SHAPE with degenerate contours, curves,
  ;; segments, and points removed
  (let ((contours (make-hash-table)))
    (map-contour-segments shape (lambda (c n e)
                                  (declare (ignore e))
                                  (push n (gethash c contours))))
    (flet ((empty-bez (n)
             ;; b2 has 0 area if start and end points are same point
             (and (typep n 'bezier2)
                  (point= (b2-p1 n) (b2-p2 n))))
           (flat-bez (n)
             ;; b2 is flat if control point is same as 1 end, or all 3
             ;; have same X or Y value (if all 3 have same X and Y, it
             ;; should have been removed by 'empty' check already)
             (when (typep n 'bezier2)
               (let ((x1 (b2-rx1 n))
                     (y1 (b2-ry1 n))
                     (xc (b2-rxc n))
                     (yc (b2-ryc n))
                     (x2 (b2-rx2 n))
                     (y2 (b2-ry2 n)))
                 (or (and (= x1 xc) (= y1 yc))
                     (and (= xc x2) (= yc y2))
                     (= x1 xc x2)
                     (= y1 yc y2)
                     ;; also consider it flat if control point is
                     ;; within some small amount of line containing
                     ;; end points
                     (< (abs (dist/v2-line*/sf (p-dv (b2-c1 n))
                                               (p-dv (b2-p1 n))
                                               (p-dv (b2-p2 n))))
                        (* single-float-epsilon
                           (min (abs (- x1 x2))
                                (abs (- y1 y2)))))))))
           (empty-seg (n)
             (when (typep n 'segment)
               (and (= (s-rx1 n) (s-rx2 n))
                    (= (s-ry1 n) (s-ry2 n)))))
           (flat-seg (n)
             (when (typep n 'segment)
               (let ((x (when (= (s-rx1 n) (s-rx2 n)) (s-rx2 n)))
                     (y (when (= (s-ry1 n) (s-ry2 n)) (s-ry2 n))))
                 (when (or x y)
                   (list x y)))))
           (flat-seg-cont (f n)
             (when (typep n 'segment)
               (let ((x (when (= (s-rx1 n) (s-rx2 n)) (s-rx2 n)))
                     (y (when (= (s-ry1 n) (s-ry2 n)) (s-ry2 n))))
                 (when (or x y)
                   (equalp f (list x y)))))))
      (with-shape-builder (new :metadata (metadata shape))
        (loop for k in (a:hash-table-keys contours)
              ;; contour in reverse order
              for rev = (gethash k contours)
              ;; assuming contours start on a POINT, so end on a line or curve
              do (assert (not (typep (car rev) 'point)))
              do ;; remove degenerate curves
                 (setf rev
                       (loop with c = rev
                             for n = (pop c)
                             while n
                             when (empty-bez n)
                               ;; if start and end of curve are same point,
                               ;; it has 0 area so remove it and end point
                               do (when verbose
                                    (format t "~&dropped empty bezier ~s, and end-poipnt ~s~%" n (car c)))
                                  (pop c)
                             else
                               when (flat-bez n)
                                 ;; if control point is same as start or end
                                 ;; point, convert it to a segment. If
                                 ;; start/control/end all have same X or Y
                                 ;; value, convert it to a segment.
                                 do (when verbose
                                      (format t "~&convert flat bezier ~s to segment~%" n))
                                 and collect (make-segment/p (b2-p1 n) (b2-p2 n))
                             else collect n))
              do ;; remove degenerate segments
                 (setf rev
                       (loop with c = rev
                             for n = (pop c)
                             while n
                             when (empty-seg n)
                               ;; if start and end of segment are same point,
                               ;; remove it and end point
                               do (pop c)
                             else collect n))
              do ;; combine adjacent horizontal or vertical segments.
                 ;; (angled segments don't affect interior mask
                 ;; generation, so ignored for now. FP error would make
                 ;; them harder to detect anyway)
                 (let ((start (flat-seg (car rev)))
                       (end nil))
                   (setf rev
                         (loop
                           with c = rev
                           for n = (pop c)
                           for flat = (flat-seg n)
                           while n
                           when flat
                             do (setf end n)
                           when (and flat (flat-seg-cont flat (cadr c)))
                             ;; while next segment continues this one
                             do (when verbose
                                  (format t "~&joining flat segments after ~s:~%" n))
                                (loop while (flat-seg-cont flat (cadr c))
                                      ;; drop start point of next segment
                                      do (when verbose
                                           (format t "  ~s | ~s~%"
                                                   (car c) (cadr c)))
                                         (assert (typep (pop c) 'point))
                                         ;; and next segment
                                         (pop c))
                                ;; and then make a new segment
                                ;; from start of this one and the
                                ;; endpoint of last one we
                                ;; dropped, unless it returned
                                ;; back to the exact same point,
                                ;; and both segments were
                                ;; pointless :/
                             and collect (if (not (equalp (car c)
                                                          (s-p2 n)))
                                             (setf end
                                                   (make-segment/p (car c)
                                                                   (s-p2 n)))
                                             (progn
                                               (when verbose
                                                 (format t "dropped overlapped segments~%"))
                                               nil))
                           else collect n
                                and do (setf end nil)))
                   (setf rev (remove nil rev))
                   (when (and start (equalp (flat-seg end) start))

                     (break "todo: join segments that overlap start")))


                 ;; contour has no area if: no bez2 and 2 or fewer lines
                 ;; 1 bez2 and no lines.
              when (let ((lines (count-if 'segment-p rev))
                         (bez (count-if 'bezier2-p rev)))
                     (or (and (zerop bez) (< lines 3))
                         (and (zerop lines) (= bez lines 1))))
                do (when verbose
                     (format t "dropping degenerate contour: (~s nodes)~%"
                             (length rev))
                     (loop for i in rev
                           do (format t "  ~s~%" i)))
              else ;; add cleaned contour to new shape
              do (let* ((c (reverse rev))
                        (start (car c)))
                   (assert (typep start 'point))
                   (start-contour (p-rx start) (p-ry start))
                   (loop for prev = start then next
                         for (edge next) on (cdr c) by #'cddr
                         ;; make sure contour still makes sense
                         do (when next (assert (typep next 'point)))
                            (assert (typep edge '(or segment bezier2)))
                         do (etypecase edge
                              (point)
                              (segment
                               (line-to (s-rx2 edge) (s-ry2 edge)))
                              (bezier2
                               (quadratic-to (b2-rxc edge) (b2-ryc edge)
                                             (b2-rx2 edge) (b2-ry2 edge)))))
                   (end-contour)))))))

(defun clean-shape (shape &key (verbose *dump*))
  ;; return a copy of SHAPE with degenerate contours, curves,
  ;; segments, and points removed
  (let ((es (shape-to-edit-shape shape))
        (drop (make-hash-table)))
    ;; run in 2 passes, so we can detect things that were degenerate
    ;; before modifications, even if modifications would shift points
    ;; a bit
    (labels ((p1 (n)
               (point (eprev n)))
             (p2 (n)
               (point (enext n)))
             (p= (a b)
               (and (= (p-rx a) (p-rx b))
                    (= (p-ry a) (p-ry b))))
             (empty-bez (n)
               ;; b2 has 0 area if start and end points are same point
               (and (typep n 'es-contour-bezier2)
                    (point= (point (eprev n)) (point (enext n)))))
             (flat-bez (n)
               ;; b2 is flat if control point is same as 1 end, or all 3
               ;; have same X or Y value (if all 3 have same X and Y, it
               ;; should have been removed by 'empty' check already)
               (when (typep n 'es-contour-bezier2)
                 (let* ((p1 (p1 n))
                        (x1 (p-rx p1))
                        (y1 (p-ry p1))
                        (c1 (control-point n))
                        (xc (p-rx c1))
                        (yc (p-ry c1))
                        (p2 (p2 n))
                        (x2 (p-rx p2))
                        (y2 (p-ry p2)))
                   (or (and (= x1 xc) (= y1 yc))
                       (and (= xc x2) (= yc y2))
                       (= x1 xc x2)
                       (= y1 yc y2)
                       ;; also consider it flat if control point is
                       ;; within some small amount of line containing
                       ;; end points
                       (< (abs (dist/v2-line*/sf (p-dv c1)
                                                 (p-dv p1)
                                                 (p-dv p2)))
                          (* single-float-epsilon
                             (min (abs (- x1 x2))
                                  (abs (- y1 y2)))))))))
             (empty-seg (n)
               (when (typep n 'es-contour-segment)
                 (and (= (p-rx (p1 n)) (p-rx (p2 n)))
                      (= (p-ry (p1 n)) (p-ry (p2 n))))))
             (flat-seg (n)
               (when (typep n 'es-contour-segment)
                 (let ((x (when (= (p-rx (p1 n)) (p-rx (p2 n))) (p-rx (p2 n))))
                       (y (when (= (p-ry (p1 n)) (p-ry (p2 n))) (p-ry (p2 n)))))
                   (when (or x y)
                     (list x y)))))
             (flat-seg-cont (f n)
               (when (typep n 'es-contour-segment)
                 (let ((x (when (= (p-rx (p1 n)) (p-rx (p2 n))) (p-rx (p2 n))))
                       (y (when (= (p-ry (p1 n)) (p-ry (p2 n))) (p-ry (p2 n)))))
                   (when (or x y)
                     (equalp f (list x y))))))
             (backtrack (n)
               (unless (typep n 'es-contour-vertex)
                 ;; true if next node backtracks over this one
                 (let* ((s (p1 n)) ;; point at start of this node
                        (nn (enext (enext n))) ;; next node
                        (e (p2 nn)) ;; point at end of next node
                        (r (and (p= s e)
                                (or (and (typep n 'es-contour-segment)
                                         (typep nn 'es-contour-segment))
                                    (and (typep n 'es-contour-bezier2)
                                         (typep nn 'es-contour-bezier2)
                                         (p= (control-point n)
                                             (control-point nn)))))))
                   (when (and verbose r)
                     (format t "backtrack @ ~s~%" (enl n))
                     (format t "  s = ~s~%" (nl s))
                     (format t "  nn = ~s~%" (enl nn))
                     (format t "  e = ~s~%" (nl e))
                     (when (and (typep n 'es-contour-bezier2)
                                (typep nn 'es-contour-bezier2))
                       (format t "  cn = ~s~%" (nl (control-point n)))
                       (format t "  cnn = ~s~%" (nl (control-point nn)))))
                   r))))
      ;; flag degenerate nodes
      (setf (contours es)
            (loop for c in (contours es)
                  collect (map-modifying-contour
                           c
                           (lambda (n)
                             (when verbose (format t "p1: ~s~%" (enl n)))
                             (etypecase n
                               (es-contour-vertex)
                               (es-contour-segment
                                (cond
                                  ((empty-seg n)
                                   (when verbose
                                     (format t " flag ~s empty~%" (enl n)))
                                   (setf (gethash n drop) t))))
                               (es-contour-bezier2
                                (cond
                                  ((backtrack n)
                                   (when verbose
                                     (format t " flag ~s backtrack~%" (enl n))
                                     (format t " flag ~s backtrack2~%"
                                             (enl (enext (enext n)))))
                                   (setf (gethash n drop) t)
                                   (setf (gethash (enext (enext n)) drop) t))
                                  ((empty-bez n)
                                   (when verbose
                                     (format t " flag ~s empty~%" (enl n)))
                                   (setf (gethash n drop) t))
                                  ((flat-bez n)
                                   (when verbose
                                     (format t " bez ~s flat~%" (enl n)))
                                   ;; replace it with a segment
                                   (change-class n 'es-contour-segment)
                                   (when (empty-seg n)
                                     (break "!~s - ~s~%" (eprev n) (enext n))
                                     (setf (gethash n drop) t))))))
                             ;; return N since we don't modify contour
                             n))))
      ;; remove flagged nodes, and any others that become degenerate,
      ;; and merge flat segments
      (setf (contours es)
            (loop for c in (contours es)
                  do (when verbose (format t "-contour-~%"))
                  collect
                  (map-modifying-contour
                   c
                   (a:named-lambda x (n)
                     (let ((ret n)) ;; return node if we don't modify it
                       (when verbose (format t "~&p2 ~s~%" (enl n)))
                       (etypecase n
                         (es-contour-vertex)
                         (es-contour-segment
                          (cond
                            ((gethash n drop)
                             (when verbose (format t " drop ~s~%" (enl n)))
                             (setf ret (collapse-edge n)))
                            ((empty-seg n)
                             (when verbose (format t " empty ~s~%" (enl n)))
                             (setf ret (collapse-edge n)))
                            ((flat-seg n)
                             (let ((v (and verbose
                                           (flat-seg-cont
                                            (flat-seg n)
                                            (enext (enext n))))))
                               (when verbose
                                 (format t "flat ~s @ ~s~%"
                                         (flat-seg n)
                                         (enl n)))
                               (loop with f = (flat-seg n)
                                     for n1 = (enext n)
                                     for n2 = (enext n1)
                                     when (or (eql n1 n)
                                              (eql n2 n))
                                       ;; deleted contour, drop it
                                       do (return-from x nil)
                                     do (when verbose
                                          (format t " @ ~s = es: ~s, drop:~s, fsc:~s, neq:~s~%"
                                                  (enl n2)
                                                  (empty-seg n2)
                                                  (gethash n2 drop)
                                                  (flat-seg-cont f n2)
                                                  (not (eql n n2))))
                                     while (or (empty-seg n2)
                                               (gethash n2 drop)
                                               (and (flat-seg-cont f n2)
                                                    (not (eql n n2))))
                                     do (when verbose
                                          (format t "-- delete ~s~%" (enl n1))
                                          (format t "-- delete ~s~%" (enl n2)))
                                        (%delete-node n1)
                                        (%delete-node n2))
                               (when (empty-seg n)
                                 (when verbose (format t "collapse ~s~%" (enl n)))
                                 (setf ret (eprev (collapse-edge n)))
                                 (when verbose
                                   (format t "flat ~s < ~s > ~s~%"
                                           (flat-seg ret)
                                           (flat-seg-cont (flat-seg ret)
                                                          (eprev (eprev ret)))
                                           (flat-seg-cont (flat-seg ret)
                                                          (enext (enext ret))))))))))
                         (es-contour-bezier2
                          (cond
                            ((gethash n drop)
                             (when verbose (format t " drop2 ~s~%" (enl n)))
                             (setf ret (collapse-edge n))))))
                       (when (and verbose (not (eql n ret)))
                         (format t " ~s -> ~s~%" (enl n) (enl ret)))
                       ret)))))
      ;; remove backtracking nodes (in a separate pass so we can
      ;; delete them in place, which avoids problems with accidentally
      ;; deleting all of an odd-numbered run of backtracks, which
      ;; would leave a gap)
      (setf (contours es)
            (loop for c in (contours es)
                  do (when verbose
                       (format t "remove backtracks in contour:~%")
                       (map-modifying-contour
                        c (lambda (n) (format t "  ~s~%" (enl n)) n)))
                  collect (map-modifying-contour
                           c
                           (lambda (n)
                             (when verbose
                               (format t "bt? ~s~%" (enl n)))
                             (loop while (and n (backtrack n))
                                   do (when verbose
                                        (format t "delete backtrack~%  ~s~%  ~s~%  ~s~%  ~s~%"
                                                (enl n)
                                                (enl (enext n))
                                                (enl (enext (enext n)))
                                                (enl (enext (enext (enext n))))))
                                      (%delete-node (enext n))
                                      (%delete-node (enext n))
                                      (%delete-node (enext n))
                                      (setf n (%delete-node n))
                                      (when verbose
                                        (format t "   => ~s~%" (enl n))))
                             n))))

      ;; make sure we didn't leave any degenerate nodes
      (setf (contours es)
            (loop for c in (contours es)
                  collect (map-modifying-contour
                           c
                           (a:named-lambda x (n)
                             (let ((ret n)) ;; return node if we don't modify it
                               (etypecase n
                                 (es-contour-vertex)
                                 (es-contour-segment
                                  (cond
                                    ((empty-seg n)
                                     (break "new seg ~s~%" n)
                                     (setf ret (collapse-edge n)))
                                    ((flat-seg n)
                                     (loop with f = (flat-seg n)
                                           for n1 = (enext n)
                                           for n2 = (enext n1)
                                           when (or (eql n1 n)
                                                    (eql n2 n))
                                             ;; deleted contour, drop it
                                             do (return-from x nil)
                                           while (or (empty-seg n2)
                                                     (gethash n2 drop)
                                                     (and (flat-seg-cont f n2)
                                                          (not (eql n n2))))
                                           do (%delete-node n1)
                                              (%delete-node n2))
                                     (when (empty-seg n)
                                       (setf ret (eprev (collapse-edge n)))))))
                                 (es-contour-bezier2
                                  (cond
                                    ((empty-bez n)
                                     (break "new bez ~s~%" n)
                                     (setf ret (collapse-edge n))))))
                               (unless (eql n ret)
                                 (format t " ~s -> ~s~%" n ret))
                               ret)))))
      (setf (contours es)
            (remove-if 'degenerate-edit-contour (contours es))))

    (edit-shape-to-shape es :metadata (metadata shape))))

(defun %sort-edges (shape)
  (let ((edges nil))
    (map-contour-segments
     shape (lambda (c n e)
             (declare (ignore e))
             (etypecase n
               (point)
               (segment
                (let ((x1 (s-dx1 n))
                      (x2 (s-dx2 n))
                      (y1 (s-dy1 n))
                      (y2 (s-dy2 n)))
                  (list (min y1 y2) (max y1 y2) (min x1 x2) (max x1 x2) c n)))
               (bezier2
                (let ((x1 (b2-dx1 n))
                      (x2 (b2-dx2 n))
                      (x3 (b2-dxc n))
                      (y1 (b2-dy1 n))
                      (y2 (b2-dy2 n))
                      (y3 (b2-dyc n)))
                  (list (min y1 y2 y3) (max y1 y2 y3)
                        (min x1 x2 x3) (max x1 x2 x3)
                        c n))))))
    (sort edges '< :key 'car)))

(defun simplify-shape (shape &key samples)
  (declare (ignorable samples))
  shape)

(defun point-normal (shape point)
  (labels ((seg-tangent (s)
             (v2n (v2- (p-dv (s-p2 s))
                       (p-dv (s-p1 s)))))
           (prev-tangent (p)
             (let ((n (prev shape p)))
               (etypecase n
                 (segment (seg-tangent n))
                 (bezier2 (v2n (v2- (p-dv (b2-p2 n))
                                    (p-dv (b2-c1 n))))))))
           (next-tangent (p)
             (let ((n (next shape p)))
               (etypecase n
                 (segment (seg-tangent n))
                 (bezier2 (v2n (v2- (p-dv (b2-c1 n))
                                    (p-dv (b2-p1 n)))))))))
    (let* ((pt (prev-tangent point))
           (nt (next-tangent point))
           (tt (v2+ pt nt)))
      (if (< (v2mag tt) 0.001)
          (v2rx pt)
          (v2rx (v2n tt))))))

(defun corner-angles (shape)
  ;; return hash of point -> (signed angle . bisector)
  ;; todo: memoize this in shape?
  (let ((corner-angles (make-hash-table)))
    (labels ((t1 (n)
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
             (v2n* (x)
               (if (< (v2mag x) 0.001)
                   (progn
                     (break "degenerate segment?")
                     (v2 0.5 0.5))
                   (v2n x)))
             (b (t1 t2)
               (let* ((t1 (v2n* t1))
                      (t2 (v2n* t2))
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
