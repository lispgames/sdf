(in-package #:sdf/base)




(defclass shape ()
  ;; plist of things like code-point, font name, index,
  ;; whatever. mostly intended for things like atlas builders that
  ;; need to track properties of the input for later use
  ((metadata :initform nil :initarg :metadata :accessor metadata)

   ;; vector of starting points of each closed curve of the shape
   (contours :accessor contours
             :initform (make-array 1 :adjustable t :fill-pointer 0))
   ;; curve/line/point -> next or previous curve/line/point on contour
   (%next :reader %next :initform (make-hash-table))
   (%prev :reader %prev :initform (make-hash-table))

   ;; todo: remove this, no used anymore
   (point-tangents :initform (make-hash-table) :reader point-tangents)

   ;; flags indicating if a node is :CW, :CCW, or :BOTH, or NIL relative to an
   ;; arbitrary horizontal line:

   ;; For segments, :CW == y2>y1 | (y2=y1&&x2>x1), else :CCW (maybe
   ;; add ':flat' for y2=y1?).

   ;; for bezier2: :CW == (<= y1 yc y2), :CCW == (<= y2 yc y1), :BOTH
   ;; otherwise. (possibly detect and complain if (= y1 yc y2), since
   ;; that should be a segment?)

   ;; for point: set py,ny = control point of adjacent b2 if it isn't
   ;; same as point's y value or opposite end point otherwise, and far
   ;; endpoint of adjacent segment, NIL if (= py y ny), :cw = (<= py
   ;; y ny), :ccw == (<= ny y py), :both otherwise,

   ;; :BOTH for bezier means we have an inflection point, and it is CW
   ;; on one side and CCW on the other.

   ;; :BOTH for a point means the point is a local extreme point of
   ;; the shape, and can't start or end a span of "inside" or
   ;; "outside" points.

   ;; NIL means point is at the end of an 'inflection point'
   ;; horizontal segment, and point at beginning of horizontal edge
   ;; already marked the transition between 'inside' and 'outside'
   ;; along that span. If horizontal segment is a 'local extreme'
   ;; instead of an 'inflection point', either both ends are NIL or 1
   ;; end is CW and other is CCW (not sure yet, but doesn't matter
   ;; since samples along that segment have distance 0 regardless of sign)

   (bounding-box :reader bounding-box :initform (make-aabb))
   (rbounding-box :reader rbounding-box :initform (make-raabb))))

(defparameter *dump* nil)
#++
(setf *dump* t)

(defmacro with-shape-builder ((shape &key metadata) &body body)
  (alexandria:with-gensyms (contour point)
    `(let ((,shape (make-instance 'shape :metadata ,metadata))
           (,contour nil)
           (,point nil))
       (labels
           ((%update-aabb (x y)
              (update-aabb (bounding-box ,shape) x y)
              (update-raabb (rbounding-box ,shape) x y))

            (split-b/y (b)
              (let* ((v1 (p-v (b2-p1 b)))
                     (vc (b2-c1 b))
                     (v2 (p-v (b2-p2 b)))
                     (y1 (vy v1))
                     (yc (vy vc))
                     (y2 (vy v2))
                     (yc-y1 (- yc y1)))
                (when (= yc-y1 (- y2 yc))
                  (break "? ~s" b))
                ;; return T of extreme point on curve
                (/ yc-y1 (- yc-y1 (- y2 yc)))))
            (split-b/x (b)
              (let* ((v1 (p-v (b2-p1 b)))
                     (vc (b2-c1 b))
                     (v2 (p-v (b2-p2 b)))
                     (x1 (vx v1))
                     (xc (vx vc))
                     (x2 (vx v2))
                     (xc-x1 (- xc x1)))
                (when (= xc-x1 (- x2 xc))
                  (break "? ~s" b))
                ;; return T of extreme point on curve
                (/ xc-x1 (- xc-x1 (- x2 xc)))))
            (%update-aabb-for-b2 (b)
              ;; bezier2 can extend past its end points, so in that
              ;; case find the extrema and add them
              (let* ((x1 (b2-x1 b))
                     (y1 (b2-y1 b))
                     (xc (b2-xc b))
                     (yc (b2-yc b))
                     (x2 (b2-x2 b))
                     (y2 (b2-y2 b))
                     (x nil)
                     (y nil)
                     (xl (min x1 x2))
                     (xh (max x1 x2))
                     (yl (min y1 y2))
                     (yh (max y1 y2)))
                (unless (or (= xl xc xh) (< xl xc xh))
                  (let ((tt (split-b/x b)))
                    (setf x (funcall (if (> xc xh) 'max 'min)
                                     (a:lerp tt x1 xc)
                                     (a:lerp tt xc x2)))))
                (unless (or (= yl yc yh) (< yl yc yh))
                  (let ((tt (split-b/y b)))
                    (setf y (funcall (if (> yc yh) 'max 'min)
                                     (a:lerp tt y1 yc)
                                     (a:lerp tt yc y2)))))
                (when (or x y)
                  ;; todo: expand this to nearest 1/N of a pixel if
                  ;; values are otherwise rationals to keep bbox rational?
                  (%update-aabb (or x x1) (or y y1)))))
            (%add-point (x y)
              (when *dump* (format t "  add point ~s,~s~%" x y))
              (%update-aabb x y)
              (setf ,point (make-point x y))
              ,point)
            (start-contour (x y)
              (when *dump* (format t "start contour~%"))
              (when ,contour
                (error "can't start contour without ending previous contour"))
              (setf ,contour (fill-pointer (contours ,shape)))
              (%add-point x y)
              (vector-push-extend ,point (contours ,shape)))
            (line-to (x y)
              (when *dump* (format t "   line-to ~s,~s~%" x y))
              (with-point (,point px py))
              (let* ((prev ,point)
                     (p (%add-point x y))
                     (l (make-segment/p prev p)))
                (setf (next ,shape prev) l)
                (setf (prev ,shape l) prev)
                (setf (next ,shape l) p)
                (setf (prev ,shape p) l)))
            (end-contour (&key close)
              (when *dump* (format t "end contour (close ~s)~%" close))
              (assert ,contour)
              (let ((s (aref (contours ,shape) ,contour)))
                (ecase close
                  ((nil) ;; assume curve is closed
                   )
                  (:line ;; close with line
                   (with-point (s x y)
                     (line-to x y))))
                (unless (point= s ,point)
                  ;; todo: close-with-line restart
                  (error "contour not closed properly?~& ~s -> ~s"
                         s ,point))
                (let* ((prev (prev ,shape ,point)))
                  (assert (not (prev ,shape s)))
                  (assert (typep s 'point))
                  (etypecase prev
                    (segment
                     (when *dump*
                       (format t "relink last segment ~s -> ~s~%"
                               (s-p2 prev) s))
                     (assert (point= (s-p2 prev) ,point))
                     (setf (s-p2 prev) s))
                    (bezier2
                     (when *dump*
                       (format t "relink last bez ~s -> ~s~%"
                               (b2-p2 prev) s))
                     (assert (point= (b2-p2 prev) ,point))
                     (setf (b2-p2 prev) s)))
                  (setf (prev ,shape s) prev)
                  (setf (next ,shape prev) s)
                  (remhash ,point (%prev ,shape))
                  (remhash ,point (%next ,shape))))
              (setf ,contour nil)
              (setf ,point nil))
            (quadratic-to (cx cy x y)
              (when *dump*
                (format t "   quadratic-to ~s,~s   ~s,~s~%" cx cy x y))
              (let* ((prev ,point)
                     (p (%add-point x y))
                     (q (%make-bezier2 prev
                                       (v2 cx cy)
                                       p)))
                (%update-aabb-for-b2 q)
                (setf (next ,shape prev) q)
                (setf (prev ,shape q) prev)
                (setf (next ,shape q) p)
                (setf (prev ,shape p) q)))
            (add-metadata (key value)
              ;; not sure if this should allow duplicates or not?
              ;; for now allowing it, since sdf code doesn't care
              ;; either way
              (setf (metadata ,shape)
                    (list* key value (metadata ,shape)))))
         (declare (ignorable #'add-metadata #'quadratic-to #'line-to))
         ,@body
         ,shape))))

(defclass indexed-shape (shape)
  (;; vector of all points in shape
   (points :accessor points
           :initform (make-array 1 :adjustable t :fill-pointer 0))
   ;; vector of all line segments in shape
   (lines :accessor lines
          :initform (make-array 1 :adjustable t :fill-pointer 0))
   ;; vector of all curves in shape
   (curves :accessor curves
           :initform (make-array 1 :adjustable t :fill-pointer 0))
   ;; curve/line/point -> contour index
   (contour-index :reader contour-index :initform (make-hash-table))))

(defun map-contour-segments (shape function)
  (loop for c# from 0
        for c across (contours shape)
        do (loop for n = c then nn
                 for nn = (next shape n)
                 for end = (eql nn c)
                 do (funcall function c# n end)
                 until end)))

(defun map-contour-segments/transform (shape function transform)
  (loop for c# from 0
        for c across (contours shape)
        do (loop for n = c then nn
                 for nn = (next shape n)
                 for end = (eql nn c)
                 do (funcall function c# n end)
                 until end)))


(defun clean-shape (shape &key (verbose *dump*))
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
               (let ((x1 (b2-x1 n))
                     (y1 (b2-y1 n))
                     (xc (b2-xc n))
                     (yc (b2-yc n))
                     (x2 (b2-x2 n))
                     (y2 (b2-y2 n)))
                 (or (and (= x1 xc) (= y1 yc))
                     (and (= xc x2) (= yc y2))
                     (= x1 xc x2)
                     (= y1 yc y2)))))
           (empty-seg (n)
             (when (typep n 'segment)
               (and (= (s-x1 n) (s-x2 n))
                    (= (s-y1 n) (s-y2 n)))))
           (flat-seg (n)
             (when (typep n 'segment)
               (let ((x (when (= (s-x1 n) (s-x2 n)) (s-x2 n)))
                     (y (when (= (s-y1 n) (s-y2 n)) (s-y2 n))))
                 (when (or x y)
                   (list x y)))))
           (flat-seg-cont (f n)
             (when (typep n 'segment)
               (let ((x (when (= (s-x1 n) (s-x2 n)) (s-x2 n)))
                     (y (when (= (s-y1 n) (s-y2 n)) (s-y2 n))))
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
                         (loop with c = rev
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
                                    ;; and then make a new segment from
                                    ;; start of this one and the
                                    ;; endpoint of last one we dropped
                                 and collect (setf end
                                                   (make-segment/p (car c)
                                                                   (s-p2 n)))
                               else collect n
                                    and do (setf end nil)))
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
                   (start-contour (p-x start) (p-y start))
                   (loop for prev = start then next
                         for (edge next) on (cdr c) by #'cddr
                         ;; make sure contour still makes sense
                         do (when next (assert (typep next 'point)))
                            (assert (typep edge '(or segment bezier2)))
                         do (etypecase edge
                              (segment
                               (line-to (s-x2 edge) (s-y2 edge)))
                              (bezier2
                               (quadratic-to (b2-xc edge) (b2-yc edge)
                                             (b2-x2 edge) (b2-y2 edge)))))
                   (end-contour)))))))

(defun check-shape (shape)
  (map-contour-segments
   shape
   (lambda (c n e)
     (declare (ignorable c e))
     (assert (prev shape n))
     (assert (next shape n))
     (etypecase n
       (point)
       (segment
        (assert (eq (s-p1 n) (prev shape n)))
        (assert (eq (s-p2 n) (next shape n))))
       (bezier2
        (assert (eq (b2-p1 n) (prev shape n)))
        (assert (eq (b2-p2 n) (next shape n)))))))
  shape)
#++
(defmethod update-instance-for-different-class :after ((old shape)
                                                       (new indexed-shape)
                                                       &rest initargs
                                                       &key &allow-other-keys)
  (declare (ignore initargs))
                                        ;(check-shape old)
  (map-contour-segments
   new (lambda (c# s end)
         (declare (ignore end))
         (etypecase s
           (v:bezier2
            (vector-push-extend s (curves new)))
           (v:line
            (vector-push-extend s (lines new)))
           (v:point
            (vector-push-extend s (points new))))
         (setf (gethash s (contour-index new)) c#))))


(defclass sdf-shape (indexed-shape)
  ;; spatial index for contour
  ((qtree :reader qtree :initarg :qtree)))



(defmethod update-instance-for-different-class :after ((old shape)
                                                       (new sdf-shape)
                                                       &rest initargs
                                                       &key &allow-other-keys)
  (declare (ignore initargs))
  ;; todo: build qtree
  )



(defun next (shape element)
  (gethash element (%next shape)))

(defun (setf next) (new shape element)
  (setf (gethash element (%next shape)) new))


(defun prev (shape element)
  (gethash element (%prev shape)))

(defun (setf prev) (new shape element)
  (setf (gethash element (%prev shape)) new))



(defclass msdf-shape (sdf-shape)
  ;; point?/segment/curve -> #(R G B) flags (not sure if point should
  ;; go on here or not, since it might be on edge between 2 colors...
  ;; might keep it if it isn't though, to simplify that case?)
  ((coloring :reader coloring :initform (make-hash-table))))


(defmethod check-shape ((shape shape))
  (let ((starts (make-hash-table))
        (max (hash-table-count (%next shape))))
    (assert (= (hash-table-count (%next shape))
               (hash-table-count (%prev shape))))
    (loop for c across (contours shape)
          do (assert (not (gethash c starts)))
             (assert (gethash c (%next shape)))
             (assert (gethash c (%prev shape)))
             (setf (gethash c starts) c))
    (loop for c across (contours shape)
          do (loop for i from 0
                   for n = (next shape c) then (next shape n)
                   for np = (prev shape (next shape n))
                   for pn = (next shape (prev shape n))
                   do (assert (eql np n))
                      (assert (eql pn n))
                      (assert (not (eql n (next shape n))))
                      (assert (not (eql n (prev shape n))))
                   until (eql n c)
                   do (assert (not (gethash n starts)))
                      (assert (<= i max)))))
  shape)

#++
(defmethod check-shape ((shape indexed-shape))
  (call-next-method)
  (loop for p across (points shape)
        for next = (next shape p)
        for prev = (prev shape p)
        do (assert (and next prev))
           (assert (typep (gethash p (contour-index shape)) 'unsigned-byte))
           (assert (eql (type-of p) 'v:point))
           (assert (typep next '(or v:line v:bezier2)))
           (assert (typep prev '(or v:line v:bezier2))))

  (loop for p across (lines shape)
        for next = (next shape p)
        for prev = (prev shape p)
        do (assert (and next prev))
           (assert (typep (gethash p (contour-index shape)) 'unsigned-byte))
           (assert (eql (type-of p) 'v:line))
           (assert (eql (type-of next) 'v:point))
           (assert (eql (type-of prev) 'v:point)))

  (loop for p across(curves shape)
        for next = (next shape p)
        for prev = (prev shape p)
        do (assert (and next prev))
           (assert (typep (gethash p (contour-index shape)) 'unsigned-byte))
           (assert (eql (type-of p) 'v:bezier2))
           (assert (eql (type-of next) 'v:point))
           (assert (eql (type-of prev) 'v:point)))
  shape)
