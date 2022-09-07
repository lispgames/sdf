(in-package #:sdf/base)

(declaim (inline lerp))
(defun lerp (f a b)
  (+ (* (- 1 f) a) (* f b)))


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
  (alexandria:with-gensyms (contour point point-x point-y)
    `(let ((,shape (make-instance 'shape :metadata ,metadata))
           (,contour nil)
           (,point nil)
           ;; store copy of original coords so we can calculate bounds of bez2
           ;; with rationals if input is rational
           (,point-x nil)
           (,point-y nil))
       (labels
           ((%update-aabb (x y)
              (update-aabb (bounding-box ,shape) x y)
              (update-raabb (rbounding-box ,shape) x y))

            (split-b/y (y1 yc y2)
              (let* ((yc-y1 (- yc y1)))
                (when (= yc-y1 (- y2 yc))
                  (break "? ~s ~s ~s" y1 yc y2))
                ;; return T of extreme point on curve
                (/ yc-y1 (- yc-y1 (- y2 yc)))))
            (split-b/x (x1 xc x2)
              (let* ((xc-x1 (- xc x1)))
                (when (= xc-x1 (- x2 xc))
                  (break "? ~s ~s ~s" x1 xc x2))
                ;; return T of extreme point on curve
                (/ xc-x1 (- xc-x1 (- x2 xc)))))
            (%update-aabb-for-b2 (x1 y1 xc yc x2 y2)
              ;; bezier2 can extend past its end points, so in that
              ;; case find the extrema and add them
              (let* ((x nil)
                     (y nil)
                     (xl (min x1 x2))
                     (xh (max x1 x2))
                     (yl (min y1 y2))
                     (yh (max y1 y2)))
                (unless (or (= xl xc xh) (< xl xc xh))
                  (let ((tt (split-b/x x1 xc x2)))
                    (setf x (funcall (if (> xc xh) 'max 'min)
                                     (lerp tt x1 xc)
                                     (lerp tt xc x2)))))
                (unless (or (= yl yc yh) (< yl yc yh))
                  (let ((tt (split-b/y y1 yc y2)))
                    (setf y (funcall (if (> yc yh) 'max 'min)
                                     (lerp tt y1 yc)
                                     (lerp tt yc y2)))))
                (when (or x y)
                  ;; todo: expand this to nearest 1/N of a pixel if
                  ;; values are otherwise rationals to keep bbox rational?
                  (%update-aabb (or x x1) (or y y1)))))
            (%add-point (x y)
              (when *dump* (format t "  add point ~s,~s~%" x y))
              (%update-aabb x y)
              (setf ,point-x x)
              (setf ,point-y y)
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
              #++(with-rpoint (,point px py))
              (let* ((prev ,point)
                     (p (%add-point x y))
                     (l (make-segment/p prev p)))
                (setf (next ,shape prev) l)
                (setf (prev ,shape l) prev)
                (setf (next ,shape l) p)
                (setf (prev ,shape p) l)))
            (end-contour (&key close)
              (when *dump*
                (format t "end contour (close ~s)~%" close)
                (format t "  rbounds = ~s~%" (rbounding-box ,shape)))
              (assert ,contour)
              (let ((s (aref (contours ,shape) ,contour)))
                (ecase close
                  ((nil) ;; assume curve is closed
                   )
                  (:line ;; close with line
                   (with-rpoint (s x y)
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
                       (format t "relink last segment ~s~% -> ~s~%"
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
                     (px ,point-x)
                     (py ,point-y)
                     (p (%add-point x y))
                     (q (%make-bezier2 prev
                                       (make-point cx cy)
                                       p)))
                (%update-aabb-for-b2 px py cx cy x y)
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

(defun next2 (shape element)
  (next shape (next shape element)))

(defun (setf next) (new shape element)
  (setf (gethash element (%next shape)) new))


(defun prev (shape element)
  (gethash element (%prev shape)))

(defun (setf prev) (new shape element)
  (setf (gethash element (%prev shape)) new))

(defun prev2 (shape element)
  (prev shape (prev shape element)))


(defclass msdf-shape (sdf-shape)
  ;; point?/segment/curve -> #(R G B) flags (not sure if point should
  ;; go on here or not, since it might be on edge between 2 colors...
  ;; might keep it if it isn't though, to simplify that case?)
  ((coloring :reader coloring :initform (make-hash-table))))

(defmethod check-shape ((shape shape))
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

