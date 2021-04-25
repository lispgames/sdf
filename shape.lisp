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

   ;; NIL means point is in the middle of 2 co-linear horizontal
   ;; segments, in which case it could be removed without affecting
   ;; shape of curve, so should be ignored here as well
   (windings :reader windings :initform (make-hash-table))

   (bounding-box :reader bounding-box :initform (make-aabb))))

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
              (update-aabb (bounding-box ,shape) x y))
            (%add-point (x y)
              (when *dump* (format t "  add point ~s,~s~%" x y))
              (%update-aabb x y)
              (setf ,point (make-point x y))
              ;;(vector-push-extend ,point (points ,shape))
              ;;(setf (gethash ,point (contour-index ,shape)) ,contour)
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
              (with-point (,point px py)
                (when (and (= x px) (= y py))
                  (warn "dropping degenerate line ~s .. ~s,~s" ,point x y)
                  (return-from line-to nil)))
              (let* ((prev ,point)
                     (p (%add-point x y))
                     (l (make-segment/p prev p)))
                (setf (next ,shape prev) l)
                (setf (prev ,shape l) prev)
                (setf (next ,shape l) p)
                (setf (prev ,shape p) l)
                ;;(vector-push-extend l (lines ,shape))
                ;;(setf (gethash l (contour-index ,shape) ,contour))
                ))
            #++
            (prev-tan (p)
              (let ((a (prev ,shape p)))
                (etypecase a
                  (segment
                   (v2n (v2- (p-v (s-p2 a))
                             (p-v (s-p1 a)))))
                  (bezier2
                   (v2n (v2- (p-v (b2-p2 a))
                             (b2-c1 a)))))))
            #++
            (next-tan (p)
              (let ((a (next ,shape p)))
                (etypecase a
                  (segment
                   (v2n (v2- (p-v (s-p2 a))
                             (p-v (s-p1 a)))))
                  (bezier2
                   (v2n (v2- (b2-c1 a)
                             (p-v (b2-p1 a))))))))
            #++
            (add-point-tangents ()
              (loop with p0 = (aref (contours ,shape),contour)
                    with p = p0
                    for i from 0
                    when (> i 1000)
                      do (break "~s" ,shape)
                    when (typep p 'point)
                      do (let* ((pt (prev-tan p))
                                (tt (v2+ pt (next-tan p))))
                           (if (and (< (abs (vx tt)) 0.001)
                                    (< (abs (vy tt)) 0.001))
                               (setf tt (v2rx pt))
                               (setf tt (v2n tt)))
                           (setf (gethash p (point-tangents ,shape)) tt))
                    do (setf p (next ,shape p))
                    until (eq p p0)))
            (mark-contour-windings ()
              (loop with p0 = (aref (contours ,shape),contour)
                    with p = p0
                    for i from 0
                    when (> i 1000)
                      do (break "~s" ,shape)
                    do (etypecase p
                         (null (break "~s" p))
                         (point
                          (let* ((prev (prev ,shape p))
                                 (next (next ,shape p))
                                 (y (p-y p))
                                 (py (etypecase prev
                                       (null (break "~s" p) y)
                                       (segment
                                        (s-y1 prev))
                                       (bezier2
                                        (if (= y (b2-yc prev))
                                            (b2-y1 prev)
                                            (b2-yc prev)))))
                                 (ny (etypecase next
                                       (null (break "~s" p) y)
                                       (segment
                                        (s-y2 next))
                                       (bezier2
                                        (if (= y (b2-yc next))
                                            (b2-y2 next)
                                            (b2-yc next))))))
                            (setf (gethash p (windings ,shape))
                                  (cond
                                    ((= py y ny) nil)
                                    ((<= py y ny) :cw)
                                    ((>= py y ny) :ccw)
                                    (t :both)))))
                         (segment
                          (setf (gethash p (windings ,shape))
                                (if (or (> (s-y2 p) (s-y1 p))
                                        (and (= (s-y2 p) (s-y1 p))
                                             (> (s-x2 p) (s-x1 p))))
                                    :cw :ccw)))
                         (bezier2
                          (let ((y1 (b2-y1 p))
                                (yc (b2-yc p))
                                (y2 (b2-y2 p)))
                            (setf (gethash p (windings ,shape))
                                  (cond
                                    ((<= y1 yc y2) :cw)
                                    ((>= y2 yc y1) :ccw)
                                    (t :both))))))
                       (setf p (next ,shape p))
                    until (eq p p0)))
            (end-contour (&key close)
              (when *dump* (format t "end contour (close ~s)~%" close))
              (assert ,contour)
              (let ((s (aref (contours ,shape) ,contour)))
                ;; todo: drop empty contours
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
                  (setf (prev ,shape s) prev)
                  (setf (next ,shape prev) s)
                  (remhash ,point (%prev ,shape))
                  (remhash ,point (%next ,shape))))
              #++
              (add-point-tangents)
              (mark-contour-windings)
              (setf ,contour nil)
              (setf ,point nil))
            (quadratic-to (cx cy x y)
              (when *dump*
                (format t "   quadratic-to ~s,~s   ~s,~s~%" cx cy x y))
              (with-point (,point px py)
                (when (or (and (= px cx) (= py cy))
                          (and (= cx x) (= cy y)))
                  (warn "converting degenerate quadratic to segment, ~s- ~s,~s - ~s,~s"
                        ,point cx cy x y)
                  (Return-From quadratic-to
                    (line-to px py)))

                (when (and (= x px) (= y py))
                  (break "dropping degenerate quadratic ~s .. ~s,~s .. ~s,~s"
                         ,point cx cy x y)
                  (return-from quadratic-to nil)))
              (%update-aabb cx cy)
              (let* ((prev ,point)
                     (p (%add-point x y))
                     (q (%make-bezier2 prev
                                       (v2 cx cy)
                                       p)))
                (setf (next ,shape prev) q)
                (setf (prev ,shape q) prev)
                (setf (next ,shape q) p)
                (setf (prev ,shape p) q)
                ;;(vector-push-extend q (curves ,shape))
                ;;(setf (gethash q (contour-index ,shape) ,contour))
                ))
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
