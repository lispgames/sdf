#++
(ql:quickload '(sdf))
(defpackage #:sdf/cleaner
  (:use :cl)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:b #:sdf/base)
                    (#:rb #:sdf/rb)
                    (#:q #:sdf/f+f-pqueue)
                    (#:dq #:sdf/df-pqueue)))

(in-package #:sdf/cleaner)

(defvar *check* t) ;; enable expensive consistency checks

;; debugging vars, used by vis and test code in clean-scratch
(defvar *shapes* ())
(defvar *error-shapes* ())

;; rb-tree sorting uses a unique ID field to ensure a stable
;; ordering. Id generation isn't thread safe for now, so bind locally
;; if that matters
(defvar *id* 0)

(defclass intersect-edge ()
  ;; wrapper for a curve or segment in sweepline rb tree.
  ((edge :initarg :edge :reader edge)
   ;; valid T range for active edge, top first (so might be 1-0), and
   ;; might be partial range if a curve was split at an extreme
   (t1 :initarg :t1 :accessor t1)
   (t2 :initarg :t2 :accessor t2)
   ;; +1 if upwards or edge, -1 if downwards edge
   (winding-sign :reader winding-sign)
   ;; winding number to right of edge while in sweepline rb tree
   (winding-number :initform nil :accessor winding-number)
   (previous-winding-number :initform nil :accessor previous-winding-number)
   ;; true if edge is (currently) a transition to or from 0 winding
   ;; (can't be updated or detected automatically because it needs to
   ;; account for join point of 2 adjacent segments, where both need
   ;; to be on boundary but we can't add both when calculating winding
   ;; number (actually might not care about that case anymore, but
   ;; still gets modified to account for stacked edges canceling out))
   (on-boundary :initform nil :accessor on-boundary)
   ;; to decide how to connect edges at an intersection, we need to
   ;; know which side (if any) of edge was inside or outside the final
   ;; shape before the edge reached the intersection. We can't just
   ;; look at values before updating them while looking at
   ;; intersection, since a previous intersection on same line might
   ;; have caused it to be updated already. Instead, we store a value
   ;; from some previous Y value, and use that.
   ;;
   ;; for now, storing a list of (y win boundary wout). probably
   ;; should replace with separate slots at some point?
   (previous-windings :initform nil :accessor previous-windings)
   ;; store point of any y extrema so we can use that directly when
   ;; calculating X for that Y value
   (split-point :initarg :split-point :reader split-point :initform nil)
   ;; store coordinates of point with maximum Y value, so we can use
   ;; it to decide if this edge contributes to winding #.
   (y-max :reader y-max :initform nil)
   ;; cached X value for sorting, calculated at specified Y
   (x)
   ;; cached T value for sorting, calculated at specified Y
   (at)
   ;; X component of tangent for sorting, calculated at specific y
   (tangent-x :initform nil)
   ;; cached curvature for sorting, calculated at specific y
   (curvature :initform nil)
   (y :initform nil :reader y)
   ;; fixnum used to order rb-tree nodes that would otherwise sort as
   ;; same value
   (id :reader id)
   ;; ref back to rb tree node, so we don't have to FIND to delete it
   (%node :initform nil :accessor %node)))

(defmethod initialize-instance :after ((o intersect-edge) &key)
  (incf *id*)
  (when (> *id* most-positive-fixnum)
    (setf *id* most-negative-fixnum))
  ;; since we split at y extrema, and t1/t2 are ordered by increasing
  ;; Y, winding sign can be calculated directly from those
  (assert (/= (t1 o) (t2 o)))
  (when (slot-boundp o 'winding-sign)
    (assert (= (winding-sign o) (signum (- (t1 o) (t2 o))))))
  (setf (slot-value o 'winding-sign) (signum (- (t1 o) (t2 o))))
  (let ((e (edge o)))
    ;; find y value of last endpoint
    (let ((p1 (b::eval-at (edge o) (t1 o)))
          (p2 (b::eval-at (edge o) (t2 o))))
      (setf (slot-value o 'y-max) (max (b::vy p1) (b::vy p2))))
    ;; tangent/curvature of segment doesn't change, so set it once on init
    (when (typep e 'b::segment)
      (let* ((x1 (b::s-dx1 e))
             (x2 (b::s-dx2 e))
             (y1 (b::s-dy1 e))
             (y2 (b::s-dy2 e))
             (dx (- x2 x1))
             (dy (- y2 y1))
             (l (sqrt (+ (expt dx 2) (expt dy 2))))
             (s (signum (- (t2 o) (t1 o)))))
        (setf (slot-value o 'tangent-x) (* s (/ dx l)))
        (setf (slot-value o 'curvature) 0d0))))
  (setf (slot-value o 'id) *id*))

(defun maybe-winding-sign (n)
  ;; don't count this edge if it is at the endpoint with highest Y
  ;; value.
  (if (= (y n) (y-max n))
      0
      (winding-sign n)))

(defmethod (setf winding-number) :before (n (o intersect-edge))
  (setf (previous-winding-number o) (winding-number o)))

(defclass horizontal-intersect-edge (intersect-edge)
  ;; store right endpoint of horizontal edges so we don't have to
  ;; calculate it from EDGE every time
  ((x2 :initarg :x2 :reader x2)))

(defmethod initialize-instance :after ((o horizontal-intersect-edge) &key x)
  ;; not usually called since we make horizontal edges with
  ;; change-class
  (setf (slot-value o 'y) (y-max o))
  (setf (slot-value o 'x) x)
  (setf (slot-value o 'winding-sign) 0))

(defmethod update-instance-for-different-class
    :after (p (o horizontal-intersect-edge) &key x)
  (setf (slot-value o 'y) (y-max o))
  (setf (slot-value o 'x) x)
  (setf (slot-value o 'winding-sign) 0))



(defun %x-at (node y tmin tmax split-point &key (errorp t))
  (declare (type double-float tmin tmax))
  (when (> tmin tmax)
    (rotatef tmin tmax))
  (flet ((x (n at)
           (let ((1-at (- 1 at)))
             (+ (* (b::b2-dx1 n)
                   (expt 1-at 2))
                (* (b::b2-dxc n)
                   (* 2 at 1-at))
                (* (b::b2-dx2 n)
                   (expt at 2))))))
    (let ((n node))
      (cond
        (n
         (etypecase n
           (b::point
            (cond
              ((= (b::p-dy n) y)
               (values (b::p-dx n) 0d0))
              (t (when errorp
                   (error "calculating X value for point on strip at wrong value?~%expected y=~s, got ~s (~s)"
                          y (b::p-dy n) (b::p-ry n))))))
           (b::segment
            (let ((y1 (b::s-dy1 n))
                  (y2 (b::s-dy2 n)))
              (cond
                ((= y1 y2)
                 (let ((x1 (b::s-dx1 n))
                       (x2 (b::s-dx2 n)))
                   (if (< x1 x2)
                       (values x1 0)
                       (values x2 1))))
                ((or (<= y1 y y2) (<= y2 y y1))
                 (let* ((at (/ (- y y1) (- y2 y1)))
                        (x1 (b::s-dx1 n))
                        (x2 (b::s-dx2 n)))
                   (when (or (< at 0) (> at 1))
                     (break "bat at ~s?" at))
                   (values (+ x1 (* (- x2 x1) at))
                           at)))
                (t
                 (when errorp
                   (error "calculating X value for Y outside valid range of segment?~% y = ~s,~%  expected ~s - ~s (~s - ~s)"
                          y y1 y2 (b::s-ry1 n) (b::s-ry2 n)))))))
           (b::bezier2
            (multiple-value-bind (t1 t2) (b::%b2-find-t (b::b2-dy1 n)
                                                        (b::b2-dyc n)
                                                        (b::b2-dy2 n)
                                                        y)
              (cond
                ((and split-point (< (abs (- (b::vy split-point) y))
                                     (b::%bcs-eps n)))
                 (values (b::vx split-point)
                         (if (< 0 tmin 1) tmin tmax)))
                ((not t1)
                 (when errorp
                   (error "calculating X value for Y that doesn't intersect curve?")))
                ((not t2)
                 (if (<= tmin t1 tmax)
                     (values (x n t1) t1)
                     (let ((p1 (b::eval-at/b2/fast n t1))
                           (e1 (b::eval-at/b2/fast n tmin))
                           (e2 (b::eval-at/b2/fast n tmax))
                           (eps (* 4 (b::%bcs-eps n))))
                       (cond
                         ((b::~ (b::vy p1) (b::vy e1) eps)
                          (values (b::vx e1) tmin))
                         ((b::~ (b::vy p1) (b::vy e2) eps)
                          (values (b::vx e2) tmax))
                         (t (when errorp
                              (error "calculating X value for Y outside valid range of curve?~% y=~s (~s, ~s, ~s)~% t = ~s (~s - ~s)~%"
                                     y
                                     (b::b2-dy1 n)
                                     (b::b2-dyc n)
                                     (b::b2-dy2 n)
                                     t1 tmin tmax)))))))
                ((<= tmin t1 tmax)
                 (if (<= tmin t2 tmax)
                     (if errorp
                         (error "found 2 X values for Y in curve?~% y=~s~% t = ~s, ~s (~s - ~s)~% x= ~s, ~s~%"
                                y
                                t1 t2 tmin tmax
                                (x n t1) (x n t2))
                         (break "found 2 X values for Y in curve?~% y=~s~% t = ~s, ~s (~s - ~s)~% x= ~s, ~s~%"
                                y
                                t1 t2 tmin tmax
                                (x n t1) (x n t2)))
                     (values (x n t1) t1)))
                ((<= tmin t2 tmax)
                 (values (x n t2) t2))
                (t
                 (when errorp
                   (error "calculating X value for Y outside valid range of curve?~% y=~s (~s, ~s, ~s)~% t = ~s, ~s (~s - ~s)~% split ~s (~s)"
                          y
                          (b::b2-dy1 n)
                          (b::b2-dyc n)
                          (b::b2-dy2 n)
                          t1 t2 tmin tmax
                          split-point (when split-point
                                        (abs (- (b::vy split-point) y)))))))))))
        (t
         (when errorp
           (error "tried to calculate X of empty contour-strip?~%")))))))

(defun df (x) (coerce x 'double-float))

(defun update-cached (strip y)
  (assert y)
  (unless (and (y strip) (= (y strip) y))
    (let ((e (edge strip)))
      (when (typep e 'b::bezier2)
        ;; we only need sorting info when we have 2 points at same X,
        ;; which should be rare, so just clear it here and recalculate
        ;; if used.
        (setf (slot-value strip 'tangent-x) nil)
        (setf (slot-value strip 'curvature) nil))
      (setf (values (slot-value strip 'x)
                    (slot-value strip 'at))
            (%x-at e y (df (t1 strip)) (df (t2 strip))
                   (split-point strip) :errorp t)))
    (setf (slot-value strip 'y) y)))

(defun %update-cached* (strip y x at &optional force)
  (assert y)
  (when (and *check* (not force) (y strip) (= (y strip) y))
    (assert (< (abs (- (slot-value strip 'at) at))
               (b::%bcs-eps (edge strip))))
    (assert (= (slot-value strip 'x) x)))
  (when (or force (not (and (y strip) (= (y strip) y))))
    (let ((e (edge strip)))
      (when (typep e 'b::bezier2)
        ;; we only need sorting info when we have 2 points at same X,
        ;; which should be rare, so just clear it here and recalculate
        ;; if used.
        (setf (slot-value strip 'tangent-x) nil)
        (setf (slot-value strip 'curvature) nil))
      (setf (slot-value strip 'x) x
            (slot-value strip 'at) at))
    (setf (slot-value strip 'y) y)))


(defun x-at (strip y)
  (update-cached strip y)
  (slot-value strip 'x))

(defun t-at (strip y)
  (update-cached strip y)
  (slot-value strip 'at))

(defun %tangent-x-at (strip y)
  ;; derivative at point, projected onto X axis
  (update-cached strip y)
  (cond
    ((slot-value strip 'tangent-x) ;; return directly if set
     )
    (t
     ;; (we never clear ANGLE slot for SEGMENTs, so this is only
     ;; called for BEZIER2s)
     (let* ((e (edge strip))
            (at (slot-value strip 'at))
            (x1 (b::b2-dx1 e))
            (y1 (b::b2-dy1 e))
            (xc (b::b2-dxc e))
            (yc (b::b2-dyc e))
            (x2 (b::b2-dx2 e))
            (y2 (b::b2-dy2 e))
            ;; dx/dt,dy/dt. Should be *2, but we normalize it so can
            ;; factor it out
            (x3 (- xc x1))
            (y3 (- yc y1))
            (x4 (- x2 xc))
            (y4 (- y2 yc))
            (dx (a:lerp at x3 x4))
            (dy (a:lerp at y3 y4))
            (l (sqrt (+ (expt dx 2) (expt dy 2))))
            (s (signum (- (t2 strip) (t1 strip)))))
       (assert (not (zerop l)))
       (setf (slot-value strip 'tangent-x)
             (* s (/ dx l)))))))

(defun %curvature-at (strip y)
  ;; not sure if this should be calculated as a 2nd value of
  ;; TANGENT-X-AT or not. For now, assuming this will be much more
  ;; rarely used, so better to duplicate some calculations here.
  (update-cached strip y)
  (cond
    ((slot-value strip 'curvature) ;; return directly if set
     )
    (t
     ;; (we never clear DERIVATIVE slot for SEGMENTs, so this is only
     ;; called for BEZIER2s)
     (let* ((e (edge strip))
            (x1 (b::b2-dx1 e))
            (y1 (b::b2-dy1 e))
            (xc (b::b2-dxc e))
            (yc (b::b2-dyc e))
            (x2 (b::b2-dx2 e))
            (y2 (b::b2-dy2 e))
            (at (slot-value strip 'at)))
       ;; flip end points to match direction we care about
       (when (< (t2 strip) (t1 strip))
         (rotatef x1 x2)
         (rotatef y1 y2)
         (setf at (- 1 at)))
       (let* (;; coefficients of 1st derivative, = linear bezier
              (x3 (* 2 (- xc x1)))
              (y3 (* 2 (- yc y1)))
              (x4 (* 2 (- x2 xc)))
              (y4 (* 2 (- y2 yc)))
              ;; 1st derivative with respect to t
              (dx (a:lerp at x3 x4))
              (dy (a:lerp at y3 y4))
              ;; 2nd derivative with respect to t
              (d2x (- x4 x3))
              (d2y (- y4 y3))
              ;; curvature
              (k (- (/ (- (* dx d2y)
                          (* dy d2x))
                       (expt (+ (expt dx 2)
                                (expt dy 2))
                             3/2)))))
         (setf (slot-value strip 'curvature) k))))))

(defun %curvature-derivative-at (strip y)
  ;; possibly should be calculated with curvature, but seems to be
  ;; very rare, so separate for now and not cached
  (typecase (edge strip)
    (b::segment
     0d0)
    (b::bezier2
     (update-cached strip y)
     (let* ((e (edge strip))
            (x1 (b::b2-dx1 e))
            (y1 (b::b2-dy1 e))
            (xc (b::b2-dxc e))
            (yc (b::b2-dyc e))
            (x2 (b::b2-dx2 e))
            (y2 (b::b2-dy2 e))
            (at (slot-value strip 'at)))
       ;; flip end points to match direction we care about
       (when (< (t2 strip) (t1 strip))
         (rotatef x1 x2)
         (rotatef y1 y2)
         (setf at (- 1 at)))
       (let* (;; coefficients of 1st derivative, = linear bezier
              (x3 (* 2 (- xc x1)))
              (y3 (* 2 (- yc y1)))
              (x4 (* 2 (- x2 xc)))
              (y4 (* 2 (- y2 yc)))
              ;; 1st derivative with respect to t
              (dx (a:lerp at x3 x4))
              (dy (a:lerp at y3 y4))
              ;; 2nd derivative with respect to t
              (d2x (- x4 x3))
              (d2y (- y4 y3))
;;; see https://www.wolframalpha.com/input?i=%28x%27%28t%29y%27%27%28t%29-y%27%28t%29x%27%27%28t%29%29%2F%28%28%28x%27%28t%29%29%5E2%2B%28y%27%28t%29%29%5E2%29%5E%283%2F2%29%29'
;;;; (2(x′² + y′²)(y‴x′ - x‴y′) + 6(x″y′ - x′y″) (x′x″ + y′y″))/(2(x′² + y′²)^(5/2))
;;;;
;;;; if y‴=x‴=0, then k′=(3(x″y′ - x′y″)(x′x″ + y′y″))/((x′² + y′²)^(5/2))

;;;<selwynning> unless you are careful to differentiate with respect to arc
;;;             length
;;;<selwynning> the arc length itself is related to the curvature though
;;;<selwynning> so it might be possible to get a useful expression for that
;;;<selwynning> ok, so you've worked out dκ/dt
;;;<selwynning> what you want is dκ/ds
;;;<selwynning> where s is arc length
;;;<selwynning> so multiply that by dt/ds
;;;<selwynning> which is 1/sqrt(x' ^2 + y' ^2)

              ;; derivative of curvature with respect to arc length
              (dkds (/ (* 3
                          (- (* d2x dy) (* dx d2y))
                          (+ (* dx d2x) (* dy d2y)))
                       (expt (+ (expt dx 2) (expt dy 2)) 3))))
         dkds)))))

(defmethod is-extreme ((p b::point) shape)
  (labels ((prev-y ()
             (let ((prev (b::prev shape p)))
               (etypecase prev
                 (b::segment
                  (b::s-ry1 prev))
                 (b::bezier2
                  (if (= (b::b2-ryc prev) (b::p-ry p))
                      (b::b2-ry1 prev)
                      (b::b2-ryc prev))))))
           (next-y ()
             (let ((next (b::next shape p)))
               (etypecase next
                 (b::segment
                  (b::s-ry2 next))
                 (b::bezier2
                  (if (= (b::b2-ryc next) (b::p-ry p))
                      (b::b2-ry2 next)
                      (b::b2-ryc next)))))))
    (let ((ym (b::p-ry p))
          (yp (prev-y))
          (yn (next-y)))
      (cond
        ((and (< ym yp) (< ym yn)) 1)
        ((and (> ym yp) (> ym yn)) -1)
        ((and (= ym yp) (= ym yn))
         (error "shouldn't happen1?"))
        (t nil)))))

(defmethod is-extreme ((s b::segment) shape)
  ;; a horizontal segment might count as an extreme, will pick an
  ;; endpoint when splitting
  (when (= (b::s-ry1 s) (b::s-ry2 s))
    (labels ((prev-y ()
               (let ((prev (b::prev2 shape s)))
                 (etypecase prev
                   (b::segment
                    (b::s-ry1 prev))
                   (b::bezier2
                    (if (= (b::b2-ryc prev) (b::s-ry1 s))
                        (b::b2-ry1 prev)
                        (b::b2-ryc prev))))))
             (next-y ()
               (let ((next (b::next2 shape s)))
                 (etypecase next
                   (b::segment
                    (b::s-ry2 next))
                   (b::bezier2
                    (if (= (b::b2-ryc next) (b::s-ry1 s))
                        (b::b2-ry2 next)
                        (b::b2-ryc next)))))))
      (let ((ym (b::s-ry1 s))
            (yp (prev-y))
            (yn (next-y)))
        (cond
          ((and (< ym yp) (< ym yn)) 1)
          ((and (> ym yp) (> ym yn)) -1)
          ((or (= ym yp) (= ym yn))
           (error "shouldn't happen?(horizontal segment with same y adjacent)"))
          (t nil))))))

(defmethod is-extreme ((b b::bezier2) shape)
  ;; only an extreme if control point is above or below both end
  ;; points
  (let ((ym (b::b2-ryc b))
        (yp (b::b2-ry1 b))
        (yn (b::b2-ry2 b)))
    (cond
      ((and (< ym yp) (< ym yn)) 1)
      ((and (> ym yp) (> ym yn)) -1)
      ((and (= ym yp) (= ym yn))
       (error "shouldn't happen?(flat bezier)"))
      (t nil))))

(defclass sweep-event ()
  ((y :initarg :y :reader y)
   (x :initarg :x :reader x)))

(defclass start-event (sweep-event)
  ((start :initarg :start :Reader start)
   (at :initarg :at :reader at)))

(defclass horizontal-event (sweep-event)
  ;; start of a horizontal edge
  ((edge :initarg :edge :reader edge)
   ;; right endpoint of edge
   (x2 :initarg :x2 :reader x2)))

(defclass end-event (sweep-event)
  ((end :initarg :end :Reader end)
   (at :initarg :at :reader at)))

;; since contours are continuous, we usually add and remove an edge at
;; the same time
;;; todo: use this and optimize for that case
#++
(defclass add-remove-event (start-event end-event)
  ())

;; future intersection
(defclass intersect-event (sweep-event)
  ((left :initarg :left :accessor left)
   (at1 :initarg :at1 :reader at1)
   (right :initarg :right :accessor right)
   (at2 :initarg :at2 :reader at2)))

(defclass horizontal-intersect-event (sweep-event)
  ;; EDGE intersects currently active horizontal span(s)
  ((edge :initarg :edge :accessor edge)
   (at :initarg :at :reader at)))

(defun make-events-1 (q shape contour)
  (labels ((f (x) (coerce x 'double-float))
           (split-b (b)
             (let* ((v1 (b::p-rv (b::b2-p1 b)))
                    (vc (b::p-rv (b::b2-c1 b)))
                    (v2 (b::p-rv (b::b2-p2 b)))
                    (y1 (b::vy v1))
                    (yc (b::vy vc))
                    (y2 (b::vy v2)))
               ;; sort endpoints so we get same results for regardless
               ;; of curve direction
               (labels ((d (a b) (when (/= a b) (signum (- a b))))
                        (p (a b) (or (d (b::vx a) (b::vx b))
                                     (d (b::vy a) (b::vy b)))))
                 ;; return T of extreme point on curve
                 (let ((s (p v1 v2)))
                   (cond
                     ((or (not s) (plusp s))
                      (let* ((yc-y1 (- yc y1))
                             (at (/ yc-y1 (- yc-y1 (- y2 yc)))))
                        (values at (b::eval-at/b2/fast b at))))
                     (t
                      (rotatef y1 y2)
                      (let* ((yc-y1 (- yc y1))
                             (at1 (/ yc-y1 (- yc-y1 (- y2 yc))))
                             (p (b::%eval-at/b2/fast (b::b2-dp2 b)
                                                     (b::b2-dc1 b)
                                                     (b::b2-dp1 b)
                                                     at1)))
                        (values (- 1 at1) p))))))))
           (add-pair (up e x1 y1 at1 x2 y2 at2)
             (if up
                 (progn
                   (assert (< y1 y2))
                   (q:enqueue q (make-instance 'start-event
                                               :x x1 :y y1
                                               :at at1
                                               :start e)
                              (f y1) (f x1))
                   (q:enqueue q (make-instance 'end-event
                                               :x x2 :y y2
                                               :at at2
                                               :end e)
                              (f y2) (f x2)))
                 (progn
                   (assert (< y2 y1))
                   (q:enqueue q (make-instance 'end-event
                                               :x x1 :y y1
                                               :at at1
                                               :end e)
                              (f y1) (f x1))
                   (q:enqueue q (make-instance 'start-event
                                               :x x2 :y y2
                                               :at at2
                                               :start e)
                              (f y2) (f x2)))))
           (make-edge (&rest args)
             (apply #'make-instance 'intersect-edge args)))
    (loop for c = contour then n
          for n = (b::next shape c)
          do (etypecase c
               (b::point nil)
               (b::segment
                (let* ((x1 (b::s-rx1 c))
                       (y1 (b::s-ry1 c))
                       (x2 (b::s-rx2 c))
                       (y2 (b::s-ry2 c))
                       (up (< y1 y2))
                       (e (make-edge :edge c
                                     :t1 (if up 0d0 1d0)
                                     :t2 (if up 1d0 0d0))))
                  (if (= y1 y2)
                      (let ((lx (min x1 x2))
                            (rx (max x1 x2)))
                        (change-class e 'horizontal-intersect-edge
                                      :x lx :x2 rx)
                        (q:enqueue q (make-instance 'horizontal-event
                                                    :x lx
                                                    :X2 rx
                                                    :y y1
                                                    :edge e)
                                   (f y1) (f lx)))
                      (add-pair up e x1 y1 0d0 x2 y2 1d0))))
               (b::bezier2
                (let* ((x1 (b::b2-rx1 c))
                       (y1 (b::b2-ry1 c))
                       (x2 (b::b2-rx2 c))
                       (y2 (b::b2-ry2 c)))
                  (if (is-extreme c shape)
                      ;; contains an extreme Y value, split and add
                      ;; as 2 pieces
                      (multiple-value-bind (at p)
                          (split-b c)
                        (let* ((at (float at 1d0))
                               (xe (b::vx p))
                               (ye (b::vy p))
                               (up (> ye y1)))
                          (assert (eql up (> ye y2)))
                          (add-pair up (make-edge :edge c
                                                  :split-point p
                                                  :t1 (if up 0d0 at)
                                                  :t2 (if up at 0d0))
                                    x1 y1 0d0 xe ye at)
                          (add-pair up (make-edge :edge c
                                                  :split-point p
                                                  :t1 (if up 1d0 at)
                                                  :t2 (if up at 1d0))
                                    x2 y2 1d0 xe ye at)))
                      ;; normal, just add directly
                      (let* ((up (< y1 y2))
                             (e (make-edge :edge c
                                           :t1 (if up 0d0 1d0)
                                           :t2 (if up 1d0 0d0))))
                        (assert (/= y1 y2))
                        (add-pair up e x1 y1 0d0 x2 y2 1d0))))))
          until (eql n contour))))

(defun make-events (shape)
  (let ((q (q:make-queue))
        (*id* 0))
    (loop for c across (b::contours shape)
          do (make-events-1 q shape c))
    q))

(defun %sort-rb/a (a b y)
  (assert (not (eql a b)))
  (assert (< (abs (- (x-at a y) (x-at b y)))
             (* 16384  double-float-epsilon)))
  ;; at intersections, sort by angle
  (let ((a1 (%tangent-x-at a y))
        (a2 (%tangent-x-at b y))
        ;; this should possibly be much larger epsilon (more than
        ;; would usually be needed for just FP noise). At small
        ;; angles, the curves are nearly tangent and intersection code
        ;; probably didn't converge to a very precise intersection. In
        ;; that case, there might actually be 0,1 or 2 intersections
        ;; included in this event, and "correct" comparison of angles
        ;; might not give the answer we want.

        ;; In particular, an angle/derivative test might correctly report
        ;; either result if there are 2 intersections too close to
        ;; resolve, depending on where we test the angle relative to those
        ;; intersections. Even 1 intersection (or near miss) might get
        ;; either result correctly, if there is an inflection in one or
        ;; both curves near the tested point.

        ;; todo: test more nearly-tangent curves and see if this needs
        ;; to be increased, and if so by how much.
        (tangent-epsilon (* 32 double-float-epsilon)))

    ;; actually, possibly to be fully correct it should always check
    ;; both derivatives, or look for inflections near test point, or
    ;; check for a future intersection near the one being tested. not
    ;; sure if an intersection near an inflection of a very pointy
    ;; curve might be off enough to get the wrong answer?

    (if (< (abs (- a1 a2)) tangent-epsilon)
        ;; if (nearly) tangent, sort by curvature
        (let ((d1 (%curvature-at a y))
              (d2 (%curvature-at b y)))
          (or (< d1 d2)
              (and (= d1 d2)
                   ;; if curvature is the same, check derivative of
                   ;; curvature with respect to s
                   (let ((dk1 (%curvature-derivative-at a y))
                         (dk2 (%curvature-derivative-at b y)))
                     (or (> dk1 dk2)
                         (and (= dk1 dk2)
                              ;; or if it seems to be on same
                              ;; curve, sort by ID
                              (or (< (id a) (id b)))))))))
        ;; otherwise just by tangent
        (< a1 a2))))

(defun %eql/a (a b y)
  ;; return true if %sort-rb/a would have returned comparison based on
  ;; ID (used to filter potential overlapped edges)
  (let ((a1 (%tangent-x-at a y))
        (a2 (%tangent-x-at b y))
        (ea (edge a))
        (eb (edge b))
        (tangent-epsilon (* 32 double-float-epsilon)))
    (and (< (abs (- a1 a2)) tangent-epsilon)
         (or (and (typep ea 'b::segment) (typep eb 'b::segment))
             (and (= (%curvature-at a y) (%curvature-at b y))
                  (progn
                    ;; if we have same curvature between a segment and
                    ;; bezier something is broken
                    (assert (and (typep ea 'b::bezier2) (typep eb 'b::bezier2)))
                    (let ((a1 (b::b2-p1 ea))
                          (ac (b::b2-c1 ea))
                          (a2 (b::b2-p2 ea))
                          (b1 (b::b2-p1 eb))
                          (bc (b::b2-c1 eb))
                          (b2 (b::b2-p2 eb)))
                      (if (and (b::point= ac bc)
                               ;; same curve regardless of which direction
                               ;; it is going
                               (or (and (b::point= a1 b1) (b::point= a2 b2))
                                   (and (b::point= a2 b1) (b::point= a1 b2))))
                          t
                          (if (/= (%curvature-derivative-at a y)
                                  (%curvature-derivative-at b y))
                              nil
                              (sdf/quadratic-intersect/int::same-parabola-p
                               (edge a) (edge b)
                               (b::%bcs-eps (edge a))))))))))))


(defun %sort-rb (a b y)
  (if (and (eql (edge a) (edge b)))
      ;; if we are comparing 2 parts of a b2, just sort by X at
      ;; endpoint closest to current Y (we only split at y extrema, so
      ;; should always have an X for both at that value)
      (progn
        (let* ((y2 (if (< (abs (- (b::b2-dy1 (edge a)) y))
                          (abs (- (b::b2-dy2 (edge b)) y)))
                       (b::b2-dy1 (edge a))
                       (b::b2-dy2 (edge a))))
               (x1 (%x-at (edge a) y2 (t1 a) (t2 a)
                          (split-point a) :errorp t))
               (x2 (%x-at (edge b) y2 (t1 b) (t2 b)
                          (split-point b) :errorp t)))
          (assert (/= x1 x2)) ;; shouldn't be able to split at an endpoint
          (< x1 x2)))
      ;; otherwise sort by current X (we set both X values to value
      ;; from calculated intersection and mostly call the angle sort
      ;; directly in that case, so we should usually have exactly =
      ;; values here. If not, we probably either missed an
      ;; intersection or should be merging nearby events better (or
      ;; quantizing them to a coarser grid, etc)
      (let ((x1 (x-at a y))
            (x2 (x-at b y)))
        (or (< x1 x2)
            (and (= x1 x2)
                 ;; or if at intersection, sort by tangent/curvature/id
                 (%sort-rb/a a b y))))))


(defclass sweep ()
  ((rb :reader rb :initarg :rb)
   (%set-y :reader %set-y :initarg :set-y)
   ;; priority queue of currently active horizontal edges, ordered by
   ;; X value of right end point
   (horizontal-edges :accessor horizontal-edges :initform (dq:make-queue))
   ;; highest X value of edges in horizontal edges, for which we have
   ;; already calculated future intersection events (so any new
   ;; horizontal edge with lower X doesn't need to check for them, and
   ;; those with higher X can start there) Should be NIL when
   ;; HORIZONTAL-EDGES is empty.
   (horizontal-edge-max :accessor horizontal-edge-max :initform nil)
   ;; state used for reconstructing contours
   (contour-index :initform (make-hash-table) :reader contour-index)
   ;; contour-vertex -> boolean indicating contour in contour index is a 'hole'
   (contour-direction-flags :initform (make-hash-table)
                            :reader contour-direction-flags)
   (finished-contours :initform nil :accessor finished-contours)
   ;; store original shape so we can check connectivity of original
   ;; edges while sweeping
   (shape :initarg :shape :reader shape)
   ;; if winding #s changed after a previous event, store first node
   ;; that needs updated here, so next event (or start of next sweep)
   ;; can propagate it
   (winding-changed :initform nil :accessor winding-changed)
   ;; debug stuff
   (dbg-added :accessor dbg-added :initform nil)
   (dbg-ordering :reader dbg-ordering
                 :initform (list (make-hash-table) (make-hash-table)))
   (dbg-events :accessor dbg-events :initform nil)))

(defmethod set-y ((sweep sweep) y)
  (funcall (%set-y sweep) y))

(defun make-sweep (&optional sweep)
  (declare (ignore sweep))
  (let ((y nil))
    (flet ((sort-func (a b)
             (%sort-rb a b y))
           (set-y (new-y)
             (setf y new-y)))
      (make-instance 'sweep
                     :shape nil
                     :rb (rb:make-tree :key-func #'identity
                                       :sort-func #'sort-func)
                     :set-y #'set-y))))

(defun nl (n)
  (labels ((i (x)
             (if (and x (= x (floor x))) (floor x) x))
           (li (&rest r)
             (mapcar #'i r)))
    (etypecase n
      (symbol n)
      (cons (mapcar 'nl n))
      (horizontal-event (list :horiz (x n) '- (x2 n) (y n) (nl (edge n))))
      (horizontal-intersect-event (list :horiz-int (x n) (y n) (nl (edge n))))
      (start-event (list :add (x n) (y n) (nl (start n))))
      (end-event (list :del (x n) (y n)(nl (end n))))
      (intersect-event (list :intersect (x n) (y n)
                             :left (nl (left n))
                             :right (nl (right n))))
      (intersect-edge (list (nl (edge n)) (i (t1 n)) (i (t2 n))
                            (format nil "+~a~@[(~a)~]=~a"
                                    (i (winding-sign n))
                                    (when (and (y n)
                                               (/= (winding-sign n)
                                                   (maybe-winding-sign n)))
                                      (i (maybe-winding-sign n)))
                                    (i (winding-number n)))
                            (if (on-boundary n) :! :x)))
      (b::point (list (b::p-dx n) (b::p-dy n)))
      (b::segment (list (li (b::s-dx1 n) (b::s-dy1 n))
                        (li (b::s-dx2 n) (b::s-dy2 n))))
      (b::bezier2 (list (li (b::b2-dx1 n) (b::b2-dy1 n))
                        (li (b::b2-dxc n) (b::b2-dyc n))
                        (li (b::b2-dx2 n) (b::b2-dy2 n)))))))

(defun pn (n) (format t "pn~s " (nl n)))
(defun ri (x) (if (and x (= x (floor x))) (floor x) x))

;; don't know control point of a b2 in final contour until we see both
;; endpoints, so store some extra data wile reconstructing contour
(defclass usc-contour-bezier2 (b::es-contour-bezier2)
  ((t1 :initarg :t1 :reader t1)
   (t2 :initarg :t2 :reader t2)
   (edge :initarg :edge :Reader edge)))

;; mostly just for debugging, track which edge it goes with
(defclass usc-contour-segment (b::es-contour-segment)
  ((edge :initarg :edge :Reader edge)))


(defun update-sweep-contours (sweep
                              intersect-in intersect-out
                              x y
                              winding-in-left winding-in-right
                              winding-out-left winding-out-right
                              horizontals
                              &key verbose)
  (when verbose
    (format t "~&---------------~%update: (~s <> ~s) | (~s <> ~s) @ ~s, ~s~%"
            (ri winding-in-left) (ri winding-in-right)
            (ri winding-out-left) (ri winding-out-right)
            (ri x) (ri y)))
  (labels ((widest-horizontal (horizontals)
             (when horizontals
               (loop with h = (car horizontals)
                     with x2 = (x2 h)
                     for i in (cdr horizontals)
                     when (> (x2 i) x2)
                       do (setf x2 (x2 i) h i)
                     finally (return h)))))
    (let* ((ci (contour-index sweep))
           (h-in (third (gethash :horizontal ci)))
           (h-out (widest-horizontal horizontals))
           ;; we have to be careful about ordering, since we might try
           ;; to add and remove and edge that continues through the
           ;; intersection, so those need to be in the right
           ;; order. Also, we update some values in the contour index
           ;; when joining contours, so that also needs to be in the
           ;; right order. To do this, we store a list of updates to
           ;; do later after deciding what needs done.

           ;;
           (to-add nil)
           (to-remove nil)
           ;; hash table of nodes that were added this update, so
           ;; splicing knows not to update them even if it saw the
           ;; other half of the edge
           #++(modified (make-hash-table))

           ;; not used?
           (deferred-updates nil))
      (declare (ignorable to-add to-remove deferred-updates))
      (labels ((key (e)
                 (if (typep e 'horizontal-intersect-edge)
                     :horizontal
                     e))
               (%activate (e)
                 (when verbose
                   (format t " +~s~%   (~s ~s)~%"
                           (nl (fourth e))
                           (third e) (b::enl (second e))))
                 (assert (not (gethash (car e) ci)))
                 (setf (gethash (car e) ci) (cdr e)))
               (%deactivate (e)
                 (when verbose
                   (format t " -~s~%" (nl e)))
                 (assert (gethash e ci))
                 (remhash e ci))
               (finish-updates ()
                 (when (and verbose (or to-remove to-add deferred-updates))
                   (format t "running deferred updates: remove ~s, add ~s defer ~s~%"
                           (length to-remove) (length to-add)
                           (length deferred-updates)))
                 (loop for (f . a) in (shiftf deferred-updates nil)
                       do (apply f a))
                 (map 'nil #'%deactivate (shiftf to-remove nil))
                 (map 'nil #'%activate (shiftf to-add nil)))
               (activate (e &key defer)
                 (if defer
                     (push e to-add)
                     (%activate e)))
               (deactivate (e &key defer)
                 (if defer
                     (push e to-remove)
                     (%deactivate e)))
               (point (&optional (x x) (y y))
                 (make-instance 'b::es-contour-vertex
                                :point (b::make-point x y)))
               (new-node (e)
                 (etypecase (edge e)
                   (b::segment
                    (make-instance 'usc-contour-segment
                                   :edge e))
                   (b::bezier2
                    (make-instance 'usc-contour-bezier2
                                   :t1 (t-at e y) :edge e))))
               (finish-node (n e)
                 (when verbose
                   (format t "finish node ~s~% n= ~s~%"
                           (nl e) (nl (edge n))))
                 (etypecase n
                   (usc-contour-segment
                    (assert (eql (edge n) e)
                            nil "~s~% /=~% ~s" (nl (edge n)) (nl e))
                    (assert (typep (edge e) 'b::segment)))
                   (usc-contour-bezier2
                    (assert (eql (edge n) e)
                            nil "~s~% /=~% ~s" (nl (edge n)) (nl e))
                    (assert (typep (edge e) 'b::bezier2))
                    (let* ((t1 (t1 n))
                           (t2 (t-at e y))
                           (b (edge e))
                           (c (b::%%trim-b2-p1 b t1 t2)))
                      (setf (slot-value n 't2) t2
                            (slot-value n 'b::control-point)
                            (b::make-point (b::vx c) (b::vy c)))))))
               (splice (l r mask)
                 ;; splice nodes so L is shared by nodes being joined,
                 ;; and R points to other sides of contours
                 (when verbose
                   (format t "splice ~2,'0b @~%" mask)
                   (format t " lp ~s~%" (b::enl (b::eprev l)))
                   (format t " l  ~s~%" (b::enl l))
                   (format t " ln ~s~%" (b::enl (b::enext l)))
                   (format t " rp ~s~%" (b::enl (b::eprev r)))
                   (format t " r  ~s~%" (b::enl r))
                   (format t " rn ~s~%" (b::enl (b::enext r))))

                 (ecase mask
                   (#b00 ;; l to l

                    (b::%reverse-contour r)
                    (setf (gethash r (contour-direction-flags sweep))
                          (not (gethash r (contour-direction-flags sweep))))

                    (rotatef (b::enext (b::eprev r))
                             (b::enext (b::eprev l)))
                    (rotatef (b::eprev r)
                             (b::eprev l)))
                   (#b01 ;; r to l
                    (rotatef (b::eprev (b::enext l))
                             (b::eprev (b::enext r)))
                    (rotatef (b::enext l)
                             (b::enext r)))
                   (#b10 ;; l to r
                    (rotatef (b::enext (b::eprev l))
                             (b::enext (b::eprev r)))
                    (rotatef (b::eprev l)
                             (b::eprev r)))
                   (#b11 ;;r to r
                    (b::%reverse-contour l)
                    (setf (gethash l (contour-direction-flags sweep))
                          (not (gethash l (contour-direction-flags sweep))))

                    (rotatef (b::enext (b::eprev l))
                             (b::enext (b::eprev r)))
                    (rotatef (b::eprev l)
                             (b::eprev r))))

                 (when verbose
                   (Format t "after=~%")
                   (format t " lp ~s~%" (b::enl (b::eprev l)))
                   (format t " l  ~s~%" (b::enl l))
                   (format t " ln ~s~%" (b::enl (b::enext l)))
                   (format t " rp ~s~%" (b::enl (b::eprev r)))
                   (format t " r  ~s~%" (b::enl r))
                   (format t " rn ~s~%" (b::enl (b::enext r))))

                 (flet ((update (n d lr)
                          (let ((e (gethash (edge n) ci)))
                            (unless e
                              (let ((h (gethash :horizontal ci)))
                                (when (and h (eql (third h) (edge n)))
                                  (setf e h))))
                            (when verbose
                              (format t "updateci ~s~%" (nl (edge n)))
                              (unless e
                                (format t "!!! edge ~s not active?~%  ~s~%  ~s~%"
                                        n (edge n) (nl (edge n))))
                              (format t " ~s ~s -> ~s ~s~%"
                                      (b::enl (car e)) (cadr e)
                                      (b::enl d) lr))
                            (when e
                              (setf (first e) d)
                              (setf (second e) lr)))))
                   (update (b::eprev l) l :r)
                   (update (b::enext l) l :l)
                   (update (b::eprev r) r :r)
                   (update (b::enext r) r :l))
                 (when verbose
                   (format t "::spliced ~b l~%  ~s~%  ~s~%"
                           mask
                           (nl (edge (b::eprev l)))
                           (nl (edge (b::enext l))))
                   (format t "::spliced ~b r~%  ~s~%  ~s~%"
                           mask
                           (nl (edge (b::eprev r)))
                           (nl (edge (b::enext r))))))
               (merge-split-edges (c)
                 (b::map-modifying-contour
                  c (lambda (n)
                      (let ((n2 (b::enext (b::enext n))))
                        (when (and (typep n 'usc-contour-bezier2)
                                   (typep n2 'usc-contour-bezier2)
                                   (eql (edge (edge n)) (edge (edge n2))))
                          (when verbose
                            (format t "~&merge split nodes:~%#~s-~s ~s~%@~s-~s ~s~%"
                                    (t1 n) (t2 n) (nl (edge n))
                                    (t1 n2) (t2 n2) (nl (edge n2))))
                          (let ((a1 (t1 n))
                                (a2 (t2 n))
                                (b1 (t1 n2))
                                (b2 (t2 n2))
                                (e (edge (edge n))))
                            (cond
                              ((= a1 b1)
                               (let ((c (b::%%trim-b2-p1 e a2 b2)))
                                 (setf (slot-value n 'b::control-point)
                                       (b::make-point (b::vx c) (b::vy c)))))
                              ((= a2 b2)
                               (let ((c (b::%%trim-b2-p1 e a1 b1)))
                                 (setf (slot-value n 'b::control-point)
                                       (b::make-point (b::vx c) (b::vy c)))))
                              ((= a1 b2)
                               (let ((c (b::%%trim-b2-p1 e a2 b1)))
                                 (setf (slot-value n 'b::control-point)
                                       (b::make-point (b::vx c) (b::vy c)))))
                              ((= a2 b1)
                               (let ((c (b::%%trim-b2-p1 e a1 b2)))
                                 (setf (slot-value n 'b::control-point)
                                       (b::make-point (b::vx c) (b::vy c)))))
                              (t (error "no common point?"))))
                          (b::%delete-node (b::enext n))
                          (b::%delete-node (b::enext n))))
                      n)))
               (%join-existing (a b)
                 (when verbose
                   (format t "join-existing @ ~s,~s:~%" x y)
                   (format t "  @~s~%" (nl a))
                   (format t "  @~s~%" (nl b)))
                 (destructuring-bind (l ld le) (gethash (key a) ci)
                   (declare (ignore le))
                   (destructuring-bind (r rd re) (gethash (key b) ci)
                     (declare (ignore re))
                     (when verbose
                       (format t " l = ~s~%" l)
                       (format t "   = ~s~%" (gethash (key a) ci))
                       (format t " r = ~s~%" r)
                       (format t "   = ~s~%" (gethash (key b) ci)))
                     (deactivate (key a) :defer nil)
                     (deactivate (key b) :defer nil)
                     (cond
                       ((eql l r)
                        ;; finishing a contour
                        (assert (not (eql ld rd)))
                        (when verbose
                          (format t "  finish contour:~%"))
                        (ecase ld
                          (:l
                           (finish-node (b::enext l) a)
                           (finish-node (b::eprev r) b))
                          (:r
                           (finish-node (b::eprev l) a)
                           (finish-node (b::enext r) b)))
                        (setf (slot-value l 'b::point)
                              (b::make-point x y))
                        (multiple-value-bind (d found)
                            (gethash l (contour-direction-flags sweep))
                          (assert found)
                          (when verbose
                            (if d
                                (format t "  hole, reversing~%")
                                (format t "  exterior.~%")))
                          (when d
                            (setf l (b::%reverse-contour l))))
                        (setf l (merge-split-edges l))
                        (push l (finished-contours sweep)))
                       (t ;; join 2 contours
                        (when verbose
                          (format t "  join contour:~%"))
                        (let ((mask 0))
                          (ecase ld
                            (:l
                             (finish-node (b::enext l) a))
                            (:r
                             (setf (ldb (byte 1 0) mask) 1)
                             (finish-node (b::eprev l) a)))
                          (ecase rd
                            (:l
                             (finish-node (b::enext r) b))
                            (:r
                             (setf (ldb (byte 1 1) mask) 1)
                             (finish-node (b::eprev r) b)))
                          (splice l r mask)
                          (let ((flags (contour-direction-flags sweep)))
                            (multiple-value-bind (dl found)
                                (gethash l flags)
                              (assert found)
                              (multiple-value-bind (dr found)
                                  (gethash r flags)
                                (assert found)
                                (when verbose
                                  (if (and dl dr)
                                      (format t "  joined holes~%")
                                      (format t "  joined exterior ~s ~s.~%"
                                              dl dr)))
                                (remhash l flags)
                                (unless (and dl dr)
                                  (setf (gethash r flags) nil)))))
                          (setf (slot-value l 'b::point)
                                (b::make-point x y))))))))
               (join-existing (a b)
                 (%join-existing a b))
               (%continue-contour (a b)
                 (when verbose
                   (format t "continue-contour:~%")
                   (format t "  @~s~%" (nl a))
                   (format t "  @~s~%" (nl b)))
                 (destructuring-bind (p d e) (gethash (key a) ci)
                   (declare (ignore e))
                   (assert d)
                   ;; if continuing, deactivate immediately since it
                   ;; shouldn't be involved in a splice
                   (deactivate (key a) :defer nil)
                   (ecase d
                     (:l
                      (finish-node (b::enext p) a)
                      (b::add-after p (point))
                      (b::add-after p (new-node b)))
                     (:r
                      (finish-node (b::eprev p) a)
                      (b::add-before p (point))
                      (b::add-before p (new-node b))))
                   ;; but defer add, since other half might be
                   ;; entering a continue or join
                   (activate (list (key b) p d b) :defer t)))
               (continue-contour (a b)
                 ;; continue needs to see updates to index from
                 ;; splice, so defer those until after we look at all
                 ;; intersection
                 (push (list #'%continue-contour a b) deferred-updates))
               (%start-contour (a b)
                 (let ((p (point (incf *id*) 123456))
                       (flags (contour-direction-flags sweep)))
                   (assert (or (on-boundary a) (on-boundary b)))
                   (cond
                     ((on-boundary a)
                      (if (zerop (winding-number a))
                          (setf (gethash p flags) t)
                          (setf (gethash p flags) nil)))
                     ((on-boundary b)
                      (if (zerop (winding-number a))
                          (setf (gethash p flags) nil)
                          (setf (gethash p flags) t))))
                   (b::add-after p (point))
                   (b::add-after p (new-node a))
                   (b::add-before p (new-node b))
                   (when verbose
                     (format t "start contour~%")
                     (if (gethash p flags)
                         (format t "  hole~%")
                         (format t "  exterior~%"))
                     (format t "  @~s~%" (nl a))
                     (format t "  @~s~%" (nl b)))
                   ;; other half of edge (if any) might be entering a
                   ;; join or continue, so defer adding
                   (activate (list (key a) p :l a) :defer t)
                   (activate (list (key b) p :r b) :defer t)))
               (start-contour (a b)
                 (%start-contour a b)))
        (declare (ignorable #'%activate #'%deactivate))
        (when *check*
          (when verbose
            (format t "## n-in=~s, n-out=~s, h-in=~s, h-out-~s~%"
                    (length intersect-in) (length intersect-out)
                    (if h-in T nil) (if h-out T nil))
            (format t "contour-index: (~s)~%" (hash-table-count ci)))
          (let ((hc (make-hash-table))
                (bad 0))
            (loop for i in (sort (a:hash-table-keys ci)
                                 '<
                                 :key (lambda (a)
                                        (or (ignore-errors (x-at a y))
                                            most-negative-fixnum)))
                  do (incf (gethash (car (gethash i ci)) hc 0))
                     (when verbose
                       (format t " ~a ~s~%"
                               (ignore-errors (ri (x-at i y)))
                               (if (symbolp i) i (nl i)))
                       (let ((v (gethash i ci)))
                         (format t "   -> ~s ~s~%"
                                 (second v)
                                 (b::enl (first v))))))
            (loop for k being the hash-keys of hc using (hash-value v)
                  do (when (/= v 2)
                       (incf bad))
                     (when verbose
                       (format t "$$$~s: ~s refs =~%" k v)
                       (b::%print-contour k :max 10)))
            (unless (zerop bad)
              (break "~s bad refs?~% ~s entries in index" bad
                     (hash-table-count ci)))
            (when (oddp (hash-table-count ci))
              (break "odd # of entries in index? ~s" (hash-table-count ci))))
          (when verbose
            (when intersect-in
              (format t "in:~%")
              (loop for i in intersect-in
                    do (format t " ~a ~s~@[ ~a~]~%"
                               (ri (x-at i y)) (nl i) (and (gethash i ci) t))))
            (when h-in
              (format t "h-in: ~a ~a~% ~a~%"
                      (ri (x-at h-in y)) (ri (x2 h-in)) (nl h-in)))
            (when intersect-out
              (format t "out:~%")
              (loop for i in intersect-out
                    do (format t " ~a ~s~@[ ~a~]~%"
                               (ri (x-at i y)) (nl i) (and (gethash i ci) t))))
            (when h-out
              (format t "h-out: ~a ~a~% ~a~%"
                      (ri (x-at h-out y)) (ri (x2 h-out)) (nl h-out)))))
        (assert (zerop (count-if-not 'on-boundary intersect-out)))
        (when (and h-in (< (x2 h-in) x))
          (error "missed ending of horizontal?"))
        (when (and h-out (<= (x2 h-out) x))
          (when (< (x2 h-out) x)
            (error "missed ending of horizontal?"))
          (when verbose
            (format t "drop h-out, ends before x: ~s (~s) <= ~s~%"
                    (x2 h-out) (when h-in (x2 h-in)) x))
          (setf h-out nil))
        (when (not (or intersect-in intersect-out))
          ;; if we don't have any edges below or above, we might be
          ;; continuing horizontally, and may need to extend the
          ;; current horizontal edge
          (when (and h-in h-out)
            (when verbose
              (format t "horizontal edge continues to ~s (was ~s)~%"
                      (ri (x2 h-out)) (ri (x2 h-in))))
            (when (> (x2 h-out) (x2 h-in))
              (when verbose
                (format t " continue h-in -> h-out~%"))
              (continue-contour h-in h-out)))
          ;; either way, we don't have anything else to do here
          (finish-updates)
          (return-from update-sweep-contours nil))
        (when (not (zerop winding-in-left))
          ;; we are starting outside shape, so match first edge with
          ;; either a horizontal, first edge from -out, a new
          ;; horizontal, or last edge from -in
          (cond
            ((not intersect-in)
             ;; no 'in' edges, do nothing here
             )
            (h-in
             ;; if we have an active horizontal, it must be other
             ;; boundary of area ending at first edge, so join those
             (when verbose (format t "in-left >< horizontal-in~%"))
             (join-existing h-in (pop intersect-in))
             (setf h-in nil))
            (intersect-out
             (when verbose (format t "in-left -> out-left~%"))
             (assert (eql (zerop winding-in-left) (zerop winding-out-left)))
             ;; we used up a non-zero->0 edge, so set winding to 0
             (setf winding-out-left 0)
             (continue-contour (pop intersect-in) (pop intersect-out)))
            ((and h-out (not (eql (signum winding-in-left)
                                  (signum winding-in-right))))
             (when verbose (format t "in-left -> horizontal-out"))
             (continue-contour (pop intersect-in) h-out)
             (setf h-out nil))
            (t
             ;; no horizontal or exiting edges, join to last incoming
             ;; edge
             (let* ((a (pop intersect-in))
                    (b (last intersect-in)))
               (when verbose (format t " in-left >< in-right~%"))
               (assert b)
               (assert (not (zerop winding-in-right)))
               ;; todo: get rid of extra passes through list
               (setf intersect-in (butlast intersect-in))
               (assert (evenp (length intersect-in)))
               (join-existing a (car b))))))
        (when  (and (not (zerop winding-out-left))
                    (/= (signum winding-out-left)
                        (signum winding-out-right)))
          ;; we are still inside shape as we leave the intersection,
          ;; so it should either match a horizontal or other side of
          ;; -out
          (cond
            ((not intersect-out)
             ;; no more edges leaving, do nothing
             )
            (h-in
             (when verbose (format t "h-in -> out-left~%"))
             (continue-contour h-in (pop intersect-out))
             (setf winding-out-left 0)
             (setf h-in nil))
            ((and h-out (zerop winding-out-right))
             (when verbose (format t " new contour out-left <-> h-out~%"))
             (start-contour (pop intersect-out) h-out)
             (setf h-out nil))
            (intersect-in
             (error "should have handled this already"))
            (t
             ;; no horizontal or incoming edges, join to last outgoing
             ;; edge
             (let* ((a (pop intersect-out))
                    (b (last intersect-out)))
               (when verbose (format t "new contour out-left <-> out-right~%"))
               (assert (not (zerop winding-out-right)))
               (assert b)
               ;; todo: get rid of extra passes through list
               (setf intersect-out (butlast intersect-out))
               (assert (evenp (length intersect-out)))
               (start-contour (car b) a)))))
        ;; at this point, first 2N elements of intersect-in should be
        ;; pairs bounding interior edges, close them all
        (when (cdr intersect-in)
          (loop for a = (pop intersect-in)
                for b = (pop intersect-in)
                do (when verbose
                     (format t "in-mid ~s >< in-mid ~s~%"
                             (winding-number a) (winding-number b)))
                   (join-existing a b)
                while (cdr intersect-in)))

        ;; if we have a h-in edge left, it continues to intersect-out,
        ;; h-out, or intersect-in
        (when h-in
          (cond
            (intersect-out
             (when verbose
               (format t "continue h-in -> out-left~%"))
             (continue-contour h-in (pop intersect-out))
             (setf h-in nil))
            (intersect-in
             (when verbose
               (format t " h-in >< in-right~%"))
             (join-existing h-in (pop intersect-in))
             (setf h-in nil))
            (h-out
             (when verbose
               (format t "continue h-in -> h-out?~%"))
             (continue-contour h-in h-out)
             (setf h-in nil)
             (setf h-out nil))))
        ;; and same for intersect-out, but starting new contours
        (when (cdr intersect-out)
          (loop for a = (pop intersect-out)
                for b = (pop intersect-out)
                do (when verbose
                     (format t "out-mid ~s <-> out-mid ~s~%"
                             (winding-number a) (winding-number b)))
                   (start-contour a b)
                while (cdr intersect-out)))
        ;; we should have 0 or 2 elements left, 0 or 1 each from
        ;; in,out, horizontal
        (when (or intersect-in intersect-out h-out)
          (cond
            (intersect-in
             (when verbose
               (format t "continue final, in-right ->~a~%"
                       (if h-out (if intersect-out "h-o+o-r?" "h-out")
                           (if intersect-out "out-right" "?nothing?"))))
             (when (and intersect-out h-out)
               (when verbose
                 (format t " ???????? assuming not horizontal?~%"))
               (setf h-out nil))
             (continue-contour (pop intersect-in)
                               (or h-out (pop intersect-out)))
             (setf h-out nil))
            ((and h-out intersect-out)
             (when verbose (format t "new contour out-right <-> h-out~%"))
             (start-contour (pop intersect-out) h-out)
             (setf h-out nil))
            (h-out
             ;; not an error
             )
            (t (error "extra edge?"))))
        (assert (not intersect-in))
        (assert (not intersect-out))
        (assert (not h-in))
        (finish-updates)
        (assert (not (or to-add to-remove deferred-updates)))
        (when *check* (assert (evenp (hash-table-count ci))))))))

(defun deferred-update-winding (sweep end verbose)
  ;; update range of winding #s in rb tree after (winding-changed
  ;; sweep) up to (but not including) END (or end of tree if NIL).
  ;; (if set, END is first node being updated by an event, so doesn't
  ;; need updated here.
  (assert (winding-changed sweep))
  (let* ((start (winding-changed sweep))
         (v (rb:value start))
         (y (y v)))
    (unless (or (eql start end)
                ;; if we insert before deferred update, we can
                ;; continue to defer it
                (and end (<= (x-at (rb:value end) y)
                             (x-at v y))))
      (assert start)
      (labels ((maybe-winding-sign (n)
                 ;; not using global version since we should never see
                 ;; the end of a segment here (it should have been
                 ;; deleted before calling this), so assert that
                 ;; instead of doing test
                 (when *check*
                   (let ((e (edge n)))
                     (if (split-point n)
                         (when (= y (aref (split-point n) 1))
                           (error "deferred update saw end of an edge?1~% ~a"
                                  (nl e)))
                         (multiple-value-bind (p1 p2)
                             (if (typep e 'b::segment)
                                 (values (b::s-p1 e) (b::s-p2 e))
                                 (values (b::b2-p1 e) (b::b2-p2 e)))
                           (when (= y (max (b::p-dy p1) (b::p-dy p2)))
                             (error "deferred update saw end of an edge?2~% ~a"
                                    (nl e)))))))
                 (winding-sign n)))
        (when verbose
          (format t "deferred update winding #s~% from ~s~%   to ~s~%"
                  (nl (rb:value start))
                  (when end (nl (rb:value end)))))
        (loop with wn = (winding-number (rb:value start))
              for n = (rb:next start) then (rb:next n)
              for v = (when n (rb:value n))
              for wp = wn
              until (eql n end)
              do (assert n)
                 (when verbose
                   (format t " ~s: ~s -> ~s~%" (nl v)
                           (winding-number v) (+ wn (winding-sign v))))
                 (incf wn (maybe-winding-sign v))
                 (cond
                   ((= (winding-number v) wn)
                    (when verbose
                      (format t " -- windings match existing, stopping~%"))
                    (loop-finish))
                   (t
                    (setf (winding-number v) wn)))
                 (when (previous-windings v)
                   (assert (/= y (car (previous-windings v)))))
                 ;; not sure if changing here is valid or not?
                 ;; probably not since it should only happen on a
                 ;; horizontal edge, and we should have events for
                 ;; that. might be valid, due to stacked boundary
                 ;; edges that got cleared?
                 (assert (eql (on-boundary v)
                              (a:xor (zerop wp) (zerop wn))))
                 (setf (previous-windings v)
                       (list y wp (on-boundary v) wn)))))
    (setf (winding-changed sweep) nil)))

(defun update-sweep-1 (sweep e q xq &key verbose)
  (declare (notinline b::intersect/range))
  ;; update sweepline for events at a particular X value, add any new
  ;; events to queue
  (when verbose
    (format t "------update sweep 1 @ y=~s~%" (y (car e))))
  (let* ((e1 (car e))
         (rb (rb sweep))
         (ordering (dbg-ordering sweep))
         ;; fixme: pass X,Y in directly, since caller might need to
         ;; merge some very close events to keep intersections
         ;; coherent
         (y (y e1))
         (x (x e1))
         ;; for various stages of processing we need (2 or) 3 different
         ;; bounds:
         ;;
         ;; first we need bounds of nodes actually involved in
         ;; intersection. FIND-INTERSECTION-BOUNDS calculates those,
         ;; and in the process updates RESULTS. UPDATE-WINDINGS or
         ;; SORT-FOR-INTERSECT (which updates winding #s) also use
         ;; those bounds.
         ;;
         ;; Next we need the same bounds, but expanded by 1 in each
         ;; direction if possible, since we might need to check for
         ;; new intersections with edges adjacent to the current
         ;; intersection. If a bound can't be expanded due to already
         ;; being at the edge of the rb tree, we can either keep it
         ;; the same or replace it with NIL. For the intersection
         ;; check we need a starting node, so storing the previous
         ;; START is more useful. For determining if we need to update
         ;; winding #s to right of this intersection, storing NIL is
         ;; more useful. In both cases we need to be careful about the
         ;; case where START,END,both,or entire range were deleted.
         ;;
         ;; ISTART/IEND stores the first set of bounds, then
         ;; EXPAND-BOUNDS is called and possibly expands them to the
         ;; non-nil version, while storing the possibly nil version in
         ;; XSTART/XEND
         (istart nil)
         (iend nil)
         (xstart nil)
         (xend nil)
         ;; list of edges that intersect at this point
         (result nil)
         ;; hash table version of RESULT, edge-> :ADD,:DEL,:HORIZ,or :INTERSECT
         (hresult (make-hash-table))
         ;; list of edges that enter this intersection, in order (not
         ;; including horizontals?).
         (intersect-in)
         ;; list of boundary edges that exit this intersection, in
         ;; order (not including horizontals?)
         (intersect-out)
         ;; pq of active horizontal edges
         (active-horizontal (horizontal-edges sweep))
         ;; list of edges to add to intersections for this event
         (horizontal nil)
         ;; winding number before and after this intersection, before updates
         (winding-in-left nil)
         (winding-in-right nil)
         ;; winding number before and after this intersection, after updates
         (winding-out-left nil)
         (winding-out-right nil))
    ;; remove any horizontal edges that completed since last event
    ;; (should be none since we should always have a start or end
    ;;  event for an adjacent segment at end of horizontal span)
    (unless (zerop (dq:size active-horizontal))
      (loop for i = (dq:peek active-horizontal)
            while (and i (< (x2 i) x))
            do (when verbose
                 (format t " dropped stale horizontal span? ~s~%"
                         (nl i)))
               (when *check* (error "dropped stale horizontal span ~s" i))
               (dq:dequeue active-horizontal)))

    (when verbose
      (format t " --> x = ~s~%" (delete-duplicates (mapcar (a:compose 'float 'x) e))))
    (labels ((%dir (n1 f)
               (let ((n (funcall f n1)))
                 (loop while n
                       until (and (not (eql (edge (rb:value n))
                                            (edge (rb:value n1))))
                                  (/= (x-at (rb:value n) y) x))
                       do (setf n (funcall f n)))
                 n))
             (prev (n)
               (%dir n #'rb:previous))
             (next (n)
               (%dir n #'rb:next))
             (check ()
               (when *check*
                 (rb::walkn rb (lambda (a)
                                 (when (rb::key a)
                                   (assert (eql (rb::key a) (rb::value a))))))
                 (let ((v (coerce (rb::to-list rb) 'vector)))
                   (loop for i below (1- (length v))
                         do (loop for j from (1+ i) below (length v)
                                  for a = (aref v i)
                                  for b = (aref v j)
                                  do (pushnew a (gethash b (first ordering)))
                                     (pushnew b (gethash a (second ordering))))))))
             (insert-node (n)
               (let* ((rn (rb:insert rb n)))
                 (assert (not (winding-number n)))
                 (assert (not (%node n)))
                 (setf (%node n) rn)
                 rn))
             (add (e)
               (let ((n (start e)))
                 (when verbose
                   (format t "add ~s~%" (nl n)))
                 (when *check*
                   (assert (not (member n (dbg-added sweep))))
                   (push (list :add n) (dbg-events sweep))
                   (push n (dbg-added sweep)))
                 (assert (not (gethash n hresult)))
                 (setf (gethash n hresult) :add)
                 (insert-node n)
                 (assert (or (null (rb::key (%node n)))
                             (eql (rb::key (%node n))
                                  (rb::value (%node n)))))
                 (check)))
             (del (e)
               (let ((n (end e)))
                 (when verbose
                   (format t "del ~s~%" (nl n)))
                 (when *check*
                   (assert (member n (dbg-added sweep)))
                   (push (list :del n) (dbg-events sweep)))
                 (assert (%node n))
                 (assert (eql (rb:value (%node n)) n))
                 (assert (or (null (rb::key (%node n)))
                             (eql (rb::key (%node n))
                                  (rb::value (%node n)))))
                 (when *check*
                   (a:deletef (dbg-added sweep) n))
                 (check)
                 ;; DEL is called after EXPAND-BOUNDS, since START/END
                 ;; might both point to the node to be deleted.
                 ;;
                 ;; before deleting node, we need to check for whether
                 ;; we are deleting one of the bounds, and update it
                 ;; while preserving desired properties
                 (when (eql istart (%node n))
                   ;; deleting expanded start, must be at left edge of
                   ;; tree (since otherwise we expanded START to
                   ;; another node)
                   (assert (not (rb:previous istart)))
                   ;; should be equivalent to previous assert
                   (assert (not xstart))
                   ;; if we were only node, might still end up nil
                   (setf istart (rb:next istart)))
                 (when (eql iend (%node n))
                   ;; same as above, on right side of tree
                   (assert (not (rb:next iend)))
                   ;; should be equivalent to previous assert
                   (assert (not xend))
                   ;; if we were only node, might still end up nil
                   (setf iend (rb:previous iend))
                   ;; if it ended up NIL, start should also be nil
                   (unless iend (assert (not istart))))
                 ;; done looking at prev/next so can delete from tree
                 (rb:delete-node rb (shiftf (%node n) nil))))
             (add-future-intersections (start end)
               ;; called after processing all events, to check for any
               ;; future intersections and add them to queue.
               ;;
               ;; should be called with START/END containing rb tree
               ;; nodes of first and last nodes we need to check (one
               ;; node before/after nodes in intersection, if any), so
               ;; after EXPAND-BOUNDS
               (when verbose
                 (format t "afi:~% s ~s~% e ~s~%"
                         (when start (nl (rb:value start)))
                         (when end (nl (rb:value end)))))
               (when (and start (not (eql start end)))
                 (assert end)
                 (loop for n1 = start then n2
                       for n2 = (rb:next n1)
                       for e1 = (rb:value n1)
                       for e2 = (when n2 (rb:value n2))
                       for swap = nil
                       while n2
                       do (let ((i* (multiple-value-list
                                     (b::intersect/range (edge e1) (edge e2)
                                                         (t1 e1) (t2 e1)
                                                         (t1 e2) (t2 e2)))))
                            (when verbose
                              (format t "??? @~s:  ~s~%  ~s~% intersections ~s~%"
                                      y (nl e1) (nl e2) i*))
                            (when (and i* (car i*))
                              (when (cdr i*)
                                (setf i* (sort i* '< :key 'b::vy)))
                              (loop
                                for i1 in i*
                                do (when (> (b::vy i1) y)
                                     (when verbose
                                       (format t "#### add intersection event ~s:~% ~s~% ~s~%"
                                               i1 (nl e1) (nl e2)))
                                     (when swap
                                       (rotatef (aref i1 2) (aref i1 3)))
                                     (when *check*
                                       (assert
                                        (< (b::v2dist (subseq i1 0 2)
                                                      (b::eval-at
                                                       (edge e1) (aref i1 2)))
                                           0.1))
                                       (assert
                                        (< (b::v2dist (subseq i1 0 2)
                                                      (b::eval-at
                                                       (edge e2) (aref i1 3)))
                                           0.1)))
                                     (q:enqueue q
                                                (make-instance
                                                 'intersect-event
                                                 :x (b::vx i1)
                                                 :y (b::vy i1)
                                                 :at1 (aref i1 2)
                                                 :at2 (aref i1 3)
                                                 :left e1
                                                 :right e2)
                                                (coerce (b::vy i1)
                                                        'double-float)
                                                (coerce (b::vx i1)
                                                        'double-float))
                                     ;; if we get 2 or more
                                     ;; intersections, they must
                                     ;; have switched places at
                                     ;; each, so swap them in
                                     ;; subsequent events so
                                     ;; intersection code can rely
                                     ;; on order
                                     (rotatef e1 e2)
                                     (setf swap (not swap))))))
                       until (eql n2 end))))
             (winding-boundary-p (a b)
               (a:xor (zerop a) (zerop b)))
             (update-winding (start end)
               (when (winding-changed sweep)
                 ;; if previous event affected winding #s at other X
                 ;; values, update edges between that event and this
                 ;; one
                 (deferred-update-winding sweep start verbose))
               ;; if not sorting, just update winding #s in
               ;; range. Should be called with first/last node in rb
               ;; tree containing an edge from current intersection,
               ;; so before EXPAND-BOUNDS
               (let* ((p (rb:previous start))
                      (wn (if p
                              (winding-number (rb:value p))
                              0)))
                 (when verbose
                   (format t "update winding:start ~s @ ~s~%"
                           (ri wn) (when p (nl (rb:value p)))))
                 (setf winding-out-left wn)
                 (loop for wp = wn
                       for n = start then (rb:next n)
                       for v = (rb:value n)
                       for s = (maybe-winding-sign v)
                       do (setf wn (+ wn s)
                                (winding-number v) wn
                                (on-boundary v) (winding-boundary-p wp wn))
                          (when (previous-windings v)
                            (assert (/= y (car (previous-windings v)))))
                          (setf (previous-windings v)
                                (list y wp (on-boundary v) wn))
                          (when verbose
                            (format t "  ->~s @ ~s~%" (ri wn) (nl v)))
                       until (eql n end))
                 (when verbose
                   (format t "--> ~s~%" (ri wn)))
                 (setf winding-out-right wn)))
             (sort-for-intersect (start end)
               ;; call with bounds before EXPAND-BOUNDS
               (when (winding-changed sweep)
                 ;; if previous event affected winding #s at other X
                 ;; values, update edges between that event and this
                 ;; one
                 (deferred-update-winding sweep start verbose))
               (let* ((atx (loop for n = start then (rb:next n)
                                 while n
                                 collect n
                                 until (eql n end)))
                      (*print-circle* t)
                      ;; for now just recalculating winding # for all
                      ;; nodes in range, so start with # before range
                      (wn (let ((p (rb:previous start)))
                            (if p
                                (winding-number (rb:value p))
                                0))))
                 (setf winding-out-left wn)
                 (cond
                   ;; multiple edges, sort and update windings
                   ((cdr atx)
                    (let ((v (make-array (length atx))))
                      (when verbose
                        (format t "sfi:sorting range at ~s,~s~%" x y)
                        (format t "from ~s~%  to ~s~%"
                                (nl (rb:value start)) (nl (rb:value end)))
                        (format t "  was~%~{    ~s~%~}"
                                (loop for n in atx
                                      for e = (rb:value n)
                                      collect (list* (x-at e y) (nl e)))))
                      (loop for n in atx for i from 0
                            do (setf (aref v i) (rb:value n)))
                      ;; nodes from START to END are intersecting, so
                      ;; sort by derivatives instead of position
                      (sort v (lambda (a b) (%sort-rb/a a b y)))
                      (loop for wp = wn
                            for n in atx
                            for i from 0
                            for vi = (aref v i)
                            do (incf wn (maybe-winding-sign vi))
                               (setf (winding-number vi) wn
                                     (on-boundary vi) (winding-boundary-p wp wn))
                               (unless (eql (rb:value n) vi)
                                 (setf (%node vi) n)
                                 (setf (rb::key n) vi)
                                 (setf (rb:value n) vi))
                               (when (previous-windings vi)
                                 (assert (/= y (car (previous-windings vi)))))
                               (setf (previous-windings vi)
                                     (list y wp (on-boundary vi) wn)))))
                   (t
                    ;; only 1 edge, just update windings
                    (let* ((wp wn)
                           (n (car atx))
                           (vi (rb:value n)))
                      (incf wn (maybe-winding-sign vi))
                      (setf (winding-number vi) wn
                            (on-boundary vi) (winding-boundary-p wp wn))
                      (when (previous-windings vi)
                        (assert (/= y (car (previous-windings vi)))))
                      (setf (previous-windings vi)
                            (list y wp (on-boundary vi) wn)))))
                 (setf winding-out-right wn)
                 (when verbose
                   (format t "  after~% winding-out-left ~s~%~{    ~s~%~}winding out ~s~%"
                           winding-out-left
                           (loop for n in atx
                                 for e = (rb:value n)
                                 collect (list* (x-at e y) (nl e)))
                           winding-out-right))))
             (sort-for-intersect1 (e)
               ;; call with single intersect event
               (when (winding-changed sweep)
                 ;; if previous event affected winding #s at other X
                 ;; values, update edges between that event and this
                 ;; one
                 (deferred-update-winding sweep (left e) verbose))
               (let ((a (left e))
                     (b (right e)))
                 (when verbose
                   (format t "sort 1: @ ~s = ~s~% a ~s~%   x=~s, angle=~s~%~
b ~s~%   x=~s, angle=~s~%"
                           y (%sort-rb/a a b y)
                           (nl a) (x-at a y) (%tangent-x-at a y)
                           (nl b) (x-at b y) (%tangent-x-at b y)))
                 ;; don't call normal sort function, since we know X
                 ;; values should be the same (but might not be due to
                 ;; fp).
                 (when verbose
                   (format t "check swap:~%  ~s~%  ~s~%"
                           (nl a) (nl b)))
                 (unless (%sort-rb/a a b y)
                   (when verbose
                     (format t " ///swap @ intersection~%   ~s~%   ~s~%"
                             (nl a) (nl b)))
                   #++(break "swap1")
                   (let* ((p (rb:previous (%node a)))
                          (wn (if p
                                  (winding-number (rb:value p))
                                  0))
                          (wb (+ wn (maybe-winding-sign b)))
                          (wa (+ wb (maybe-winding-sign a))))
                     (when (previous-windings a)
                       (assert (/= y (car (previous-windings a)))))
                     (setf (previous-windings a)
                           (list y wn (on-boundary a) wa))
                     (when (previous-windings b)
                       (assert (/= y (car (previous-windings b)))))
                     (setf (previous-windings b)
                           (list y wa (on-boundary b) wb))
                     (setf winding-out-left wn
                           winding-out-right wa
                           (winding-number a) wa
                           (winding-number b) wb
                           (on-boundary a) (winding-boundary-p wb wa)
                           (on-boundary b) (winding-boundary-p wn wb)))
                   (rotatef (rb:value (%node a)) (rb:value (%node b)))
                   (rotatef (rb::key (%node a)) (rb::key (%node b)))
                   (rotatef (%node a) (%node b)))
                 ;; Also, force the X values to be the same so other
                 ;; checks don't get confused.
                 #++(when (/= (x-at a y) (x-at b y))
                      (assert (< (abs (- (x-at a y) (x-at b y)))
                                 (* 16384  double-float-epsilon)))
                      (setf (slot-value b 'x) (slot-value a 'x)))))
             (find-intersection-bounds (ref)
               ;; when looking for new intersections, we want to check
               ;; nodes adjacent to ones involved in current event(s),
               ;; so find those nodes (if any), and in the process
               ;; collect results for this set of events. This also
               ;; catches any new intersections we missed due to
               ;; overlapping edges (since only one of them would have
               ;; been adjacent to a crossing edge in rb tree, others
               ;; might not have been included in intersect events)
               ;; this actually returns with START/END as first/last
               ;; node involved in intersection, rather than one past,
               ;; so we can use them for sort and then expand by 1 more.
               #++(setf first nil)
               (let ((eps (* 16 (b::%bcs-eps (edge ref)))))
                 (flet ((~=x (n)
                          (let* ((xn (x-at (rb:value n) y))
                                 (r (< (abs (- x xn))eps)))
                            (when (and r (/= x xn))
                              (setf (slot-value (rb:value n) 'x) x))
                            r)))
                   (let ((n1 (%node ref)))
                     (assert (~=x n1))
                     (loop for n = n1 then prev
                           for v = (rb:value n)
                           for prev = (rb:previous n)
                           while (and prev (~=x prev))
                           finally (setf istart n)))
                   (let ((n1 (rb:next (%node ref))))
                     (if (and n1 (~=x n1))
                         (loop for n = n1 then next
                               for v = (rb:value n)
                               for next = (rb:next n)
                               while (and next (~=x next))
                               finally (setf iend n))
                         (setf iend (%node ref))))
                   (when *check*
                     (assert (not (a:xor istart iend)))
                     (when istart
                       (assert (~=x istart))
                       (assert (~=x iend))
                       (when (rb:previous istart)
                         (assert (< (x-at (rb:value (rb:previous istart)) y) x)))
                       (when (rb:next iend)
                         (assert (> (x-at (rb:value (rb:next iend)) y) x)))
                       (when verbose
                         (format t "fib @ ~s~%" (nl ref))
                         (loop for n = (or (rb:previous (rb:previous istart))
                                           (rb:previous istart)
                                           istart)
                                 then (rb:next n)
                               do (format t "::~a~a ~s ~s~%"
                                          (if (eql n istart) "S" " ")
                                          (if (eql n iend) "E" " ")
                                          (ri (x-at (rb:value n) y))
                                          (nl (rb:value n)))
                               until (eql n (or (rb:next (rb:next iend))
                                                (rb:next iend)
                                                iend))
                               ;; if we hit end, start/end were out of order?
                               do (assert n)))))))
               ;; update intersection-in before we sort or update
               ;; windings, since we want incoming order and boundary
               ;; flags (possibly should do this before actually
               ;; adding new edges, but for now easier to just remove
               ;; those since we want the start/end values from here)
               (loop for n = istart then (rb:next n)
                     for v = (rb:value n)
                     do #++ (format t "+?? ~s ~s ~s add ~s ~s~%"
                                    (on-boundary v)
                                    (third (previous-windings v))
                                    (not (eql :add (gethash v hresult)))
                                    (previous-windings v) (nl v))
                        (when (and (on-boundary v)
                                   (third (previous-windings v))
                                   (not (eql :add (gethash v hresult))))
                          #++(format t "++++ add ~s ~s~%"
                                     (previous-windings v) (nl v))
                          (push v intersect-in))
                     until (eql n iend))
               (setf intersect-in (nreverse intersect-in))
               ;; calculate winding #s before and after this intersection,
               ;; when entering the intersection
               (let ((ws (previous-windings (rb:value istart)))
                     (we (previous-windings (rb:value iend))))
                 (when verbose
                   (format t "&&& windings-in,prev = ~s | ~s~%" ws we))
                 (unless ws
                   (assert (eql :add (gethash (rb:value istart) hresult)))
                   ;; can't look at previous node since it was
                   ;; already updated, but next non-add node should
                   ;; have the correct value we want
                   (loop for n = (rb:next istart) then (rb:next n)
                         for v = (when n (rb:value n))
                         while n
                         when (not (eql :add (gethash v hresult)))
                           do (setf ws (previous-windings v))
                              (loop-finish))
                   (when verbose
                     (format t "    ws -> ~s~%" ws)))
                 (setf winding-in-left (if ws (second ws) 0))
                 (unless we
                   (assert (eql :add (gethash (rb:value iend) hresult))))
                 ;; in other direction, we can just look at next
                 ;; node after intersection, but have to use in
                 ;; value from that
                 (cond
                   ((and (not we) (rb:next iend))
                    (let ((n (rb:next iend)))
                      (setf we (previous-windings (rb:value n)))
                      (assert we)
                      (when verbose
                        (format t "    we -> ~s~%" (second we)))
                      (setf winding-in-right (second we))))
                   (t (setf winding-in-right (if we (fourth we) 0)))))
               #++(let ((p (rb:previous istart)))
                    (setf winding-in-left (if p (winding-number (rb:value p)) 0)))
               #++(let ((n iend))
                    ;; not sure if we can safely calculate
                    ;; winding-in-right by looking at winding # and sign
                    ;; of next element, since redundant stacks and end of
                    ;; segment might affect it? possibly should just
                    ;; store winding # before edge in addition to storing
                    ;; winding # after edge, so it could be used directly
                    ;; here?
                    (loop while (and n (eql :add (gethash (rb:value n) hresult)))
                          do (setf n (rb:previous n)))
                    (setf winding-in-right
                          (if n (winding-number (rb:value n)) 0))
                    #++(let* ((n2 (next n))
                              (w2 (if n2
                                      (- (winding-number (rb:value n2))
                                         (winding-sign (rb:value n2)))
                                      winding-in-left)))
                         (unless (= w2 winding-in-right)
                           (format t "sweep:~%")
                           (rb::walk rb
                                     (lambda (a)
                                       (let* ((x (ri (x-at a y))))
                                         (format t "~a~a~a ~s~%"
                                                 (if (eql a (rb:value istart))
                                                     "S" " ")
                                                 (if (eql a (rb:value iend))
                                                     "E" " ")
                                                 x (nl a)))))
                           ;; todo: figure out which way of calculating it
                           ;; is right if this doesn't match
                           (break "winding mismatch? ~s ~s"
                                  winding-in-right w2)))))
             (expand-bounds ()
               ;; collecting results here instead of
               ;; find-intersection-bounds, since we want them sorted
               ;; later. todo: refactor this. Collect intersect-out
               ;; here, after sorting and updating winding
               (loop for n = istart then (rb:next n)
                     for v = (rb:value n)
                     do (push v result)
                        (unless (gethash v hresult)
                          (setf (gethash v hresult) :intersect))
                        (when verbose
                          (format t "eb: ~s?~s ~s~%" (on-boundary v)
                                  (gethash v hresult)
                                  (nl v)))
                        (when (and (on-boundary v)
                                   (not (eql :del (gethash v hresult))))
                          (push v intersect-out))
                     until (eql n iend))
               (setf intersect-out (nreverse intersect-out))
               ;; store nodes before/after current intersections, if
               ;; any for use when checking for effects on future
               ;; winding #s
               (setf xstart (rb:previous istart))
               (setf xend (rb:next iend))
               ;; expand start/end by 1 node if possible to prepare
               ;; for future intersection check
               (setf istart (or xstart istart))
               (setf iend (or xend iend))))
      ;; sometimes we can't map back from y value to curve position
      ;; when at start/end, so manually update the cached x,y,etc values
      ;; for those (already done in caller?)
      #++
      (loop for i in e
            do (typecase i
                 (start-event
                  (let ((a (start i)))
                    (%update-cached* a y (x i) (t1 a))))
                 (end-event
                  (let ((d (end i)))
                    (%update-cached* d y (x i) (t2 d))))))
      (cond
        ((not e1) (error "no event?"))
        ;; single event, start
        ((and (not (cdr e)) (typep e1 'start-event))
         (when verbose
           (format t "single add:y=~s: ~s~%= ~s~%"
                   y e (nl (start (car e)))))
         (add e1)
         (when *check*
           (assert (a:set-equal (rb::to-list rb) (dbg-added sweep))))
         (find-intersection-bounds (start (car e)))
         (update-winding istart iend)
         (expand-bounds))
        ;; single event, end
        ((and (not (cdr e)) (typep e1 'end-event))
         (when verbose
           (format t "single del:y=~s: ~s~%= ~s~%"
                   y e (nl (end (car e)))))
         (find-intersection-bounds (end e1))
         (setf (gethash (end e1) hresult) :del)
         (update-winding istart iend)
         (expand-bounds)
         (del e1)
         (when *check*
           (assert (a:set-equal (rb::to-list rb) (dbg-added sweep)))))
        ((and (not (cdr e)) (typep e1 'horizontal-event))
         ;; probably shouldn't happen, most likely failed to merge 2
         ;; consecutive horizontal edges?
         (restart-case
             (break "horizontal edge event without adjacent edge? e = ~s~%" e)
           (error () (error "horizontal edge event without adjacent edge? e = ~s~%" e))))
        ((and (not (cdr e)) (typep e1 'horizontal-intersect-event))
         (assert (not result))
         (let ((ee1 (edge e1)))
           (when verbose
             (format t "single horizontal intersect:y=~s: ~s~%h=~s~%e=~s~%"
                     y e (mapcar 'nl horizontal)
                     (nl (edge e1))))
           (assert (not (gethash ee1 hresult)))
           (setf (gethash ee1 hresult) :intersect)
           #++(push ee1 result)
           (find-intersection-bounds ee1))
         (update-winding istart iend)
         (expand-bounds))
        ;; single event, intersect
        ((not (cdr e))
         (when verbose
           (format t "single intersect:y=~s: ~s~%l ~s~%r ~s~%"
                   y e (nl (left (car e))) (nl (right (car e)))))
         (assert (typep e1 'intersect-event))
         (when verbose
           (unless (%node (left e1)) (format t "  left edges not in sweep~%"))
           (unless (%node (right e1)) (format t "  right edge not in sweep~%")))
         (when (and (not (%node (left e1)))
                    (not (%node (right e1))))
           ;; assuming it was already handled by a nearby intersection
           ;; event now that they accept x values within a small range
           (return-from update-sweep-1 nil))
         (assert (%node (left e1)))
         (assert (%node (right e1)))
         (setf istart (prev (%node (left e1))))
         (setf iend (next (%node (right e1))))
         (find-intersection-bounds (left e1))
         (if (and (eql istart (left e1))
                  (eql iend (right e1)))
             ;; fast path for single intersecting pair
             (sort-for-intersect1 e1)
             ;; if we found more intersections, we need to include
             ;; them in sort
             (sort-for-intersect istart iend))
         (expand-bounds))
        (t
         (let (se
               ee
               ie
               he
               hie)
           (when *check*
             (assert (a:set-equal (rb::to-list rb) (dbg-added sweep))))
           (loop for x in e
                 do (etypecase x
                      (horizontal-event (push x he))
                      (horizontal-intersect-event (push x hie))
                      (start-event (push x se))
                      (end-event
                       (setf (gethash (end x) hresult) :del)
                       (push x ee))
                      (intersect-event (push x ie))))
           (when verbose
             (format t "se=~s,ee=~s,ie=~s~%"
                     (length se) (length ee) (length ie))
             (when se (format t "se:~%~{ ~s~%~}"
                              (mapcar (a:compose 'nl 'start) se)))
             (when ee (format t "ee:~%~{ ~s~%~}"
                              (mapcar (a:compose 'nl 'end) ee)))
             (when he (format t "he:~%~{ ~s~%~}"
                              (mapcar 'nl he)))
             (when hie (format t "hie:~%~{ ~s~%~}"
                               (mapcar 'nl hie)))
             (when ie
               (format t "ie:~%~{ ~s~%~}"
                       (loop for x in ie collect (list (nl (left x))
                                                       (nl (right x)))))))
           (when se
             (loop for s in se do (add s))
             (when *check*
               (assert (a:set-equal (rb::to-list rb) (dbg-added sweep)))))
           (when (or ie se ee hie)
             (find-intersection-bounds (cond
                                         (ie
                                          (left (car ie)))
                                         (se
                                          (start (car se)))
                                         (ee
                                          (end (car ee)))
                                         (hie
                                          (edge (car hie)))))
             (if ie
                 (sort-for-intersect istart iend)
                 (update-winding istart iend))
             (expand-bounds))
           (when he
             ;; include any new horizontal edge in intersections
             (loop for i in he
                   do #++ (push (edge i) horizontal)
                      (dq:enqueue active-horizontal (edge i)
                                  (coerce (x2 i) 'double-float)))
             ;; add events for any new intersections with current (set
             ;; of) horizontal edge(s)
             (let* ((x1 (or (horizontal-edge-max sweep) (x (car he))))
                    (x2 (loop for i in he maximize (x2 i))))
               (when (> x2 x1)
                 (when verbose
                   (format t "extend horizontal span from ~s (~s) to ~s (@ ~s)~%"
                           (horizontal-edge-max sweep) x1 x2 (x (car he))))
                 (rb::%map-range rb
                                 (lambda (a)
                                   (let ((ax (x-at a y)))
                                     (when verbose
                                       (format t "  -> add hintersect @ ~s~%"
                                               ax))
                                     #++(break " ~s - ~s - ~s~% ~s" x1 x x2 a)
                                     (dq:enqueue
                                      xq
                                      (make-instance 'horizontal-intersect-event
                                                     :x ax :y y
                                                     :at (slot-value a 'at)
                                                     :edge a)
                                      (coerce ax 'double-float))))
                                 ;; don't include endpoints. Both
                                 ;; endpoints should be included with
                                 ;; start or end events for adjacent
                                 ;; non-horizontal edge.
                                 (lambda (a)
                                   #++(when verbose
                                        (format t "l:~a = ~s~%"
                                                (x-at a y)
                                                (> (x-at a y) x1)))
                                   (> (x-at a y) x1))
                                 (lambda (a)
                                   #++
                                   (format t "h:~a = ~s~%"
                                           (x-at a y)
                                           (< (x-at a y) x2))
                                   (< (x-at a y) x2)))
                 (setf (horizontal-edge-max sweep) x2))))
           (when hie
             ;; add to results any edges we marked as intersecting
             ;; with horizontal edges at this Y
             (when *check*
               (when (or ie se)
                 ;; if we have any intersections or new edges at this
                 ;; point, they should have included the ones found by
                 ;; the horizontal edge check already
                 (loop for i in hie do (assert (member (edge i) result)))))
             (unless (or ie se)
               (loop for i in hie
                     for ei = (edge i)
                     do #++(when *check*(assert (not (member ei result))))
                     #++(push ei result)
                        (unless (gethash ei hresult)
                          (setf (gethash ei hresult) :intersect)))))
           (when *check*
             (assert (a:set-equal (rb::to-list rb) (dbg-added sweep))))
           (loop for e in ee do (del e))
           (when *check*
             (assert (a:set-equal (rb::to-list rb) (dbg-added sweep)))))))
      (progn
        ;; todo:
        #+ when (or ie se ee)
        ;; don't need to check for intersections if we only had a
        ;; horizontal intersection, since that doesn't affect sweep
        ;; tree
        (add-future-intersections istart iend))
      (when verbose
        (format t "sweep @ ~s,~s =~%" (float x) (float y))
        (when (and (rb::root rb) (rb:value (rb::root rb)))
          (format t "  root = ~s~%" (nl (rb:value (rb::root rb))))))
      (when *check*
        (let ((prev most-negative-double-float)
              (prev-node nil)
              (bad 0))
          (rb::walk rb (lambda (a)
                         (let* ((t1 (t1 a))
                                (t2 (t2 a))
                                (x (x-at a y))
                                (ok (or (not prev-node)
                                        (<= (- prev x)
                                            (* 32 (max (abs y) (abs prev))
                                               double-float-epsilon)))))
                           (when verbose
                             (format t "~a~f ~s~%"
                                     (if ok " " "!") x (nl a)))
                           (unless ok
                             (let ((epy (* 1 (abs y)
                                           double-float-epsilon)))
                               (when verbose
                                 (break "a~s < p~s?~%a~s ? p~s~%~
 a~s ? p~s~%~
 a<p:~s. p<a:~s~%~
 a<p:~s. p<a:~s~%"
                                        x prev
                                        (%x-at (edge a)
                                               (+ y epy)
                                               (min t1 t2) (max t1 t2)
                                               (split-point a)
                                               :errorp nil)
                                        (let ((t1 (t1 prev-node))
                                              (t2 (t2 prev-node)))
                                          (%x-at (edge prev-node)
                                                 (+ y epy)
                                                 (min t1 t2) (max t1 t2)
                                                 (split-point prev-node)
                                                 :errorp nil))
                                        (%x-at (edge a)
                                               (+ y 0.15)
                                               (min t1 t2) (max t1 t2)
                                               (split-point a)
                                               :errorp nil)
                                        (let ((t1 (t1 prev-node))
                                              (t2 (t2 prev-node)))
                                          (%x-at (edge prev-node)
                                                 (+ y 0.15)
                                                 (min t1 t2) (max t1 t2)
                                                 (split-point prev-node)
                                                 :errorp nil))
                                        (%sort-rb a prev-node y)
                                        (%sort-rb prev-node a y)
                                        (ignore-errors
                                         (%sort-rb a prev-node (+ y epy)))
                                        (ignore-errors
                                         (%sort-rb prev-node a (+ y epy))))))
                             (incf bad))
                           (setf prev x
                                 prev-node a))))
          (when *check*
            (assert (a:set-equal (rb::to-list rb) (dbg-added sweep))))
          (when verbose
            (format t " ===~%"))
          (if verbose
              (when (plusp bad) (format t "bad = ~s" bad))
              (assert (zerop bad)))
          (when *check*
            (rb::%red-black-tree/check-invariants rb)))))
    (when verbose
      (format t "/// result @ ~s ~s =~%~s~%" x y result)
      (when result
        (format t "    == ~s~%" (mapcar 'nl result))))
    ;; remove any horizontal spans that completed at this event
    (unless (zerop (dq:size active-horizontal))
      (loop for i = (dq:peek active-horizontal)
            while (and i (<= (x2 i) x))
            do (when verbose
                 (format t " finished horizontal span ~s~%"
                         (nl i)))
               (dq:dequeue active-horizontal))
      (when (zerop (dq:size active-horizontal))
        (setf (horizontal-edge-max sweep) nil)))
    ;; decide if there is a horizontal boundary edge leaving this
    ;; intersection (true if # of leftwards and rightwards edges if
    ;; different) and if so save widest
    (let ((n 0)
          (last nil))
      (dq:map active-horizontal
              (lambda (a)
                (let ((e (edge a)))
                  (incf n (if (< (b::s-rx1 e) (b::s-rx2 e)) +1 -1)))
                (unless (and last (<= (x2 a) (X2 last)))
                  (setf last a))))
      (when (and verbose last (zerop n))
        (format t "horizontal edges canceled~%")
        (dq:map active-horizontal
                (lambda (a) (format t "  ~s~%" (nl a)))))
      (setf horizontal (unless (zerop n) (list last))))

    ;; check for overlapped edges, and remove any sets that cancel out
    (when (cdr result)
      ;; assuming RESULT is still ordered by %sort-rb/a (reversed is
      ;; OK though)
      (let* ((work result)
             (r)
             (stack nil)
             (stacks nil))
        (when verbose
          (format t "** check stacks~%"))
        (flet ((next ()
                 (loop while (and work (eql :del (gethash (car work) hresult)))
                       do (pop work))
                 (pop work)))
          (setf r (next))
          (when r
            (setf stack (list r))
            (loop for i = (next)
                  while i
                  do (when verbose
                       (format t "~s: ~s =? ~s~%"
                               (%eql/a i r y) (nl (edge r)) (nl (edge i))))
                  when (%eql/a i r y)
                    do (let ((ei (edge i))
                             (er (edge r)))
                         (cond
                           ((and (typep ei 'b::segment) (typep er 'b::segment))
                            (when verbose (format t " stacked segments~%"))
                            (push i stack))
                           ((and (typep ei 'b::bezier2) (typep er 'b::bezier2))
                            (when verbose (format t " stacked bezier2~%"))
                            (push i stack))
                           (t (break "~s ~s~%~s ~s~%"
                                     (type-of ei) ei
                                     (type-of er) er))))
                  else
                    do (when (cdr stack)
                         (when verbose (format t " finished stack~%"))
                         (push stack stacks))
                       (when verbose (format t " ->next~%"))
                       (setf r i)
                       (setf stack (list i))
                  finally (when (cdr stack)
                            (when verbose (format t " finished stack2~%"))
                            (push stack stacks)))))
        (when stacks
          (when verbose
            (format t "stacks~{~&-~{~s~%~^ ~}~}" (mapcar 'nl stacks)))
          (loop for stack in stacks
                for l = (length stack)
                for s = (reduce '+ stack :key 'winding-sign)
                do (cond
                     ((zerop s)
                      (when verbose
                        (format t "deleting redundant stack = (+ ~a) = ~s~%"
                                (mapcar 'winding-sign stack) s))
                      (loop for i in stack
                            do (when verbose
                                 (format t "  ~s (was ~s)~%"
                                         (nl i) (gethash i hresult)))
                               (setf (on-boundary i) nil)
                               (if (eql :add (gethash i hresult))
                                   (if *check*
                                       (setf (gethash i hresult) :duplicate)
                                       (remhash i hresult))
                                   (setf (gethash i hresult) :del)))
                      (setf intersect-out
                            (remove-if-not 'on-boundary intersect-out)))
                     ((/= (abs s) l)
                      (when verbose
                        (format t "collapsing partially redundant stack = (+ ~a) = ~s / ~s~%"
                                (mapcar (a:compose 'ri 'winding-sign) stack)
                                (ri s) l))
                      (let ((r nil)
                            (s2 0)
                            (boundary nil)
                            (sign (signum s)))
                        (loop for i in stack
                              do (when (on-boundary i)
                                   (setf boundary t))
                                 (cond
                                   ;; for now just keep first edge(s)
                                   ;; with correct sign. not sure if it
                                   ;; matters which we pick?
                                   ((and (= sign (maybe-winding-sign i))
                                         ;; keep enough to have same
                                         ;; final winding after stack,
                                         ;; since that might matter
                                         (/= s2 s))
                                    (when verbose
                                      (format t "  ~s/~s keep ~s (was ~s)~%"
                                              s2 (ri s) (nl i)
                                              (gethash i hresult)))
                                    (incf s2 (maybe-winding-sign i))
                                    (setf (on-boundary i) nil)
                                    (push i r))
                                   (t
                                    (when verbose
                                      (format t "  drop ~s (was ~s)~%"
                                              (nl i) (gethash i hresult)))
                                    (setf (on-boundary i) nil)
                                    (if (eql :add (gethash i hresult))
                                        (if *check*
                                            (setf (gethash i hresult) :duplicate)
                                            (remhash i hresult))
                                        (setf (gethash i hresult) :del)))))
                        ;; if we saw any boundary edges in the stack,
                        ;; mark the (first) one we kept as a boundary.
                        (setf (on-boundary (car r)) boundary)
                        (setf intersect-out
                              (remove-if-not 'on-boundary intersect-out))))
                     (t
                      ;; not removing any edges for now, make sure
                      ;; there is at most 1 boundary. (not sure 3 is
                      ;; possible if rest is working correctly, but 2
                      ;; might happen if stack is between ±1, in which
                      ;; case it cancels out so remove both)
                      (let ((n (loop for i in stack
                                     count (on-boundary i))))
                        (case n
                          ((0 1) ;; ok, do nothing
                           (when verbose
                             (format t " stack boundary count ~s~%" n)))
                          (2 ;; cancels out, remove
                           (when verbose
                             (format t " stack boundary count ~s, clearing~%" n))
                           (loop for i in stack do (setf (on-boundary i) nil))
                           (setf intersect-out
                                 (remove-if-not 'on-boundary intersect-out)))
                          (t ;; not sure if this can happen inspect further
                           (break "too many boundaries on stack? ~s~%~{~s~%~}"
                                  n (mapcar 'nl stack)))))))))))
    ;; add active horizontal edges to results
    (when (and horizontal result)
      (when verbose
        (format t "result += ~s~%" (mapcar 'nl horizontal)))
      (loop for i in horizontal
            do (push i result)
               (setf (gethash i hresult) :horizontal)))
    (when result
      (when *check*
        (loop for i in result
              do (assert (gethash i hresult))
                 (when (eql (gethash i hresult) :duplicate)
                   (remhash i hresult)))
        (assert (a:set-equal (remove-duplicates result) result)))
      (assert (and winding-out-left winding-out-right))
      (when *check*
        (macrolet ((assert-p (x)
                     (a:with-gensyms (r)
                       (let ((f (format nil "~s" x)))
                         `(let ((,r ,x))
                            (unless ,r
                              (format t "assert failed:~% ~a~%" ,f)
                              (format t " winding in = ~s, winding out = ~s~%"
                                      winding-out-left winding-out-right)
                              (let ((wn 0))
                                (rb::walk (rb sweep)
                                          (lambda (a)
                                            (incf wn (maybe-winding-sign a))
                                            (format t "~s|~s ~a~a~a~a~a~s~%"
                                                    (ri wn)
                                                    (ri (winding-number a))
                                                    (if (and xstart
                                                             (eql a (rb:value xstart)))
                                                        "S" "")
                                                    (if (eql a (rb:value istart))
                                                        "s" "")
                                                    (if (gethash a hresult)
                                                        "i" "")
                                                    (if (and xend
                                                             (eql a (rb:value xend)))
                                                        "E" "")
                                                    (if (eql a (rb:value iend))
                                                        "e" "")
                                                    (nl a)))))
                              (error "assert failed:~% ~a~%" ,f)))))))
          (when xstart
            (assert-p (= winding-out-left (winding-number (rb:value xstart)))))
          (when (and xend (rb:previous xend))
            (assert-p (= winding-out-right
                         (winding-number (rb:value (rb:previous xend))))))))
      (when (and xend
                 (/= (+ winding-out-right
                        (maybe-winding-sign (rb:value xend)))
                     (winding-number (rb:value xend))))
        (setf (winding-number (rb:value xend))
              (+ winding-out-right
                 (maybe-winding-sign (rb:value xend))))
        (assert (not (eql (rb::tree xend) :deleted)))
        (when verbose
          (let ((*print-circle* t))
            (format t "add deferred winding update:~% ~s~%"
                    (nl (rb:value xend)))
            (format t " (after ~s~%)"
                    (when (rb:previous xend)
                      (nl (rb:value (rb:previous xend)))))))
        (setf (winding-changed sweep) xend))

      (update-sweep-contours sweep
                             intersect-in intersect-out
                             x y
                             winding-in-left winding-in-right
                             winding-out-left winding-out-right
                             horizontal
                             :verbose verbose)
      (list* x y result))))

(defun update-sweep (sweep queue &key verbose)
  (let ((r nil)
        (skip nil)
        ;; to avoid problems with fp accuracy, we force the X values
        ;; of all edges with events on current sweep to match those
        ;; events. We need to do that before processing any of the
        ;; events since walking the rb tree might depend on edges
        ;; affected by later events. Normal intersections only add
        ;; events to strictly greater Y values, so that isn't a
        ;; problem for them. Horizontal edges need to add events to
        ;; same line though, so we first remove all events for current
        ;; Y from main pq, set their edges's X values, and add them to
        ;; another priority queue for just this sweep. Normal
        ;; intersections are added to main pq, while horizontal
        ;; intersections are added to the sweep pq.
        (xq (dq:make-queue)))
    (flet ((ys ()
             (unless (zerop (q:size queue))
               (loop for e = (q:dequeue queue)
                     for p = (q:peek queue)
                     while e
                     collect e
                     until (or (zerop (q:size queue))
                               (not p)
                               (/= (y e) (y p)))))))
      (unwind-protect
           (progn
             (setf r
                   (loop
                     for e = (ys)
                     for y = (when e (y (car e)))
                     for r1 = nil
                     while e
                     do (assert (zerop (dq:size (horizontal-edges sweep))))
                        (assert (zerop (dq:size xq)))
                        (when verbose
                          (format t "start sweep for y=~s~%~{   ~s~%~}"
                                  (y (car e))
                                  (mapcar 'nl e)))
                        ;; fixme: add an O(N) function to
                        ;; reinitialize queue from sorted input
                        (loop for i in e
                              do (dq:enqueue xq i (df (x i))))
                        ;; update Y in rb tree
                        (set-y sweep (y (car e)))
                        ;; merge X values of all events involving
                        ;; same set of edges (todo)

                        ;; force all edges involved in events at
                        ;; this y value to have same X value as
                        ;; event, so RB tree stays consistent. In
                        ;; particular, we need to make sure X
                        ;; values of intersection events are the
                        ;; same since FP noise might cause them to
                        ;; be out of order, but also avoid
                        ;; problems caused by mapping back from Y
                        ;; to X through T at split points of
                        ;; curves that had Y extrema and were
                        ;; split.
                        (restart-case
                            (progn
                              (flet ((u (n x y at &optional force)
                                       (let ((st (or (and (/= (t1 n) 0)
                                                          (/= (t1 n) 1)
                                                          (t1 n))
                                                     (and (/= (t2 n) 0)
                                                          (/= (t2 n) 1)
                                                          (t2 n)))))
                                         (if (and st
                                                  ;; should only split
                                                  ;; beziers, so don't
                                                  ;; need to test for
                                                  ;; segments here
                                                  (= (b::vy (b::eval-at/b2/fast
                                                             (edge n)
                                                             st))
                                                     y))
                                             ;; if at a split endpoint, use
                                             ;; that
                                             (%update-cached* n y x st t)
                                             (%update-cached* n y x at)))
                                       (setf (slot-value n 'at) at)
                                       (setf (slot-value n 'x) x)))
                                (loop for e1 in e
                                      do (etypecase e1
                                           ;; horizontal edges aren't
                                           ;; added to rb tree, so do
                                           ;; nothing
                                           (horizontal-event)
                                           (horizontal-intersect-event
                                            (u (edge e1)
                                               (x e1) (y e1)
                                               (at e1)))
                                           (start-event
                                            (u (start e1)
                                               (x e1) (y e1)
                                               (at e1)
                                               t))
                                           (end-event
                                            (u (end e1)
                                               (x e1) (y e1)
                                               (at e1)
                                               t))
                                           (intersect-event
                                            (u (left e1)
                                               (x e1) (y e1)
                                               (at1 e1))
                                            (u (right e1)
                                               (x e1) (y e1)
                                               (at2 e1))))))
                              (when verbose
                                (format t "@@ y = ~s, x = ~s~%"
                                        (float (y (car e)))
                                        (mapcar (a:compose 'float 'x) e)))
                              ;; call update-sweep-1 for each distinct X
                              ;; value at this Y value
                              (setf
                               r1 (loop
                                    for ex = (loop
                                               for x = (dq:dequeue xq)
                                               do (assert x)
                                               collect x
                                               until (or (zerop (dq:size xq))
                                                         (/= (x x)
                                                             (x (dq:peek xq)))))
                                    when (update-sweep-1 sweep ex queue xq
                                                         :verbose verbose)
                                      collect it
                                    until (zerop (dq:size xq)))))
                          (give-up () "give up"
                            (setf skip t)))
                        (unless skip
                          (when (winding-changed sweep)
                            ;; if last event of previous sweep
                            ;; affected future winding #s, update
                            ;; them
                            (deferred-update-winding sweep nil verbose))
                          (when *check*
                            (with-simple-restart (ignore "ignore")
                              (let ((wn 0))
                                (when verbose
                                  (format t "check windings @ ~s~%" y))
                                (rb::walk (rb sweep)
                                          (lambda (a)
                                            (incf wn (maybe-winding-sign a))
                                            (when verbose
                                              (format t "~s|~s ~s x=~s~%"
                                                      (ri wn)
                                                      (ri (winding-number a))
                                                      (nl a)
                                                      (ri (x-at a y))))
                                            (when (/= wn (winding-number a))
                                              (error "winding # mismatch"))))))))

                        (when verbose
                          (format t "======================================~%"))
                     until skip
                     append r1)))
        (when verbose
          (let* ((o (dbg-ordering sweep))
                 (ops (dbg-events sweep))
                 (v (sort (mapcar 'second ops)
                          (lambda (a b)
                            (member b (gethash a (first o))))))
                 (index (make-hash-table)))
            (loop for i from 1
                  for k in v
                  do (setf (gethash k index) i))
            (format t "~a~%"
                    (cons 'check
                          (loop for (o k) in (reverse ops)
                                collect (ecase o
                                          (:add (gethash k index))
                                          (:del (- (gethash k index))))))))))
      (with-simple-restart (ignore "ignore")
        (when t ;; *check*
          (unless (zerop (hash-table-count (contour-index sweep)))
            (format t "???didn't close all countours?~%")
            (let ((ci (contour-index sweep)))
              (loop for i in (a:hash-table-keys ci)
                    do (format t " ~s~%" (nl i)))))
          (assert (zerop (hash-table-count (contour-index sweep))))
          (assert (zerop (dq:size (horizontal-edges sweep))))
          (assert (not (rb:min (rb sweep))))
          (assert (not (winding-changed sweep)))))
      (when verbose
        (format t ">>>>>>>>>>> result = ~%~s~%" r))
      r)))

(defun fix-shape (shape &key verbose)
  (let* ((*id* 10000)
         (s (b::clean-shape shape :verbose verbose)))
    (let ((sc (make-events s))
          (sw (make-sweep s)))
      (update-sweep sw sc :verbose verbose)
      (if (finished-contours sw)
          (b::%edit-shape-to-shape
           (mapcar 'b::%reverse-contour (finished-contours sw))
           :metadata (b::metadata shape))
          s))))
