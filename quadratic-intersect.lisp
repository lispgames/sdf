(in-package #:sdf/quadratic-intersect)

;; Curve Intersection using Bezier Clipping
;; http://nishitalab.org/user/nis/cdrom/cad/CAGD90Curve.pdf



(defclass bezier ()
  ;; we keep the original curve and try to do most operations on that
  ;; where possible
  ((original :initform (d::b2 -1 1 0 -1 1 1)
             :reader original :initarg :original)
   ;; current clipped/split curve
   (currnet :initform (d::b2 -1 1 0 -1 1 1)
            :reader current :initarg :current)
   ;; T values of range of current segment in original bezier
   (t1 :initform 0d0 :reader t1 :initarg :t1)
   (t2 :initform 1d0 :reader t2 :initarg :t2)
   ;; original range of T values, in case we started with an already
   ;; clipped curve
   (o1 :initform 0d0 :reader o1)
   (o2 :initform 1d0 :reader o2)
   ;; cached t2-t1
   (interval :initform 0d0 :reader interval :initarg :interval)
   ;; straight-line distance from start to end of curve
   (base :initform 0d0 :reader base :initarg :base)
   ;; epsilon calculated from largest value in original bezier
   (eps :initform 0d0 :reader epsilon :initarg :epsilon)
   ;; implicit curve containing this bezier
   (curve :initform nil :reader curve :initarg :curve)))

(defmethod initialize-instance :after ((o bezier) &key)
  (setf (slot-value o 'o1) (t1 o))
  (setf (slot-value o 'o2) (t2 o)))

;;; since we can't reliably distinguish some near-tangent curves over
;;; wide ranges, we return intersections as a region rather than just
;;; a point. Not sure what callers will need, so for now returning
;;; region as an axis aligned bounding box and tight bounding box
;;; (tight bounding box is only calculated for cases where it is
;;; expected to be significantly different from AABB, not sure if it
;;; will return the AABB or nil in the case where they are expected to
;;; be similar).
;;
;;; If possible, try to minimize # of bits used for bounding boxes:
;;    round bounds (outwards) to a power of 2 near the target minimum
;;    size?
;;
;;    for tight bounding box, calculate lines so that A,B in line
;;    equation are 16 (or whatever) bits?
(defclass intersect-region ()
  ;; curves being intersected (probably not needed, but useful for
  ;; debugging and makes it more obvious which T values go with which
  ;; curve)
  ((a :initarg :a :reader a)
   (b :initarg :b :reader b)
   ;; ranges of t values included in the intersection (generally will
   ;; be smaller than intersection of curve with bounding region, but
   ;; might be slightly larger due to FP error if bounding box happens
   ;; to match last clip during recursion)
   (at0 :initarg :at0 :reader at0)
   (at1 :initarg :at1 :reader at1)
   (bt0 :initarg :bt0 :reader bt0)
   (bt1 :initarg :bt1 :reader bt1)
   ;; T values used to calculate intersection point (usually midpoint
   ;; of *t[01], but might be snapped to an endpoint or otherwise
   ;; refined at some point) actually returned point isn't usually on
   ;; either curve, so not sure if this is useful, or if it should be
   ;; NIL in that case, or what

   ;;(at :initarg :at :reader at)
   ;;(bt :initarg :bt :reader bt)

   ;; pair of fat lines representing axis-aligned bounding box (todo:
   ;; decide if this is actually a useful representation, and/or
   ;; abstract it more)
   (aabb-x :initarg :aabb-x :reader aabb-x)
   (aabb-y :initarg :aabb-y :reader aabb-y)
   ;; pair of fat lines representing tighter bounding box (probably
   ;; only non-nil when curves are nearly the same, in which case X
   ;; line will probably approximate the line between endpoints of
   ;; each curve, and Y is perpendicular to that)
   (obb-x :initarg :obb-x :reader obb-x)
   (obb-y :initarg :obb-y :reader obb-y)
   ;; not sure if it will be useful beyond debugging, but store
   ;; epsilon and target resolution used to calculate intersection
   ;; regions
   (epsilon :initarg :epsilon :reader epsilon)
   ;; (not sure if RESOLUTION is right name or not, intended to be
   ;; smallest distance we care about, while epsilon is smallest value
   ;; we expect to be able to distinguish. For example we might set
   ;; RESOLUTION to be 1/4 or 1/8 or so if working with values on an
   ;; integer grid, while epsilon would be something closer to 1e-14
   ;; calculated from double-float-epsilon and magnitude of values in
   ;; curve)
   (resolution :initarg :resolution :reader resolution)
   ;; nominal intersection coordinates, probably just middle of one of
   ;; the bounding boxes (or maybe average of middles of curves?)
   (xy :initarg :xy :reader xy)
   ;; same thing, with T values as well
   (xyat :initarg :xyat :reader xyat)))

(defun make-bcs (b &key (t1 0d0) (t2 1d0))
  (etypecase b
    (bezier
     b)
    (b::bezier2
     ;; todo: add option to center bezier around 0,0 in hopes of
     ;; preserving more precision in sub-segments?
     (make-instance 'bezier
                    :original b
                    :current (if (and (= t1 0d0) (= t2 1d0))
                                 b
                                 (b::trim-b2 b t1 t2))
                    :t1 t1 :t2 t2
                    :interval (- t2 t1)
                    :base (b::v2dist (b::b2-dp1 b) (b::b2-dp2 b))
                    :epsilon (b::%bcs-eps b)))))

(defun make-sub-bcs (b t1 t2)
  (let* ((o (original b)))
    (cond
      ((= t1 t2)
       (make-instance 'bezier  :original o
                               :current (current b)
                               :t1 t1
                               :t2 t2
                               :interval (- t2 t1)
                               :base 0d0
                               :epsilon (epsilon b)
                               :curve (curve b)))
      (t
       (let ((c (b::trim-b2 (original b) t1 t2)))
         (make-instance 'bezier :original o
                                :current c
                                :t1 t1
                                :t2 t2
                                :interval (- t2 t1)
                                :base (b::v2dist (b::b2-dp1 c) (b::b2-dp2 c))
                                :epsilon (epsilon b)
                                :curve (curve b)))))))

(defun split/2 (b2 at)
  (let ((t1 (t1 b2))
        (t2 (t2 b2)))
    (assert (< t1 at t2))
    (let* ((original (original b2))
           (current (current b2))
           ;; start at same point as current
           (a (b::b2-dp1 current))
           ;; off-curve control point
           (b (b::%%trim-b2-p1 original t1 at))
           ;; new shared endpoint
           (c (b::eval-at/b2/fast (original b2) at))
           ;; off-curve control point for 2nd curve
           (d (b::%%trim-b2-p1 original at t2))
           ;; and end at same point as current
           (e (b::b2-dp2 current)))
      (values (b::make-bezier2/v2 a b c)
              (b::make-bezier2/v2 c d e)))))

(defclass intersect-pair ()
  ;; pair of BEZIER instances being tested
  ((a :initarg :a :reader a)
   (b :initarg :b :reader b)
   ;; remaining range A and B
   (ar :initform nil :accessor ar)
   (br :initform nil :accessor br)
   ;; number of passes since A,B were last updated, used to decide if
   ;; we need to try split fat lines, or otherwise aren't converging
   (passes :initform 0 :accessor passes)
   ;; we store some flags indicating a pair has run out of resolution
   ;; and iteration should stop (we run at least one more iteration
   ;; after seeing a flat fat line to trim curves to approximately
   ;; same length. not sure yet if that will just be an extra
   ;; perpendicular pass or if running all 3 lines would work better)
   (flat :initform nil :accessor flat)
   (slow :initform nil :accessor slow :initarg :slow)))

(defmethod initialize-instance :after ((o intersect-pair) &key)
  (setf (ar o) (list (list (t1 (a o)) (t2 (a o)))))
  (setf (br o) (list (list (t1 (b o)) (t2 (b o))))))

(defun %check-fl (a fla c t0 t1 epsilon)
  (when d::*check-lines*
    (d::check-line c fla)
    (multiple-value-bind (n1 x1)
        (d::intersect-bez2/fat-line (original a)
                                    fla
                                    t0 t1
                                    epsilon)
      (unless (and (= n1 1)
                   (= (aref x1 0) t0)
                   (= (aref x1 1) t1))
        (break "intersect~% ~s ~s~% was ~s, ~s" n1 x1 t0 t1))))
  fla)

(defun %clip1 (b fl epsilon)
  (multiple-value-bind (n v) (d::intersect-bez2/fat-line
                              (original b) fl
                              (t1 b) (t2 b)
                              epsilon)
    (let ((s (sort (loop for i below n
                         collect (list (aref v (* i 2))
                                       (aref v (1+ (* i 2)))))
                   '< :key 'car)))
      s)))

(defun clip1/2 (a b clip epsilon expand)
  ;; not sure if this should support other clips or not?
  (assert (eql clip :n))
  (let ((t0 (t1 a))
        (t1 (t2 a)))
    (flet ((combine1 (a1 a2)
             (d::%union-regions (sort a1 '< :key 'car)
                                (sort a2 '< :key 'car)
                                (t1 b) (t2 b)))
           (clip (c t0 t1)
             (let ((fl (d::fat-line-from-bez2 c)))
               (values
                (%clip1 b
                        (%check-fl a
                                   (d::expand fl expand)
                                   c
                                   t0 t1
                                   epsilon)
                        epsilon)
                ;; return the fat line, since we use this one for a
                ;; recursion termination test
                fl))))
      (let ((tm (/ (+ t0 t1) 2)))
        (multiple-value-bind (c1 c2) (split/2 a tm)
          (combine1 (clip c1 t0 tm)
                    (clip c2 tm t1)))))))

(defun clip1 (a b clip epsilon expand resolution)
  (let* ((ca (current a))
         (fl1 (ecase clip
                (:n (d::fat-line-from-bez2 ca))
                (:1 (d::fat-line-from-points-b2 (b::b2-dp1 ca)
                                                (b::b2-dc1 ca)
                                                (b::b2-dp2 ca)))
                (:2 (d::fat-line-from-points-b2 (b::b2-dc1 ca)
                                                (b::b2-dp2 ca)
                                                (b::b2-dp1 ca)))))
         (w (- (d::fl-max fl1) (d::fl-min fl1)))
         ;; expand fat line enough to make sure it is at least
         ;; RESOLUTION wide, otherwise by EXPAND in each direction
         (ex2 (if (and resolution (< (+ w (* expand 2)) resolution))
                  (/ (- resolution w) 2)
                  expand))
         (fl (d::expand fl1 ex2)))
    (%check-fl a fl ca (t1 a) (t2 a) epsilon)
    (values (%clip1 b fl epsilon)
            (if (and resolution (< w resolution))
                '(:flat)
                nil))))

(defun intersect* (pairs epsilon expand resolution)
  ;; repeatedly clip elements of PAIRS against each other until either
  ;; they don't overlap or the overlap region is too small.

  ;; PAIRS is a list of lists of 2 INTERSECT-PAIR instances A,B. On
  ;; each iteration we clip each of the 2 beziers against the other
  ;; using one fat line. If nothing remains for either, drop that
  ;; pair. If remaining space for A or B is less than 70% of space
  ;; spanned by CURRENT, replace the pair with new pair(s) formed by
  ;; clipping A,B to remaining range(s). If we have tried all the fat
  ;; lines (normal + 2x edges, so PASSES=3?), try clipping on 1/2s and
  ;; then replace pair with new pair(s) formed by clipping A,B to
  ;; remaining range(s).

  (when (and pairs (typep (caar pairs) 'bezier))
    (setf pairs (loop for (a b) in pairs
                      collect (make-instance 'intersect-pair :a a :b b))))
  (let ((cutoff 0.7d0)
        (slow-cutoff 0.9d0)
        (done nil)
        (clips #(:n :1 :2)))
    (labels ((sumr (r)
               (loop for (a b) in r
                     sum (- b a)))
             (trim1 (b r)
               (loop for (t1 t2) in r
                     collect (make-sub-bcs b t1 t2)))
             (update (pair &key slow)
               (let ((an (trim1 (a pair) (ar pair)))
                     (bn (trim1 (b pair) (br pair))))
                 (loop
                   for a in an
                   append (loop
                            for b in bn
                            collect (make-instance 'intersect-pair
                                                   :a a :b b :slow slow)))))
             (merge1 (r s t1 t2)
               (setf r (d::%merge-regions r (sort s '< :key 'car)
                                          t1 t2))
               r)
             (merges (pair ra rb)
               (setf (ar pair) (merge1 (ar pair) ra
                                       (t1 (a pair)) (t2 (a pair))))
               (setf (br pair) (merge1 (br pair) rb
                                       (t1 (b pair)) (t2 (b pair))))))
      (loop
        for work = pairs then next
        for passes fixnum from 0
        while work
        for next
          = (progn
              (loop
                for pair in work
                for i from 0
                for a = (a pair)
                for b = (b pair)
                for clip = (aref clips (mod passes 3))
                for n
                  = (multiple-value-bind (rb rbf)
                        (clip1 a b clip epsilon expand resolution)
                      (assert (= (interval a) (- (t2 a) (t1 a))))
                      (assert (= (interval b) (- (t2 b) (t1 b))))
                      (incf (passes pair))
                      (when rb
                        (multiple-value-bind (ra raf)
                            (clip1 b a clip epsilon expand resolution)
                          (when ra
                            ;; only check % after clipping both, since we
                            ;; need to clip both to keep the function
                            ;; symmetrical ((intersect a b) should return
                            ;; same results as (intersect b a))
                            (merges pair ra rb)
                            (when (or (member :flat raf)
                                      (member :flat rbf))
                              (setf (flat pair) t))
                            (cond
                              ;; done recursion, one or both curves
                              ;; are too flat to continue and we
                              ;; clipped against 3 lines
                              ((and (flat pair)
                                    (>= (passes pair) 3))
                               (setf done (append (update pair) done))
                               ;; return NIL so it doesn't keep
                               ;; processing this pair
                               nil)
                              ;; almost done recursion, finish set of
                              ;; 3 clips before returning
                              ((flat pair)
                               (list pair))
                              ;; normal recursion, one or both of the
                              ;; pair were trimmed by a reasonable
                              ;; amount so update the pairs. (todo:
                              ;; tune cutoff and/or test to see if it
                              ;; is faster to always run all 3 clips
                              ;; before updating)
                              ((< (+ (sumr (ar pair))
                                     (sumr (br pair)))
                                  (* cutoff (+ (interval a) (interval b))))
                               (update pair))
                              ;; not converging, try clipping against
                              ;; split curves in case we are stuck on a
                              ;; pair of intersections
                              ((>= (passes pair) 3)
                               (multiple-value-bind (rb rbf)
                                   (clip1/2 a b :n epsilon expand)
                                 (declare (ignore rbf))
                                 (when rb
                                   (multiple-value-bind (ra raf)
                                       (clip1/2 b a :n epsilon expand)
                                     (declare (ignore raf))
                                     (when ra
                                       ;; we don't check for :FLAT here,
                                       ;; since we might not have
                                       ;; trimmed curves enough for
                                       ;; normal lines to be flat
                                       (merges pair ra rb)
                                       (let* ((left (+ (sumr (ar pair))
                                                       (sumr (br pair))))
                                              (start (+ (interval a)
                                                        (interval b)))
                                              (slow (>= left
                                                        (* slow-cutoff start)))
                                              (r (update pair
                                                         :slow
                                                         (when slow
                                                           (/ left start)))))
                                         (if (and slow (slow pair))
                                             ;; didn't trim enough
                                             ;; twice, give up on this
                                             ;; pair
                                             (progn
                                               (setf done (append r done))
                                               nil)
                                             ;; still converging OK,
                                             ;; keep working on it
                                             r)))))))
                              ;; otherwise keep pair and try clipping
                              ;; against next fat line
                              (t
                               (list pair)))))))
                when n append n))
        when (> passes 64)
          do (setf next nil)
             (loop-finish))
      (loop for p in done
            collect (list (a p) (b p))
            do (assert (zerop (passes p)))))))


(defun resolve-one-intersection (a b epsilon expand)
  (labels ((mid-at (a)
             (+ (/ (t1 a) 2) (/ (t2 a) 2)))
           (mid (a)
             (b::eval-at/b2/fast (original a) (+ (/ (t1 a) 2) (/ (t2 a) 2))))
           (p4 (p at1 at2)
             (make-array 4 :element-type 'double-float
                           :initial-contents (list (b::vx p) (b::vy p)
                                                   at1 at2)))
           (rp* (p a b at1 at2)
             (return-from resolve-one-intersection
               (make-instance 'intersect-region
                              :xy (b::v2copy p)
                              :xyat (p4 p at1 at2)
                              :a (original a) :b (original b)
                              :at0 (t1 a) :at1 (t2 a)
                              ;; todo: calculate bounding boxes
                              :bt0 (t1 b) :bt1 (t2 b)
                              ;; fixme: pass values for these around
                              :epsilon epsilon
                              :resolution nil)))
           (rp (p at1 at2)
             (rp* p a b at1 at2)))
    ;; if curves share an endpoint within range, just use that
    (let* ((a1 (t1 a))
           (a2 (t2 a))
           (b1 (t1 b))
           (b2 (t2 b))
           (ao1 (o1 a))
           (ao2 (o2 a))
           (bo1 (o1 b))
           (bo2 (o2 b))
           (ap0 (b::eval-at/b2/fast (original a) ao1))
           (ap2 (b::eval-at/b2/fast (original a) ao2))
           (bp0 (b::eval-at/b2/fast (original b) bo1))
           (bp2 (b::eval-at/b2/fast (original b) bo2)))
      (cond
        ((and (= a1 ao1) (= b1 bo1) (b::v2= ap0 bp0))
         (rp ap0 ao1 bo1))
        ((and (= a1 ao1) (= b2 bo2) (b::v2= ap0 bp2))
         (rp ap0 ao1 bo2))
        ((and (= a2 ao2) (= b1 bo1) (b::v2= ap2 bp0))
         (rp ap2 ao2 bo1))
        ((and (= a2 ao2) (= b2 bo2) (b::v2= ap2 bp2))
         (rp ap2 ao2 bo2)))

      ;; try final clip at full resolution to reject extra pairs
      ;; leftover from near tangent clips
      (flet ((maybe-return-endpoint (a b xy ata atb)
               ;; if one curve has an endpoint in range, and it is
               ;; closer to other curve than default point, use the endpoint
               (let* ((oa (original a))
                      (ob (original b))
                      (da (b::dist/v2-bezier2/sf xy oa))
                      (db (b::dist/v2-bezier2/sf xy ob)))
                 (cond
                   ((and (= a1 ao1) (< (b::dist/v2-bezier2/sf ap0 ob) db))
                    (rp* ap0 a b ao1 atb))
                   ((and (= a2 ao2) (< (b::dist/v2-bezier2/sf ap2 ob) db))
                    (rp* ap2 a b ao2 atb))
                   ((and (= b1 bo1) (< (b::dist/v2-bezier2/sf bp0 oa) da))
                    (rp* bp0 a b ata bo1))
                   ((and (= b2 bo2) (< (b::dist/v2-bezier2/sf bp2 oa) da))
                    (rp* bp2 a b ata bo2))))))
        (let* ((c1 (clip1 a b :n epsilon expand nil))
               (c2 (when c1 (clip1 a b :1 epsilon expand nil)))
               (c3 (when c2 (clip1 a b :2 epsilon expand nil)))
               (m1 (when c3 (d::%merge-regions
                             (sort c1 '< :key 'car)
                             (d::%merge-regions
                              (sort c2 '< :key 'car)
                              (sort c3 '< :key 'car)
                              (t1 b) (t2 b))
                             (t1 b) (t2 b))))
               (c4 (when m1 (clip1 b a :n epsilon expand nil)))
               (c5 (when c4 (clip1 b a :1 epsilon expand nil)))
               (c6 (when c5 (clip1 b a :2 epsilon expand nil)))
               (m2 (when c6 (d::%merge-regions
                             (sort c4 '< :key 'car)
                             (d::%merge-regions
                              (sort c5 '< :key 'car)
                              (sort c6 '< :key 'car)
                              (t1 a) (t2 a))
                             (t1 a) (t2 a)))))
          (when m2
            (cond
              ((and (not (cdr m1)) (not (cdr m2)))
               ;; if final clip keeps 1 region from each, reduce region
               ;; to that
               (let* ((a2 (make-sub-bcs a (caar m2) (cadar m2)))
                      (b2 (make-sub-bcs b (caar m1) (cadar m1)))
                      (xy (b::v2lerp (mid a2) (mid b2) 0.5)))
                 (maybe-return-endpoint a2 b2 xy (mid-at a2) (mid-at b2))
                 (make-instance 'intersect-region
                                :xy xy
                                :xyat (p4 xy (mid-at a2) (mid-at b2))
                                :a (original a) :b (original b)
                                :at0 (t1 a2) :at1 (t2 a2)
                                ;; todo: calculate bounding boxes
                                :bt0 (t1 b2) :bt1 (t2 b2)
                                ;; fixme: pass values for these around
                                :epsilon epsilon
                                :resolution nil)))
              (t
               ;; if we got multiple regions, just use full range
               ;; since we are probably nearly tangent
               (let ((xy (b::v2lerp (mid a) (mid b) 0.5)))
                 (maybe-return-endpoint a b xy (mid-at a) (mid-at b))
                 (make-instance 'intersect-region
                                :xy xy
                                :xyat (p4 xy (mid-at a) (mid-at b))
                                :a (original a) :b (original b)
                                :at0 (t1 a) :at1 (t2 a)
                                ;; todo: calculate bounding boxes
                                :bt0 (t1 b) :bt1 (t2 b)
                                ;; fixme: pass values for these around
                                :epsilon epsilon
                                :resolution nil))))))))))

(defun resolve-intersections (intersections epsilon expand a1 b1)
  (when intersections
    (let ((a1 (original a1))
          (b1 (original b1)))
      (loop for (a b . flags) in intersections
            do (unless (eql (original a) a1)
                 (rotatef a b))
               (assert (eql a1 (original a)))
               (assert (eql b1 (original b)))
            when (resolve-one-intersection a b epsilon expand)
              collect it))))

(defun intersect (a b)
  (let* ((epsilon (max (epsilon a) (epsilon b)))
         ;; we always expand fat lines by a small multiple of epsilon
         ;; to avoid rounding errors (todo: tune this and/or pick a
         ;; better epsilon so it can use a multiple of 1)
         (expand (* 16 epsilon))
         ;; we also set a minimum size for fat lines, which limits
         ;; distance between near-tangent curves which can be
         ;; distinguished. Should be at least 2x expand (probably
         ;; more?) since we do a final pass with 1/2 resolution to
         ;; remove some extra pairs that are common near tangents with
         ;; default clipping method)
         (resolution (* 1024 epsilon)))
    (resolve-intersections (intersect* (list (list a b))
                                       epsilon expand resolution)
                           epsilon expand a b)))
