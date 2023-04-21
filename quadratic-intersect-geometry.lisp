(in-package #:sdf/quadratic-intersect/int)

;; distance field of a line, stored as Ax + By + C in #(a b c).
(deftype linedf () '(simple-array double-float (3)))

(declaim (inline %linedf linedf linedf* linedf/pp linedf/pp* a b c))
(defun %linedf (a b c)
  (make-array 3 :element-type 'double-float
                :initial-contents (list (f a) (f b) (f c))))

(declaim (inline v2lerp))
(defun v2lerp (a b f)
  ;; fixme: v::v2lerp tries to fix some stability problems, but
  ;; intersection code depends on this version. look up 'correct'
  ;; version that checks values and gets both cases correct and see if
  ;; it works here
  (b::v2+ (b::v2scale a (- 1d0 f))
          (b::v2scale b f)))

(defun a (p) (aref p 0))
(defun b (p) (aref p 1))
(defun c (p) (aref p 2))

(defun linedf (x1 y1 x2 y2)
  ;; line of form ax+by+c=0, a²+b²=1 through x1,y1 and x2,y2
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (l (sqrt (f (+ (expt dx 2) (expt dy 2)))))
         (a (/ dy l))
         (b (- (/ dx l)))
         (c (- (+ (* a x1) (* b y1)))))
    (%linedf a b c)))

(defun linedf* (x1 y1 dx dy)
  ;; line of form ax+by+c=0, a²+b²=1 through x1,y1 in direction dx,dy
  (let* ((l (sqrt (f (+ (expt dx 2) (expt dy 2)))))
         (a (/ dy l))
         (b (- (/ dx l)))
         (c (- (+ (* a x1) (* b y1)))))
    (%linedf a b c)))

(defun linedf/pp (p1 p2)
  (let ((x1 (vx p1))
        (y1 (vy p1))
        (x2 (vx p2))
        (y2 (vy p2)))
    (linedf x1 y1 x2 y2)))


(defun linedf/pp* (p1 d)
  (let ((x1 (vx p1))
        (y1 (vy p1))
        (dx (vx d))
        (dy (vy d)))
    (linedf* x1 y1 dx dy)))

(defun linedf/perpendicular-through-point (l p)
  (let ((x1 (vx p))
        (y1 (vy p))
        (dx (a l))
        (dy (b l)))
    (linedf* x1 y1 dx dy)))

(declaim (inline make-fat-line make-fat-line/p make-fat-line/l
                 %make-fat-line fl-line fl-min fl-max))
(defstruct (fat-line (:conc-name fl-)
                     (:constructor %make-fat-line))
  (line (linedf 0 0 0 1) :type linedf)
  (min 0d0 :type double-float)
  (max 0d0 :type double-float))

(defun make-fat-line (x1 y1 x2 y2 min max)
  (with-f (x1 y1 x2 y2 min max)
    (assert (<= min max))
    (%make-fat-line :line (linedf x1 y1 x2 y2) :min min :max max)))

(defun make-fat-line/p (p1 p2 min max)
  (declare (type b::v2 p1 p2))
  (with-f (min max)
    (assert (<= min max))
    (%make-fat-line :line (linedf/pp p1 p2) :min min :max max)))

(defun make-fat-line/l (l min max)
  (declare (type linedf l))
  (with-f (min max)
    (assert (<= min max))
    (%make-fat-line :line l :min min :max max)))

(declaim (inline linedf@xy linedf@p
                 %eval-b2 eval-b2
                 %eval-b2-tan eval-b2-tan))
(defun linedf@xy (l x y)
  (declare (type linedf l))
  (with-f (x y)
    (+ (* (a l) x)
       (* (b l) y)
       (c l))))

(defun linedf@p (l p)
  (declare (type linedf l) (type b::v2 p))
  (+ (* (a l) (vx p))
     (* (b l) (vy p))
     (c l)))

(defun intersect-bez2/linedf (b2 l ofs)
  (declare (optimize speed)
           (type b::bezier2 b2)
           (type linedf l)
           (type double-float ofs))
  ;; distance from L to B2 = (1-t)²d0 + 2t(1-t)d1 + t²d2
  (let* ((d0 (linedf@p l (b::b2-dp1 b2)))
         (d1 (linedf@p l (b::b2-dc1 b2)))
         (d2 (linedf@p l (b::b2-dp2 b2)))
         ;; = at² + bt + c
         (a (+ d0 d2 (* -2 d1)))
         (b (* 2 (- d1 d0)))
         (c (- d0 ofs)))
    (multiple-value-bind (r1 r2) (b::solve-quadratic a b c)
      (declare (type (or null double-float) r1 r2))
      (labels ((rets (x)
                 ;; derivative of distance = 2at + b, 2nd deriv = 2a
                 (let ((2a (* 2 a))
                       (dv (make-array (* 2 (length x))
                                       :element-type 'double-float)))
                   (loop for i below (length x)
                         for at double-float across x
                         do (setf (aref dv (* i 2)) (+ (* 2a at) b))
                            (setf (aref dv (1+ (* i 2))) 2a))
                   (values x dv))))
        (cond
          (r2
           (rets (make-array 2 :element-type 'double-float
                               :initial-contents (list r1 r2))))
          (r1
           (rets (make-array 1 :element-type 'double-float
                               :initial-contents (list r1))))
          (t
           nil))))))

#++(setf *dump* nil)

(defun intersect-bez2/fat-line (b2 fl t0 t1 &optional (eps (b::%bcs-eps b2)))
  (declare (type b::bezier2 b2)
           (type fat-line fl)
           (type double-float t0 t1)
           (optimize speed))
  (assert (< t0 t1))
  (let ((eps (coerce eps 'double-float)))
    (multiple-value-bind (mins mind)
        (intersect-bez2/linedf b2 (fl-line fl) (fl-min fl))
      (declare (type (or null (simple-array double-float (*))) mins mind))
      (multiple-value-bind (maxs maxd)
          (intersect-bez2/linedf b2 (fl-line fl) (fl-max fl))
        (declare (type (or null (simple-array double-float (*))) maxs maxs))
        (let* ((nmin (length mins))
               (nmax (length maxs)))
          (cond
            ((not (= 0 nmin nmax))
             (let* ((d0 t0)
                    ;; cubic can have up to 3 segments remaining, with 2
                    ;; endpoints each = 6 elements max
                    (ret (make-array 6 :element-type 'double-float
                                       :initial-element 0d0))
                    (n 0)  ;; # of overlapping segments found so far
                    (n1 0) ;; index into mins
                    (n2 0) ;; index into maxs
                    (state :start)
                                        ;(last-state nil)
                    (at t0))            ;; last crossing seen
               (declare (type fixnum n n1 n2))
               (when *dump*
                 (format t "~&start ~s @ 0 = ~s~%" d0 state)
                 (format t " t: ~s - ~s~%" t0 t1))
               (labels ((add (a b)
                          (cond
                            ;; if start of this segment is same as end of
                            ;; last segment, merge them
                            ((and (plusp n)
                                  (= a (aref ret (- (* n 2) 1))))
                             (when *dump* (format t "@@@1 n=~s, add ~s ~s~%" n a b))
                             (setf (aref ret (- (* n 2) 1)) (min t1 b)))
                            ((and (< a t1)
                                  (> b t0))
                             (when *dump* (format t "@@@2 n=~s, add ~s ~s~%" n a b))
                             (setf (aref ret (* n 2)) (max a t0))
                             (setf (aref ret (1+ (* n 2))) (min t1 b))
                             (incf n))
                            (t
                             (when *dump* (format t "@XX n=~s,~s, skip ~s ~s~%" n1 n2 a b))))
                          (when *dump* (format t "  @@->~s~%" (subseq ret 0 (* n 2)))))
                        (next ()
                          (cond
                            ((= n1 nmin)
                             (when *dump* (format t "  next: n1=nmin~%"))
                             (when (< n2 nmax)
                               (let ((i n2))
                                 (incf n2)
                                 (values (aref maxs i)
                                         :max
                                         (aref maxd (* i 2))
                                         (aref maxd (1+ (* i 2)))))))
                            ((= n2 nmax)
                             (when *dump* (format t "  next: n2=nmax~%"))
                             (when (< n1 nmin)
                               (let ((i n1))
                                 (incf n1)
                                 (values (aref mins i)
                                         :min
                                         (aref mind (* i 2))
                                         (aref mind (1+ (* i 1)))))))
                            ((= (aref mins n1)
                                (aref maxs n2))
                             (when *dump*
                               (format t " next=? ~s ~s~%"
                                       (aref mind (* n1 2))
                                       (aref maxd (* n2 2))))
                             (cond
                               ((plusp (aref mind (* n1 2)))
                                (let ((i n1))
                                  (incf n1)
                                  (values (aref mins i)
                                          :min
                                          (aref mind (* i 2))
                                          (aref mind (1+ (* i 1))))))
                               ((minusp (aref maxd (* n2 2)))
                                (let ((i n2))
                                  (incf n2)
                                  (values (aref maxs i)
                                          :max
                                          (aref maxd (* i 2))
                                          (aref maxd (1+ (* i 2))))))
                               (t (return-from next nil))))
                            (t
                             (cond
                               ((< (aref mins n1)
                                   (aref maxs n2))

                                (let ((i n1))
                                  (incf n1)
                                  (values (aref mins i)
                                          :min
                                          (aref mind (* i 2))
                                          (aref mind (1+ (* i 1))))))
                               (t
                                (let ((i n2))
                                  (incf n2)
                                  (values (aref maxs i)
                                          :max
                                          (aref maxd (* i 2))
                                          (aref maxd (1+ (* i 2)))))))))))
                 (tagbody
                  :loop
                    (when *dump*
                      (format t " mins ~s~% maxs ~s~%"
                              (subseq mins n1 nmin)
                              (subseq maxs n2 nmax)))
                    (when (>= at t1)
                      (when *dump* (format t "   (>= ~s ~s), done~%" at t1))
                      (go :done))
                    (multiple-value-bind (nt ns nd1 nd2) (next)
                      (when *dump* (format t " ~s -> ~s ~s~%" state nt ns))
                      (cond
                        ((not nt)
                         (when *dump* (format t "   not nt, done~%"))
                         (go :done))
                        ;; if we aren't within 0..1 range, we don't need to
                        ;; add a segment, so just store this crossing for latter
                        ((< nt t0)
                         (when *dump* (format t "   xx early~%"))
                         (setf at nt)
                         (setf state ns))
                        ;; easy case, if curve touches one
                        ;; side of fat line then touches other side, it must
                        ;; have crossed it so add a segment
                        ((or (and (eql state :min) (eql ns :max))
                             (and (eql state :max) (eql ns :min)))
                         (when *dump* (format t "   !! crosses~%"))
                         (add at nt)
                         (setf at nt)
                         (setf state ns))
                        ;; otherwise (first crossing, or we crossed one side
                        ;; twice in a row) we need to figure out which side
                        ;; of the line we are crossing from to determine
                        ;; whether to add preceding segment
                        (t
                         (when *dump*
                           (format t "   ?? ~s-~s: ~s ~s~%" state ns nd1 nd2))
                         (let ((add nil))
                           (when (= nd1 nd2 0)
                             ;; todo: figure out what is going on when
                             ;; both derivatives are 0
                             (ebreak "todo"))
                           (when (eql ns :??)
                             (when (eql state :??)
                               ;; previous intersection was ambiguous
                               ;; too, must have passed valid range
                               ;; completely so exit (consecutive :??
                               ;; before 0 would have been skipped by
                               ;; other clauses, and we should only see
                               ;; 1 state after 0)
                               (assert (and (< at t0) (> nt t1)))
                               (go :done))
                             ;; new intersection is ambiguous, calculate
                             ;; position at 1 and use that to decide how
                             ;; to handle final segment
                             (let ((p (linedf@p (fl-line fl)
                                                (b::eval-at/b2/fast b2 t1))))
                               (assert (> nt t1))
                               (when (<= (fl-min fl) p (fl-max fl))
                                 (add at t1))
                               (go :done)))
                           (ecase ns
                             (:min
                              ;; when first derivative is negative at
                              ;; MIN, we are leaving the line
                              (when (or (minusp nd1)
                                        ;; if it is 0 and 2nd deriv is
                                        ;; positive, we are tangent to
                                        ;; MIN from inside
                                        (and (zerop nd1)
                                             (plusp nd2)))
                                (when *dump* (format t "     -> leave/min~%"))
                                (setf add t)))
                             (:max
                              ;; if derivative is positive at MAX, we
                              ;; are leaving the line
                              (when (or (plusp nd1)
                                        ;; if it is 0 and 2nd deriv is
                                        ;; negative, we are tangent to
                                        ;; MIN from inside
                                        (and (zerop nd1)
                                             (minusp nd2)))
                                (when *dump* (format t "     -> leave/max~%"))
                                (setf add t))))
                           (when add
                             (when *dump* (format t "       arr ~s ~s~%" at nt))
                             (add at nt))
                           (setf at nt)
                           (setf state ns)))))
                    (go :loop)
                  :done)

                 (when *dump* (format t " -> ~s~%" (subseq ret 0 (* n 2))))
                 ;; push the values apart a bit to reduce chance of
                 ;; numerical error pushing the actual endpoints inside
                 ;; the fat line. Without this, tend to get errors from
                 ;; intersection code when curves have almost the same
                 ;; endpoint. fixme: see if those errors can be fixed
                 ;; directly, and remove or reduce this?
                 ;;
                 (loop with e double-float = eps
                       for i below n
                       for a = (aref ret (* i 2))
                       for b = (aref ret (1+ (* i 2)))
                       do (setf (aref ret (* i 2))
                                ;; fixme: is (< b a) valid here?
                                (max t0 (if (<= a b)
                                            (- a e)
                                            (+ a e))))
                          (setf (aref ret (1+ (* i 2)))
                                (min t1
                                     (if (<= a b)
                                         (+ b e)
                                         (- b e)))))
                 (when *dump*
                   (format t " -> ~s~%" (subseq ret 0 (* n 2))))
                 (values n ret))))
            (t
             (let* ((d0 (linedf@p (fl-line fl) (b::eval-at/b2/fast b2 t0)))
                    (tmid (/ (+ t0 t1) 2))
                    (d0.5 (linedf@p (fl-line fl) (b::eval-at/b2/fast b2 tmid)))
                    (d1 (linedf@p (fl-line fl) (b::eval-at/b2/fast b2 t1))))
               (flet ((in (x) (and (<= (fl-min fl) x (fl-max fl)))))
                 (assert (and (eql (in d0) (in d1))
                              (eql (in d0) (in d0.5))))
                 (if (in d0) ;; entirely within fat line, return whole thing
                     (values 1 (vector t0 t1))
                     (values 0 nil)))))))))))

(defparameter *check-lines* t)
#++
(setf *check-lines* nil)
(defun check-line (b fl)
  (when *check-lines*
    (let* ((dl (fl-line fl))
           (d0 (linedf@p dl (b::b2-dp1 b)))
           (d1 (/ (linedf@p dl (b::b2-dc1 b)) 2))
           (d2 (linedf@p dl (b::b2-dp2 b)))
           (min (fl-min fl))
           (max (fl-max fl)))
      (unless (and (<= min d0 max)
                   (<= min d1 max)
                   (<= min d2 max))
        (ebreak "fat line doesn't contain curve ~s~%~s~%~s ~S~%~S ~s~%~S ~s"
               fl b
               d0 (<= min d0 max)
               d1 (<= min d1 max)
               d2 (<= min d2 max)))))
  fl)

(defun check-line2* (p0 p1 p2 fl)
  (when *check-lines*
    (let* ((dl (fl-line fl))
           (min (fl-min fl))
           (max (fl-max fl))
           (d0 (linedf@p dl p0))
           (d1 (/ (linedf@p dl p1) 2))
           (d2 (linedf@p dl p2)))
      (unless (and (<= min d0 max)
                   (<= min d1 max)
                   (<= min d2 max))
        (ebreak "fat line doesn't contain curve ~s~%~s~%~s ~S~%~S ~s~%~S ~s"
               fl (list p0 p1 p2)
               d0 (<= min d0 max)
               d1 (<= min d1 max)
               d2 (<= min d2 max)))))
  fl)

(defun expand (fl expand)
  (decf (fl-min fl) expand)
  (incf (fl-max fl) expand)
  fl)

(defun fat-line-from-bez2 (b)
  (let* ((dl (linedf/pp (b::b2-dp1 b) (b::b2-dp2 b)))
         (d1/2 (/ (linedf@p dl (b::b2-dc1 b)) 2))
         (r (make-fat-line/l dl (min 0 d1/2) (max 0 d1/2))))
    r))

(defun perpendicular-fat-line-from-bez2 (b)
  (let* ((dl (linedf/perpendicular-through-point
              (linedf/pp (b::b2-dp1 b) (b::b2-dp2 b)) (b2-pn/2 b)))
         (d0 (linedf@p dl (b::b2-dp1 b)))
         (d1 (linedf@p dl (b::b2-dc1 b)))
         (d2 (linedf@p dl (b::b2-dp2 b))))
    (check-line b
                (make-fat-line/l dl (min 0 d0 d1 d2) (max 0 d0 d1 d2)))))


(defun fat-line-from-bez2* (p0 p1 p2)
  (let* ((dl (linedf/pp p0 p2))
         (d1/2 (/ (linedf@p dl p1) 2)))
    (check-line2* p0 p1 p2
                  (make-fat-line/l dl (min 0 d1/2) (max 0 d1/2)))))

(defun fat-line-from-points (p0 p1 &rest points)
  (declare (type b::v2 p0 p1))
  ;; fl to clip to one of the other lines of convex hull, makes line
  ;; through p0,p1 bounded by remaining points
  (let* ((dl (linedf/pp p0 p1))
         (min 0d0)
         (max 0d0))
    (loop for p of-type b::v2 in points
          for d = (linedf@p dl p)
          do (setf min (min min d)
                   max (max max d)))
    (make-fat-line/l dl min max)))

(defun fat-line-from-points-b2 (p0 p1 p2)
  (declare (type b::v2 p0 p1 p2))
  ;; fl to clip to one of the other lines of convex hull, makes line
  ;; through p0,p1 bounded by remaining point
  (let* ((dl (linedf/pp p0 p1))
         (d (linedf@p dl p2))
         (r (make-fat-line/l dl (min 0 d) (max 0 d))))
    r))


(defun %clip-bez-to-rect (b x1 y1 x2 y2 t0 t1)
  (let ((l1 (make-fat-line 0 0 0 1 x1 x2))
        (l2 (make-fat-line 0 0 -1 0 y1 y2))
        (i1 nil)
        (i2 nil))
    (multiple-value-bind (n v) (intersect-bez2/fat-line b l1 (f t0) (f t1))
      (setf i1 (sort (loop for i below n
                           collect (list (aref v (* i 2))
                                         (aref v (1+ (* i 2)))))
                     '< :key 'car)))
    (multiple-value-bind (n v) (intersect-bez2/fat-line b l2 (f t0) (f t1))
      (setf i2 (sort (loop for i below n
                           collect (list (aref v (* i 2))
                                         (aref v (1+ (* i 2)))))
                     '< :key 'car)))
    (%merge-regions i1 i2 t0 t1)))

(deftype general-parabola ()
  ;; ax²+bxy+cy²+dx+ey+f
  `(simple-array double-float (6)))




(defun b2-perpendicular-at (b2 line)
  (let ((a1 (a line))
        (b1 (b line))
        (p0 (b::b2-dp1 b2))
        (p1 (b::b2-dc1 b2))
        (p2 (b::b2-dp2 b2)))
    (when (zerop a1)
      ;; if LINE is horizontal, find point on curve where tangent is
      ;; vertical.

      ;; (= (+ (* p1 1-at) (* p2 at)) (+ (* p0 1-at) (* p1 at)))
      ;; (+ p1 -p1×at p2×at)  =  (+ p0 -p0×at p1×at)

      ;; p1-p0 = -p0×at + p1×at + p1×at -p2×at
      ;; p1-p0 = (-p0 + p1 + p1 -p2)×at
      ;; (/ p1-p0 (-p0 p1 p1 -p2)) = at
      ;; (/ (- p1 p0) (- 2p1 (+ p0 p2))) = at


      ;; p1-p0 = -p1×at p1×at + p1×at -p2×at
      ;; p1-p0 = (-p1 p1 + p1 -p2)×at
      ;; (/ p1-p0 (-p1 p1 p1 -p2)) = at
      ;; (/ (- p1 p0) (- p1 p2)) = at
      (let* ((x0 (vx p0))
             (x1 (vx p1))
             (x2 (vx p2))
             (d (- (* 2 x1) (+ x0 x2))))
        (if (zerop d)
            (return-from b2-perpendicular-at nil)
            (return-from b2-perpendicular-at (/ (- x1 x0) d)))))
    (let ((x0 (vx p0))
          (y0 (vy p0))
          (x1 (vx p1))
          (y1 (vy p1))
          (x2 (vx p2))
          (y2 (vy p2))
          (s (- (/ b1 a1))))
      ;; otherwise, point where dy1-dy2/dx2-dx1 = s

      ;; dx1 = (+ (* x0 1-at) (* x1 at))
      ;; dy1 = (+ (* y0 1-at) (* y1 at))
      ;; dx2 = (+ (* x1 1-at) (* x2 at))
      ;; dy2 = (+ (* y1 1-at) (* y2 at))
      ;; s = (/ (- (+ (* y0 1-at) (* y1 at)) (+ (* y1 1-at) (* y2 at)))
      ;;        (- (+ (* x1 1-at) (* x2 at)) (+ (* x0 1-at) (* x1 at))))
      ;;   = (/ (+ (* y0 1-at) (* y1 2at-1) (* y2 -at))
      ;;        (+ (* x1 1-2at) (* x2 at) (* x0 1-at -1)))
      ;; s(+ (* x1 1-2at) (* x2 at) (* x0 1-at -1)) =  (+ (* y0 1-at) (* y1 2at-1) (* y2 -at))
      ;; s×x1 - s×x0 + at×s(-2x1 + x2 + x0) = y0 - y1 + at(- y0 + 2y1 - y2)
      ;; at (s(-2x1 + x2 + x0) - (- y0 + 2y1 - y2)) = y0 - y1 - s×x1 + s×x0
      ;; at = y0 - y1 - s×x1 + s×x0 / (s(-2x1 + x2 + x0) - (- y0 + 2y1 - y2))
      (let ((n (+ y0 (- y1) (* s (- x0 x1))))
            (d (+ (* s (+ x0 x2 (* -2 x1)))
                  y0 y2 (* -2 y1))))
        (when (zerop d)
          (ebreak "missed?"))
        (if (zerop d)
            nil
            (/ n d))))))


(defun parallel-through-point (line point)
  (let ((a (a line))
        (b (b line))
        (c (c line))
        (x (vx point))
        (y (vy point)))
    (declare (ignorable c))
    (vector a b (- (+ (* a x) (* b y))))))

(defun perpendicular-through-point (line point)
  (let ((a (a line))
        (b (b line))
        (c (c line))
        (x (vx point))
        (y (vy point)))
    (declare (ignorable c))
    (make-array 3 :element-type 'double-float
                  :initial-contents (list (- b)
                                          a
                                          (- (+ (* (- b) x) (* a y)))))))

(defun b2-tangent-at (b at)
  (linedf/pp (v2lerp (b::b2-dp1 b) (b::b2-dc1 b) at)
             (v2lerp (b::b2-dc1 b) (b::b2-dp2 b) at)))


(defun intersect-lines (l1 l2 &key (offset1 0) (offset2 0))
  (let* ((a1 (a l1))
         (b1 (b l1))
         (c1 (- (c l1) offset1))
         (a2 (a l2))
         (b2 (b l2))
         (c2 (- (c l2) offset2))
         (c (- (* a1 b2) (* a2 b1))))
    (if (zerop c)
        nil
        (b::v2 (/ (- (* b1 c2) (* b2 c1)) c)
               (/ (- (* a2 c1) (* a1 c2)) c)))))

;; todo: calculate this more directly from points/tangents
(defun curve-from-b2 (b)
  (let* ((parallel-to-axis
           ;; line through middle control point and midpoint of
           ;; endpoints is parallel to axis
           (linedf/pp (b::b2-dc1 b)
                      (v2lerp (b::b2-dp1 b) (b::b2-dp2 b) 0.5)))
         ;; vertex is point on curve where tangent is perpendicular to
         ;; axis
         (vertex-at (b2-perpendicular-at b parallel-to-axis))
         (vertex (eval-b2 b vertex-at))
         (axis (parallel-through-point parallel-to-axis vertex))
         ;; intersection of perpendicular tangents are on directrix
         (dpoint (let* ((at (if (< (abs vertex-at) (abs (- 1 vertex-at)))
                                ;; use endpoint of bezier further from vertex
                                1 0))
                        (tan (b2-tangent-at b at))
                        (p-at (b2-perpendicular-at b tan)))
                   (unless p-at
                     ;; try other endpoint
                     (setf at (- 1 at)
                           tan (b2-tangent-at b at)
                           p-at (b2-perpendicular-at b tan))
                     (unless p-at (ebreak "couldn't find directrix?")))
                   (intersect-lines tan (b2-tangent-at b p-at))))
         (directrix (perpendicular-through-point axis dpoint))
         ;; point on directrix closest to vertex
         (dv (intersect-lines axis directrix))
         (focus (b::v2+ vertex (b::v2- vertex dv)))
         (a (a directrix))
         (a2 (expt a 2))
         (b (b directrix))
         (b2 (expt b 2))
         (a2+b2 (+ a2 b2))
         (c (c directrix))
         (d (vx focus))
         (e (vy focus))
         (parabola (vector (- b2)                        ;; * x²
                           (* 2 a b)                     ;; * xy
                           (- a2)                        ;; * y²
                           (* 2 (+ (* a c) (* d a2+b2))) ;; * x
                           (* 2 (+ (* b c) (* e a2+b2))) ;; * y
                           (- (expt c 2) (* a2+b2 (+ (expt d 2) (expt e 2)))))))
    parabola))

(defun eval-parabola-at-point (parabola point)
  (let ((a (aref parabola 0))
        (b (aref parabola 1))
        (c (aref parabola 2))
        (d (aref parabola 3))
        (e (aref parabola 4))
        (f (aref parabola 5))
        (x (vx point))
        (y (vy point)))
    (+ (* a x x) (* b x y) (* c y y) (* d x) (* e y) f)))

(defun vertex-and-directrix (b)
  ;; return as 3 values : vertex, directrix, and (focal-distance/2)² for
  ;; quadratic bezier B (
  (let* ((parallel-to-axis
           ;; line through middle control point and midpoint of
           ;; endpoints is parallel to axis
           (linedf/pp (b::b2-dc1 b)
                      (v2lerp (b::b2-dp1 b) (b::b2-dp2 b) 0.5)))
         ;; vertex is point on curve where tangent is perpendicular to
         ;; axis
         (vertex-at (b2-perpendicular-at b parallel-to-axis))
         (vertex (b::eval-at/b2/fast b vertex-at))
         (axis (parallel-through-point parallel-to-axis vertex))
         ;; intersection of perpendicular tangents are on directrix
         (dpoint (let* ((at (if (< (abs vertex-at) (abs (- 1 vertex-at)))
                                ;; use endpoint of bezier further from vertex
                                1 0))
                        (tan (b2-tangent-at b at))
                        (p-at (b2-perpendicular-at b tan)))
                   (unless p-at
                     ;; try other endpoint
                     (setf at (- 1 at)
                           tan (b2-tangent-at b at)
                           p-at (b2-perpendicular-at b tan))
                     (unless p-at (ebreak "couldn't find directrix?")))
                   (intersect-lines tan (b2-tangent-at b p-at))))
         (directrix (perpendicular-through-point axis dpoint))
         ;; point on directrix closest to vertex
         (dv (intersect-lines axis directrix)))
    (values vertex directrix (b::v2. dv dv))))

(defun same-parabola-p (b1 b2 eps)
  ;; return true if quadratic bezier curves B1 and B2 are on
  ;; (approximately) the same parabola
  (flet ((~= (a b &optional (eps eps))
           (< (abs (- a b)) eps)))
    (multiple-value-bind (v1 d1 l1) (vertex-and-directrix b1)
      (multiple-value-bind (v2 d2 l2) (vertex-and-directrix b2)
        ;; l is squared, so can be fairly large and noisy, use a different
        ;; epsilon for it
        (let ((e2 (max eps (* double-float-epsilon l1))))
          ;; todo: tune epsilon multipliers a bit more, and decide if
          ;; it should actually try to accept near matches or
          ;; not. Currently trying to accept results from splitting a
          ;; bezier curve as double floats, while rejecting
          ;; differences on the order of single-float noise.
          (and (~= l1 l2 (* 2048 e2))
               (~= (b::vx v1) (b::vx v2) (* 512 eps))
               (~= (b::vy v1) (b::vy v2) (* 512 eps))
               (~= (a d1) (a d2) (* 64 eps))
               (~= (b d1) (b d2) (* 64 eps))
               ;; C coefficient of directrix is also noisy. so use
               ;; coarser epsilon
               (~= (c d1) (c d2) (* 64 e2))))))))
