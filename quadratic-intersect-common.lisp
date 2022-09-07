(in-package #:sdf/quadratic-intersect/int)

(defvar *dump* nil)

(declaim (inline f))
(defun f (x) (coerce x 'double-float))
(defmacro with-f ((&rest vars) &body body)
  `(let (,@(loop for v in vars collect (list v `(f ,v))))
     (declare (type double-float ,@vars))
     ,@body))

(declaim (inline b2))
(defun b2 (x1 y1 xc yc x2 y2)
  (b::%make-bezier2 (b::make-point x1 y1)
                    (b::make-point xc yc)
                    (b::make-point x2 y2)))


(declaim (inline b2-pn/2))
(defun b2-pn/2 (b)
  ;; midpoint between endpoints (just average of endpoints, not t=0.5 or
  ;; whatever on curve)
  (b::v2lerp (b::b2-dp1 b) (b::b2-dp2 b) 0.5))
;; evaluators
(defun %merge-regions (i1 i2 t0 t1)
  ;; return intersections of ranges of i1,i2 (as (list (start end)
  ;; (start2 end2)...)), clipped to t0,t1. I1,I2 should be sorted by
  ;; start, and END of each range should be > START
  (loop with r = nil
        for (a1 b1) = (car i1)
        for (a2 b2) = (car i2)
        while (and i1 i2)
        when (< b1 a1)
          do (break "a backwards?")
        when (< b2 a2)
          do (break "a backwards?")
        do (cond
             ;; a1-b1 is before a2, no overlap
             ((> a2 b1)
              (pop i1))
             ;; a2-b2 is before a1, no overlap
             ((> a1 b2)
              (pop i2))
             ;; a1-b1 ends before a2-b2, add a segment
             ((<= b1 b2)
              (unless (or (< b1 t0)
                          (> (max a1 a2) t1))
                (push (list (max a1 a2 t0) (min b1 t1)) r))
              (pop i1))
             ;; a2-b2 ends before a1-b1, add a segment
             ((<= b2 b1)
              (unless (or (< b2 t0)
                          (> (max a1 a2) t1))
                (push (list (max a1 a2 t0) (min b2 t1)) r))
              (pop i2))
             (t (break "what is this?")))
        finally (return (if (cdr r) (sort r '< :key 'car) r))))

(defun %union-regions (i1 i2 t0 t1)
  ;; return union of ranges of i1,i2 (as (list (start end) (start2
  ;; end2)...)), clipped to t0,t1. Merge overlapping/touching
  ;; ranges. (todo: decide if we need to merge overlap/touch from a
  ;; single input list or not? currently assuming that won't happen,
  ;; so only worrying about overlaps between segments from different
  ;; input lists)
  (loop with r = nil
        with start = nil
        for (a1 b1) = (car i1)
        for (a2 b2) = (car i2)
        while (or i1 i2)
        when (and i1 (< b1 a1))
          do (break "a backwards?")
        when (and i2 (< b2 a2))
          do (break "a backwards?")
        do (cond
             ;; a1-b1 is before a2 or i2 empty =  no overlap, keep segment
             ((and a1 (or (not a2) (< b1 a2)))
              (unless (or (< b1 t0) (> a1 t1))
                (push (list (max t0 (or start a1)) (min t1 b1)) r))
              (setf start nil)
              (pop i1))
             ;; a2-b2 is before a1 or i1 empty =  no overlap, keep segment
             ((and a2 (or (not a1) (< b2 a1)))
              (unless (or (< b2 t0) (> a2 t1))
                (push (list (max t0 (or start a2)) (min t1 b2)) r))
              (setf start nil)
              (pop i2))
             ;; a1-b1 ends before a2-b2, start or extend current segment
             ((<= b1 b2)
              (when start (assert (<= start a1)))
              (unless start
                (setf start (min a1 a2)))
              (pop i1))
             ;; a2-b2 ends before a1-b1, start or extend current segment
             ((<= b2 b1)
              (when start (assert (<= start a2)))
              (unless start
                (setf start (min a1 a2)))
              (pop i2))
             (t (break "what is this?")))
        finally (return (nreverse r))))
