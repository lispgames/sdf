(in-package #:sdf)


;;;
;;;  MAXRECT packing as defined in http://clb.demon.fi/files/RectangleBinPack.pdf
;;;  See also: https://github.com/juj/RectangleBinPack
;;;

(defmacro with-rect ((x y &optional (w (gensym)) (h (gensym))) rect &body body)
  `(destructuring-bind (,x ,y ,w ,h) ,rect
     (declare (ignorable ,x ,y ,w ,h))
     ,@body))

(defun delta-weight (w h rect)
  (with-rect (x y rw rh) rect
    (min (- rw w) (- rh h))))

(defun find-free-rect (w h rects)
  (loop with min-rect = (car rects)
     with min-d = (delta-weight w h min-rect)
     for rect in (rest rects)
     for cur-d = (delta-weight w h rect)
     ;; add case for when w and h of free rect exactly matches required w h
     when (or (< min-d 0) (and (>= cur-d 0) (< cur-d min-d)))
     do (setf min-rect rect
              min-d cur-d)
     finally (return (if (< min-d 0)
                         (error "Cannot pack any more rectangles")
                         min-rect))))

(defun intersectsp (r0 r1)
  (with-rect (x0 y0 w0 h0) r0
    (with-rect (x1 y1 w1 h1) r1
      (and (< x0 (+ x1 w1))
           (> (+ x0 w0) x1)
           (< y0 (+ y1 h1))
           (> (+ y0 h0) y1)))))


(defun splitsp (coord coord-from coord-to)
  (> coord-to coord coord-from))

(defun subdivide-rect (rect placed)
  (if (intersectsp placed rect)
      (with-rect (x y w h) rect
        (with-rect (xp yp wp hp) placed
          (let (result)
            ;; left part
            (when (splitsp xp x (+ x w))
              (push (list x y (- xp x) h) result))
            ;; right part
            (when (splitsp (+ xp wp) x (+ x w))
              (push (list (+ xp wp) y (- (+ x w) (+ xp wp)) h) result))
            ;; bottom
            (when (splitsp yp y (+ y h))
              (push (list x y w (- yp y)) result))
            ;; top
            (when (splitsp (+ yp hp) y (+ y h))
              (push (list x (+ yp hp) w (- (+ y h) (+ yp hp))) result))
            result)))
      (list rect)))

(defun subdivide-intersecting (rect free-rects)
  (loop for free-rect in free-rects appending (subdivide-rect free-rect rect)))

(defun containsp (outer inner)
  (with-rect (x0 y0 w0 h0) outer
    (with-rect (x1 y1 w1 h1) inner
      (and (>= (+ x0 w0) (+ x1 w1) x1 x0)
           (>= (+ y0 h0) (+ y1 h1) y1 y0)))))

(defun normalize-free-space (rects)
  (loop with rest-filtered = rects
     for (rect . rest) = rest-filtered until (null rect)
     collecting
       (loop with contained-p = nil
          for other-rect in rest
          unless (containsp rect other-rect) collect other-rect into filtered
          when (and (not contained-p) (containsp other-rect rect))
          do (setf contained-p t)
          finally
            (setf rest-filtered filtered)
            (return (unless contained-p rect)))
     into result
     finally (return (delete-if #'null result))))

(defun subrect (w h rect)
  (with-rect (x y) rect
    (list x y w h)))

(defun place-rect (w h free-rects)
  (let* ((free-rect (find-free-rect w h free-rects))
         (result (subrect w h free-rect)))
    (values result (normalize-free-space (subdivide-intersecting result
                                                                 free-rects)))))

(defun pack (dimensions &key width height)
  (labels ((largest-side (el)
             (max (second el) (third el)))
           (shortest-side (el)
             (min (second el) (third el)))
           (short-side-last ()
             (sort dimensions #'> :key #'shortest-side))
           (double-sorted-dimensions ()
             (sort (short-side-last) #'> :key #'largest-side)))

  (loop with free-rects = (list (list 0 0 width height))
     for (id rect-width rect-height) in (double-sorted-dimensions) collect
       (multiple-value-bind (rect new-free-rects)
           (place-rect rect-width rect-height free-rects)
         (setf free-rects new-free-rects)
         (with-rect (x y w h) rect
           (list id x y w h))))))
