#++(ql:quickload '(zpb-ttf cl-vectors cl-paths cl-aa cl-aa-misc cl-paths-ttf
                   opticl))
(defpackage #:sdf
  (:use :cl :zpb-ttf))
(in-package #:sdf)

(defparameter *default-characters*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.,;:?!@#$%^&*()-_<>'\"$[] ")

(defun dist (image x y w h search sdf-scale)
  (declare (optimize speed (debug 0)))
  (check-type search (unsigned-byte 16))
  (check-type y (unsigned-byte 16))
  (check-type x (unsigned-byte 16))
  (check-type w (unsigned-byte 16))
  (check-type h (unsigned-byte 16))
  (check-type sdf-scale (unsigned-byte 16))
  (check-type image (simple-array (unsigned-byte 8) (* * 3)))

  (locally (declare (type (simple-array (unsigned-byte 8) (* * 3)) image))
    (let* ((d (float search 1.0))
           (xs (float (* x sdf-scale) 1.0))
           (ys (float (* y sdf-scale) 1.0))
           (fsearch (float search 1.0))
           (px (aref image (floor (* y sdf-scale)) (floor (* x sdf-scale)) 0)))
      (declare (type (single-float 0.0 65536.0) d))
      (check-type xs (single-float 0.0 65536.0))
      (check-type ys (single-float 0.0 65536.0))
      (locally (declare (type (single-float 0.0 65536.0) xs ys fsearch)
                        (type (unsigned-byte 16) w h))
        (flet ((x (y2 dy)
                 (declare (fixnum dy))
                 (loop for x2 from (max 0
                                        (floor
                                         (- xs (min (1+ d )
                                                    fsearch))))
                         below (min w
                                    (ceiling
                                     (+ xs (min (1+ d)
                                                fsearch))))
                       for px2 = (aref image y2 x2 0)
                       do (when (/= px px2)
                            (setf d
                                  (min d
                                       (sqrt
                                        (+ (expt (- x2 xs) 2)
                                           (expt (float dy 1.0) 2)))))))))
          (x (floor ys) 0)
          (loop for dy fixnum from 1 below search
                do (when (< dy search)
                     (let ((a (floor (- ys dy)))
                           (b (floor (+ ys dy))))
                       (declare (fixnum a b))
                       (when (> a 0)
                         (x a dy))
                       (when (< b h)
                         (x b dy))))
                   ;; early out of outer search loop
                   ;; when distance to scanline is
                   ;; more than best distance
                when (> dy d)
                  return nil)))
      (if (plusp px) d (- d)))))

(defun sdf (font glyph font-scale sdf-scale spread)
  (let* ((scale (* sdf-scale font-scale))
         (gw (- (xmax glyph) (xmin glyph)))
         (gh (- (ymax glyph) (ymin glyph)))
         (padding (ceiling (* 1/2 spread font-scale (units/em font))))
         (search (ceiling (* 1/2 spread scale (units/em font))))
         (dw (+ (* 2 padding) (ceiling (* font-scale gw))))
         (dh (+ (* 2 padding) (ceiling (* font-scale gh))))
         (w (* dw sdf-scale))
         (h (* dh sdf-scale)))
    (declare (fixnum w h))
    (let* ((image (aa-misc:make-image w h #(0 0 0)))
           (state (aa:make-state))
           (px (aa-misc:image-put-pixel image #(255 255 255))))
      (vectors:update-state
       state (paths-ttf:paths-from-glyph
              glyph
              :offset (paths:make-point (- (/ (- w (* scale gw)) 2)
                                           (* (xmin glyph) scale))
                                        (- (/ (- h (* scale gh)) 2)
                                           (* (ymin glyph) scale)))
              :scale-x scale
              :scale-y scale))
      (aa:cells-sweep state (lambda (x y a)
                              (if (>= (abs a) 128)
                                  (funcall px x y 255))))
      (time
       (let* ((dest (aa-misc:make-image dw dh #(0 0 0)))
              (write (aa-misc:image-put-pixel dest #(255 255 255)))
              )
         (declare ;(type (simple-array (unsigned-byte 8) 3) dest image)
          (type (unsigned-byte 16) search))
         (loop for y below (array-dimension dest 0)
               do (loop for x below (array-dimension dest 1)
                        for d = (dist image x y w h search sdf-scale)
                        do (funcall write x y (+ 128 (* 128 (/ d search))))))
         #++(aa-misc:save-image "/tmp/font2.pnm" dest :pnm)
         #++(aa-misc:save-image "/tmp/font2h.pnm" image :pnm)
         dest)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MAXRECT packing
;;

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
  (loop with free-rects = (list (list 0 0 width height))
     for (id rect-width rect-height) in dimensions collect
       (multiple-value-bind (rect new-free-rects)
           (place-rect rect-width rect-height free-rects)
         (setf free-rects new-free-rects)
         (with-rect (x y w h) rect
           (list id x y w h)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun stupid-packing (dimensions &key width height)
  (loop with x = 0
        with y = 0
        with row-height = 0
        for (id w h) in dimensions
        when (> (+ x w) (or width 512))
          do (setf x 0)
             (incf y row-height)
             (setf row-height 0)
        do (setf row-height (max row-height h))
        collect (list id x y w h)
        do (incf x w)))

(defun make-atlas (png-filename metrics-filename
                   font-name pixel-size
                   &key (scale 8) (spread 0.1)
                     (string *default-characters*)
                     width height)
  (zpb-ttf:with-font-loader (ttf font-name)
    (let* ((glyphs (loop for c across string
                         ;; possibly check zpb-ttf:glyph-exists-p
                         ;; instead of storing box or whatever
                         ;; missing chars get replaced with?
                         collect (zpb-ttf:find-glyph c ttf)))
           (font-height (- (zpb-ttf:ascender ttf)
                           (zpb-ttf:descender ttf)))
           (font-scale (/ pixel-size font-height)))

      (list :asc (zpb-ttf:ascender ttf)
            :desc (zpb-ttf:descender ttf)
            :bounds (loop for g in glyphs
                          collect (zpb-ttf:bounding-box g)))
      #++
      (format t "~%~%em = ~s = ~s ~s~%" (zpb-ttf:units/em ttf)
              (float (* (zpb-ttf:units/em ttf) font-scale))
              (float (* scale (zpb-ttf:units/em ttf) font-scale)))
      (let* ((images
               (loop for c across string
                     for g = (zpb-ttf:find-glyph c ttf)
                     for i from 0 ;;below 3
                     for sdf = (sdf ttf g font-scale scale spread)
                     collect (cons g sdf)
                     #+do (aa-misc:save-image
                           (format nil "/tmp/font/font~a.pnm" i) sdf :pnm)))
             (pack (pack
                    (loop for i in images
                          collect (list i
                                        (array-dimension (cdr i) 1)
                                        (array-dimension (cdr i) 0)))
                    :width 368
                    :height 368))
             (dims (loop for (nil x y w h) in pack
                         maximize (+ x w) into width
                         maximize (+ y h) into height
                         finally (return (list width height)))))
        (time
         (let* ((out (aa-misc:make-image (first dims) (second dims) #(0 0 0)))
                (write (aa-misc:image-put-pixel out #(255 255 255))))
           (loop for ((g . i) x y w h) in pack
                 do (loop for ox from x
                          for ix below w
                          do (loop for oy from y
                                   for iy below h
                                   do (funcall write ox oy
                                               (aref i (- h iy 1) ix 0)))))
           (opticl:write-image-file png-filename out)))))))

#++
(time
 (make-atlas "/tmp/atlas.png" "/tmp/font2.met"
             "/windows/fonts/arial.ttf" 48
             :scale 64
             :spread 0.2
             :string "ABCabc"))

#++
(time
 (make-atlas "/tmp/atlas.png" "/tmp/font2.met"
             "/windows/fonts/arial.ttf" 48
             :scale 32
             :spread 0.1))



#++
(time
 (make-atlas "/tmp/atlas.pnm" "/tmp/font2.met"
             "/Library/Fonts/Arial.ttf" 48
             :scale 32
             :spread 0.1))
