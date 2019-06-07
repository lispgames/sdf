(in-package #:sdf)

;; brute force multisampling SDF generator

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

(defun sdf/ms (font glyph font-scale sdf-scale spread)
  (declare (ignorable font))
  (let* ((scale (* sdf-scale font-scale))
         (gw (- (xmax glyph) (xmin glyph)))
         (gh (- (ymax glyph) (ymin glyph)))
         (padding spread)
         (search (ceiling (* spread sdf-scale)))
         (dw (ceiling (+ 2 (* 2 padding) (* font-scale gw))))
         (dh (ceiling (+ 2 (* 2 padding) (* font-scale gh))))
         (w (ceiling (* dw sdf-scale)))
         (h (ceiling (* dh sdf-scale))))
    (declare (fixnum w h))
    (let* ((image (aa-misc:make-image w h #(0 0 0)))
           (state (aa:make-state))
           (px (aa-misc:image-put-pixel image #(255 255 255))))
      (vectors:update-state
       state (paths-ttf:paths-from-glyph
              glyph
              :offset (paths:make-point (- (/ (- w (* scale gw) sdf-scale)
                                              2)
                                           (* (xmin glyph) scale))
                                        (- (/ (- h (* scale gh) sdf-scale)
                                              2)
                                           (* (ymin glyph) scale)))
              :scale-x scale
              :scale-y scale))
      (aa:cells-sweep state (lambda (x y a)
                              (if (>= (abs a) 128)
                                  (funcall px x y 255))))
      (time
       (let* ((dest (aa-misc:make-image dw dh #(0 0 0)))
              (write (aa-misc:image-put-pixel dest #(255 255 255))))
         (declare ;(type (simple-array (unsigned-byte 8) 3) dest image)
          (type (unsigned-byte 16) search))
         (loop for y below (array-dimension dest 0)
               do (loop for x below (array-dimension dest 1)
                        for d = (dist image x y w h search sdf-scale)
                        for iy = (- dh 1 y)
                        do (funcall write x iy (+ 128 (* 128 (/ d search))))))
         (aa-misc:save-image "/tmp/font2a.pnm" dest :pnm)
         #++(aa-misc:save-image "/tmp/font2h.pnm" image :pnm)
         (values dest padding))))))
