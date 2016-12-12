#++(ql:quickload '(zpb-ttf cl-vectors cl-paths cl-aa cl-aa-misc cl-paths-ttf))
(defpackage #:sdf
  (:use :cl :zpb-ttf))
(in-package #:sdf)

(defparameter *default-characters*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.,;:?!@#$%^&*()-_<>'\"$[] ")


(defun sdf (font char glyph font-scale sdf-scale font-height spread)
  (let* ((scale (* sdf-scale font-scale))
         (gw (- (xmax glyph) (xmin glyph)))
         (gh (- (ymax glyph) (ymin glyph)))
         (padding (ceiling (* 1/2 spread font-scale (units/em font))))
         (search (ceiling (* 1/2 spread scale (units/em font))))
         (dw (+ (* 2 padding) (ceiling (* font-scale gw))))
         (dh (+ (* 2 padding) (ceiling (* font-scale gh))))
         (w (* dw sdf-scale))
         (h (* dh sdf-scale)))
    #++(format t "padding = ~s, search = ~s~%^" padding search)
    (let* ((image (aa-misc:make-image w h
                                     #(0 0 0)))
          (state (aa:make-state))
          (px (aa-misc:image-put-pixel image #(255 255 255))))
      
      (progn
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
                                    (funcall px x y 255)))))
      (time
       (let* ((dest (aa-misc:make-image dw dh #(0 0 255)))
              (write (aa-misc:image-put-pixel dest #(255 255 255))))
         #++(format t "size= ~s ~s~%" dw dh)
         (loop for y below (array-dimension dest 0)
               do (loop for x below (array-dimension dest 1)
                        for px = (aref image (* y sdf-scale)
                                       (* x sdf-scale)
                                       0)
                        for d = search
                        do (loop for x2 from (max 0 (- (* x sdf-scale) search))
                                   below (min w (+ (* x sdf-scale) search))
                                 do (loop for y2 from (max 0
                                                           (floor
                                                            (- (* y sdf-scale)
                                                               (min (1+ d )
                                                                    search))))
                                            below (min h
                                                       (ceiling
                                                        (+ (* y sdf-scale)
                                                           (min (1+ d) search))))
                                          for px2 = (aref image y2 x2 0)
                                          do (when (/= px px2)
                                               (setf d
                                                     (min d
                                                          (sqrt
                                                           (+ (expt (- x2 (* sdf-scale x)) 2)
                                                              (expt (- y2 (* sdf-scale y)) 2)))))))
                                    ;; early out of outer search loop
                                    ;; when distance to scanline is
                                    ;; more than best distance
                                 when (> (- x2 (* x sdf-scale))
                                           d)
                                 return nil)
                           (if (plusp px)
                               (funcall write x y (+ 128 (* 128 (/ d search))))
                               (funcall write x y (- 128 (* 128 (/ d search)))))))
         #++(aa-misc:save-image "/tmp/font2.pnm" dest :pnm)
         #++(aa-misc:save-image "/tmp/font2h.pnm" image :pnm)
         dest)))))

(defun make-atlas (png-filename metrics-filename width height
                   font-name pixel-size
                   &key (scale 8) (spread 0.1)
                     (string *default-characters*))
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
      (format t "~%~%em = ~s = ~s ~s~%" (zpb-ttf:units/em ttf)
              (float (* (zpb-ttf:units/em ttf) font-scale))
              (float (* scale (zpb-ttf:units/em ttf) font-scale))
              )
      (loop for c across string
            for g = (zpb-ttf:find-glyph c ttf)
            for i from 0
            for sdf = (sdf ttf c g font-scale scale pixel-size spread)
            do (aa-misc:save-image (format nil "/tmp/font/font~a.pnm" i)
                                   sdf :pnm)

               #++(sdf ttf (char string 0) (first glyphs) font-scale scale pixel-size
            spread)))))


(time
 (make-atlas "/tmp/font2.png" "/tmp/font2.met"
             256 256
             "/windows/fonts/arial.ttf" 48
             :scale 32
             :spread 0.1))
