(in-package #:sdf)

(defun assign-colors (shape)
  (let ((rgb #( #(nil t t) #(t nil t) #(t t nil)))
        (c 1))
    (loop for p in (points shape)
          for c1 = (p-e1 p)
          for c2 = (p-e2 p)
          when (s-start c1)
            do (setf (s-channels c1) (aref rgb 2))
          when (p-corner p)
            do (setf c (mod (1+ c) 2))
          do (unless (or (equalp (s-channels c2) #(t t t)))
               (format t "2~s -> ~s, was ~s~%" c2 (aref rgb c) (s-channels c2)))
             (when (equalp (s-channels c2) #(t t t))
               (setf (s-channels c2) (aref rgb c))))
    (loop for s in (curves shape)
          for i from 0
          do (assert (not (equalp (s-channels s) #(t t t)))))
    (loop for s in (lines shape)
          for i from 0
          do (assert (not (equalp (s-channels s) #(t t t)))))
    shape))

(defun msdf (font glyph font-scale ms-scale spread)
  (declare (ignore ms-scale font))
  (let* ((scale font-scale)
         (spread (/ spread))
         (gw (- (xmax glyph) (xmin glyph)))
         (gh (- (ymax glyph) (ymin glyph)))
         (padding (+ 0 spread))
         (dw (ceiling (+ 2 (* 2 padding) (* gw scale))))
         (dh (ceiling (+ 2 (* 2 padding) (* gh scale))))
         (rx (* 1 (- (random 1.0) 0.5)))
         (ry (* 1 (- (random 1.0) 0.5))))
    (format t "~s / ~s ~sx~s~%" (zpb-ttf:postscript-name glyph)
            (zpb-ttf:code-point glyph) dw dh)
    (format t " :random offset ~x,~x~%" rx ry)
    (when (or (zerop gw) (zerop gh))
      (return-from msdf (values (aa-misc:make-image 4 4 #(0 0 0)) 2)))
    (let* ((segments nil))
      (setf segments (assign-colors (translate-glyph glyph scale)))
      (setf (car *f*) segments)
      (let* ((dest (aa-misc:make-image (ceiling dw) (ceiling dh) #(0 0 0)))
             (write (aa-misc:image-put-pixel dest #(255 255 255))))
        (declare (ignorable write))
        (loop for y below (array-dimension dest 0)
              do (loop for x below (array-dimension dest 1)
                       for fx = (- x (- (/ (- dw (* scale gw) 1 rx)
                                           2)
                                        (* (xmin glyph) scale)))
                       for fy =  (- y (- (/ (- dh (* scale gh) 1 ry)
                                            2)
                                         (* (ymin glyph) scale)))
                       for (r g b) = (dist/s (v2 fx fy) segments nil)
                       for dy = y
                       do (if (< (abs r) 100000)
                              (setf (aref dest dy x 0)
                                    (min 255
                                         (max 0 (round
                                                 (+ 128
                                                    (* 128 (/ r spread)))))))
                              (setf (aref dest dy x 0)
                                    (random 255)))
                          (if (< (abs g) 100000)
                              (setf (aref dest dy x 1)
                                    (min 255
                                         (max 0 (round
                                                 (+ 128
                                                    (* 128 (/ g spread)))))))

                              (setf (aref dest dy x 1)
                                    (random 255)))
                          (if (< (abs b) 100000)
                              (setf (aref dest dy x 2)
                                    (min 255
                                         (max 0 (round
                                                 (+ 128
                                                    (* 128 (/ b spread)))))))
                              (setf (aref dest dy x 2)
                                    (random 255)))))
        (aa-misc:save-image "/tmp/font2a.pnm" dest :pnm)
        #++(aa-misc:save-image "/tmp/font2h.pnm" image :pnm)
        (values dest padding)))))


