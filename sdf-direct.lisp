(in-package #:sdf)

(require 'sb-cga)


(defun dist/line (p p0 p1)
  (let* ((d (v2- p1 p0)))
    (let* ((tt (min 1 (max 0 (/ (v2. (v2- p p0) d)
                                (v2. d d)))))
           (pp (v2+ p0 (v2scale d (min 1 (max 0 tt)))))
           (s (v2x d (v2- pp p))))
      (* (if (plusp s) 1 -1)
         (v2dist p pp)))))

(defun solve-cubic (a b c d)
  (let* ((bc (* b c))
         (ad (* a d))
         (abc (* a bc))
         (abcd (* ad bc))

         (b3 (expt b 3))
         (det (+ (* 18 abcd)
                 (* -4 b3 d)
                 (* bc bc)
                 (* -4 a (expt c 3))
                 (* -27 ad ad)))
         (det0 (+ (expt b 2)
                  (* -3 a c))))
    (list* 0 1
           (cond
             ((and (zerop det) (zerop det0))
              (list (/ b (* -3 a))))
             ((zerop det)
              (list (/ (- (* 9 ad) bc)
                       (* 2 det0))
                    (/ (- (* 4 abc) (* 9 a ad) b3)
                       (* a det0))))
             (t
              (let* ((det1 (+ (* 2 b3) (* -9 abc) (* 27 a ad)))
                     (pm (sqrt (* -27 a a det)))
                     (cc (expt
                          (/ (if (= det1 pm)
                                 (+ det1 pm)
                                 (- det1 pm))
                             2)
                          1/3))
                     (xi (+ -1/2 (/ #c(0 #.(sqrt 3)) 2))))
                (loop for k upto 2
                      for r = (* (/ (* -3 a))
                                 (+ b
                                    (* (expt xi k) cc)
                                    (/ det0 (* (expt xi k) cc))))
                      when (and (< (abs (imagpart r)) 0.0001)
                                (< 0 (realpart r) 1))
                        collect (realpart r))))))))

(defun dist/curve (p p0 p1 p2)
  (let* ((d (v2- p p0))
         (d1 (v2- p1 p0))
         (d2 (v2- p2 (v2- (v2scale p1 2) p0)))
         (roots (solve-cubic
                 (v2. d2 d2)
                 (* 3 (v2. d1 d2))
                 (- (* 2 (v2. d1 d1))
                    (v2. d2 d))
                 (* -1 (v2. d1 d)))))
    (loop with dd = most-positive-single-float
          for r in roots
          for b = (v2+ (v2+ (v2scale d2 (* r r))
                            (v2scale d1 (* 2 r)))
                       p0)
          for d = (v2dist p b)
          when (< d (abs dd))
            do (let* ((db (v2+ (v2scale (v2+ (v2- p2 (v2scale p1 2))
                                             p0)
                                        (* 2 r))
                               (v2scale (v2- p1 p0) 2)))
                      (s (if (plusp (v2x db (v2- b p))) 1 -1)))
                 (setf dd (* s d)))
          finally (return dd))))

(defun dist/s (p p1 p2 p3)
  (if p2
      (dist/curve p p1 p2 p3)
      (dist/line p p1 p3)))

(defun dist/c (p segments)
  (let ((d most-positive-single-float))
    (loop for (p1 p2 p3) in segments
          for dp = (dist/s p p1 p2 p3)
          when (complexp dp)
            do (break "complex ~s?" dp)
          do (when (< (abs dp) (abs d))
               (setf d dp)))
    d))

(defun scale-point (p s)
  (when p
    (v2 (* s (zpb-ttf:x p))
        (* s (zpb-ttf:y p)))))

(defun translate-segment (p1 p2 p3 s)
  (let ((s1 (scale-point p1 s))
        (s2 (scale-point p2 s))
        (s3 (scale-point p3 s)))
    (list s1 s2 s3)))

(defun sdf (font glyph scale spread)
  (let* ((gw (- (xmax glyph) (xmin glyph)))
         (gh (- (ymax glyph) (ymin glyph)))
         (padding (ceiling (* 1/2 spread scale (units/em font))))
         (dw (+ (* 2 padding) (ceiling (* scale gw))))
         (dh (+ (* 2 padding) (ceiling (* scale gh)))))
    (let* ((segments nil))
      (zpb-ttf:do-contours (c glyph)
        (zpb-ttf:do-contour-segments (p1 p2 p3) c
          (push (translate-segment p1 p2 p3 scale)
                segments)))
      (time
       (let* ((dest (aa-misc:make-image dw dh #(0 0 0)))
              (write (aa-misc:image-put-pixel dest #(255 255 255))))
         (loop for y below (array-dimension dest 0)
               do (loop for x below (array-dimension dest 1)
                        for fx = (- x (- (/ (- dw (* scale gw)) 2)
                                         (* (xmin glyph) scale)))
                        for fy = (- y (- (/ (- dh (* scale gh)) 2)
                                         (* (ymin glyph) scale)))
                        for d = (dist/c (v2 fx fy) segments)
                        do (funcall write x (- dh y)
                                    (max 0 (+ 128 (* 128 (/ d 12)))))))
         #++(aa-misc:save-image "/tmp/font2a.pnm" dest :pnm)
         #++(aa-misc:save-image "/tmp/font2h.pnm" image :pnm)
         (values dest padding))))))


#++
(require 'sdf)

#++
(zpb-ttf:with-font-loader (ttf "georgia.ttf")
  (let ((g (zpb-ttf:find-glyph #\* ttf)))
    (time (sdf ttf g 0.05 0.6))
    nil))
