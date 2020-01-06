(in-package #:sdf)

#++
(loop for i in '(6 5 3)
      for s = (ash i 1)
      for i2 = (logand (logior s (ash s -3)) 7)
      collect (list i (loop for x below 3 collect (logbitp x i))
                    i2 (loop for x below 3
                             collect (logbitp x i2))))
#++
((6 (NIL T T) 5 (T NIL T))
 (5 (T NIL T) 3 (T T NIL))
 (3 (T T NIL) 6 (NIL T T)))


(defun assign-colors/full (shape)
  (let* ((rgb #(#(t nil t) #(t t nil) #(nil t t)))
         (c0 0)
         (h (make-hash-table :test 'equalp))
         (ps (coerce (points shape) 'vector))
         (start (or (position-if 'p-corner ps) 0))
         (end (position-if 'p-corner ps :from-end t)))
    #++(format t "start = ~s,end=~s~%" start end)
    #++(format t "~s~%" (map 'vector 'p-corner ps))

    (loop with c = c0
          with l = (length ps)
          for i below l
          for p = (aref ps (mod (+ start i) l))
          when (and (plusp i) (p-corner p))
            do (if (= (mod (+ i start) l) end)
                   (setf c (car (set-difference '(0 1 2)
                                                (list c0 c))))
                   (setf c (mod (+ 1 c) 3)))

          do (setf (gethash (p-p0 p) h) c))
    #++(Format t "~s: ~s ~s ~s~%" start c0 cn-1 cn)
    #++(when (= cn c0)
         (loop with c = (car (set-difference '(0 1 2) (list cn-1 c0)))
               for i downfrom (1- start)
               repeat (length ps)
               for p = (aref ps (mod (+ i (length ps)) (length ps)))
               while (= (gethash (p-p0 p) h) cn)
               do (setf (gethash (p-p0 p) h) c)
                  (format t "reset ~s ~s ~s~%" (position p (points shape)) c
                          (p-p0 p))))

    (loop for s in (contours shape)
          for c = (gethash (s-p0 s) h)
          do (setf (s-channels s) (aref rgb c)))

    (loop for p in (points shape)
          when (p-corner p)
            do (assert (not (equalp (s-channels (p-e1 p))
                                    (s-channels (p-e2 p)))))
                                        ;else do
          #++(assert (equalp (s-channels (p-e1 p))
                             (s-channels (p-e2 p)))))
    (loop for s in (curves shape)
          for i from 0
          do (assert (not (equalp (s-channels s) #(t t t)))))
    (loop for s in (lines shape)
          for i from 0
          do (assert (not (equalp (s-channels s) #(t t t)))))
    shape))

(defun assign-colors/teardrop (shape)
  (when (and (= 1 (length (curves shape)))
             (zerop (length (lines shape))))
    ;; single curve, needs split
    (break "todo: split teardrop curves"))
  (let* ((corner (find-if 'p-corner (points shape)))
         (split (loop with p1 = (car (points shape))
                      with d1 = (v2dist (p-p0 p1) (p-p0 corner))
                      for p in (cdr (points shape))
                      for d = (v2dist (p-p0 p) (p-p0 corner))
                      when (> d d1)
                        do (setf d1 d p1 p)
                      finally (return p1))))
    (loop with c1 = #(t nil t)
          with c2 = #(t t nil)
          with color = c1
          for c in (contours shape)
          if (equalp (p-p0 corner) (s-p0 c))
            do (setf color c2)
          else if (equalp (p-p0 split) (s-p0 c))
                 do (setf color c1)
          do (setf (s-channels c) color)))
  shape)

(defun simplify-shape (shape)
  #++
  (format t "simplify shape ~s ~s ~s~%"
          (length (points shape))
          (length (lines shape))
          (length (curves shape)))
  (let* ((h (make-hash-table :test 'equalp))
         (v (coerce (contours shape) 'vector))
         (delete nil))
    (loop for p in (points shape)
          do (setf (gethash (p-p0 p) h) p))
    #++(loop for c in (curves shape)
             for p0 = (gethash (c-p0 c) h)
             for p2 = (gethash (c-p2 c) h)
             for l = (+ (v2dist (c-p0 c) (c-p1 c))
                        (v2dist (c-p1 c) (c-p2 c)))
             when (and (p-corner p0)
                       (p-corner p2)
                       (< l 0.5))
               do (format t "delete curve? ~s%~s~% ~s~% ~s~%"
                          l (c-p0 c) (c-p1 c) (c-p2 c))
                  (push c delete))
    #++(loop for c in (lines shape)
             for p0 = (gethash (l-p0 c) h)
             for p2 = (gethash (l-p2 c) h)
             for l = (v2dist (l-p0 c) (l-p2 c))
             when (and (p-corner p0)
                       (p-corner p2)
                       (< l 0.4))
               do (format t "delete line? ~s%~s~% ~s~%"
                          l (l-p0 c) (l-p2 c))
                  (push c delete))
    ;; we check for any pair of corners separated by small distance
    ;; along contour and if found remove that segment of the contour.
    ;; probably should also check for small area in addition to length.
    #++(format t "contours = ~s~%" (length (contours shape)))
    (loop with l = (length v)
          with start = (or (loop for i below l
                                 for p = (gethash (s-p0 (aref v i)) h)
                                 when (and (point-p p) (p-corner p))
                                   return i)
                           0)
          with segment = nil
          with segment-len = 0
          for i1 below l
          for i = (mod (+ i1 start) l)
          for s = (aref v i)
          for p = (gethash (s-p0 s) h)
          when (p-corner p)
            do #++
               (when (and segment (< 0.5 segment-len 2))
                 (format t "short segment ~s?: ~s~%" segment-len segment))
               (when (and segment (< segment-len 0.5))
                 (setf segment (reverse segment))
                 #++(format t "delete segment ~s: ~s~%" segment-len segment)
                 #++(loop for d in segment do (setf (aref delete d) t))
                 (push segment delete))
               (setf segment nil
                     segment-len 0)
          do (push i segment)
             (if (typep s 'curve)
                 ;; approximate length of curve by length of control
                 ;; points
                 (incf segment-len
                       (+ (v2dist (c-p0 s) (c-p1 s))
                          (v2dist (c-p1 s) (c-p2 s))))
                 (incf segment-len (v2dist (s-p0 s) (s-p2 s)))))
    #++ ;; not working yet?
    (when delete
      (let ((dh (make-hash-table)))
        (loop
          for ds in delete
          for p1 = (gethash (s-p0 (aref v (first ds))) h)
          for p2 = (loop for d in ds
                         for s = (aref v d)
                         do (setf (gethash s dh) t)
                            (setf (gethash (gethash (s-p2 s) h) dh) t)
                            (when (curve-p s)
                              (setf (gethash (c-p1 s) dh) t))
                         finally (return (gethash (s-p2 s) h)))
          do (setf (p-e2 p1) (p-e2 p2)
                   (p-t2 p1) (p-t2 p2))
             (setf (s-p0 (p-e2 p1)) (p-p0 p1)))
        (flet ((d (x &optional (d t))
                 (loop for p in x
                       unless (gethash p dh)
                         collect p
                       else do (when d
                                 (format t "delete ~s~%" p)))))
          (setf (points shape) (d (points shape)))
          (setf (curves shape) (d (curves shape)))
          (setf (lines shape) (d (lines shape)))
          (setf (contours shape) (d (contours shape) nil))))
      #++
      (loop for c in (contours shape)
            do (format t "~s: ~s~%" (type-of c) (s-p0 c))
               (format t "    ~s~%" (s-p2 c)))))
  shape)

(defun assign-colors (shape)
  (let ((c (count-if 'p-corner (points shape))))
    (case c
      (0 ;; no corners, use white
       (loop for p in (curves shape) do (setf (s-channels p) #(t t t)))
       (loop for p in (lines shape) do (setf (s-channels p) #(t t t)))
       shape)
      (1 ;; teardrop, 2 colors, possibly need to split curve
       (assign-colors/teardrop shape))
      (t                   ;; multiple corners, use 3 colors
       (assign-colors/full ;shape
        (simplify-shape shape))))))

(defparameter *postprocess* t)

(defun postprocess (image)
  (opticl:with-image-bounds (wy wx c) image
    (assert (= c 4))
    (labels ((check4 (a b c d)
               ;; at least 1 channel is always above or always below 0
               (or (and (>= a 128) (>= b 126) (>= c 126) (>= d 126))
                   (and (<= a 128) (<= b 128) (<= c 128) (<= d 128))))
             (check4b (&rest x)
               (count-if (lambda (a) (>= a 127)) x))
             (check1 (a b)
               (let ((as (signum (- a 127)))
                     (bs (signum (- b 127))))
                 (not (eql as bs))))
             (check (a b)
               (flet ((m (a) (- a 127)))
                 (when (and (= 2 (count t (mapcar #'check1 a b)))
                            #++(eql (car (remove (fourth a) a))
                                    (car (remove (fourth b) b)))

                            (let* ((a (mapcar #'m a))
                                   (b (mapcar #'m b))
                                   (a2 (fourth a))
                                   (a1 (car (remove a2 a)))
                                   (b2 (fourth b))
                                   (b1 (car (remove b2 b)))
                                   (d (min (abs (- a2 b2)))))
                              (declare (ignorable d))
                              ;; heuristic to try to avoid gaps while
                              ;; preserving corners
                              (and
                               ;; both pixels are on or both off

                               (= (signum a2) (signum b2))
                               ;; ranges of good and bad values overlap
                               (or (<= a1 b1 a2 b2)
                                   (<= b1 a1 b2 a2)
                                   (<= a1 b1 b2 a2)
                                   (<= b1 a1 a2 b2)
                                   (= a2 b2 -128))
                               ;; not close to edge
                               (> (max (abs a2) (abs b2)) 12)
                               (> (max (abs a1) (abs b1)) 25)
                               ;; similar size ranges
                               (< (abs (- (abs (- a2 a1))
                                          (abs (- b2 b1))))
                                  80)
                               #++
                               (<= (- b1 d) a1 (+ b1 d)))))
                   (let* ((a (mapcar #'m a))
                          (b (mapcar #'m b))
                          (a2 (fourth a))
                          (a1 (car (remove a2 a)))
                          (b2 (fourth b))
                          (b1 (car (remove b2 b)))
                          (d (min (abs (- a2 b2)))))
                     (declare (ignorable a1 a2 b1 b2 d))
                     (format t "~&~{~4,' d ~} |~{ ~4,' d~} [~2d ~2d] {~2d ~2d} @~3,' d ~d ~d ~d~%"
                             a b
                             (min (abs a2) (abs b2))
                             (max (abs a2) (abs b2))
                             (min (abs a1) (abs b1))
                             (max (abs a1) (abs b1))
                             d
                             (abs (- a2 a1))
                             (abs (- b2 b1))
                             (abs (- (abs (- a2 a1))
                                     (abs (- b2 b1))))




                             ))
                   #++(break "~s ~s ~s ~s" a b
                             (mapcar #'check1 (subseq a 0 3)
                                     (subseq b 0 3))
                             (loop for x in a
                                   for y in b
                                   collect (list
                                            (- x 128) (- y 128)
                                            (plusp (- x 128)) (plusp (- y 128)))
                                   )
                             )
                   t)))
             (dont-need-msdf (x)
               (destructuring-bind (r g b a) x
                 (or (<= 3 (count 0 x))
                     (<= 3 (count 255 x))
                     #++(= (abs (- a 127))
                           (min (abs (- r 127))
                                (abs (- g 127))
                                (abs (- b 127))))))))
      (loop
        for y from 1 below (1- wy)
        do (loop
             for x from 1 below (1- wx)
             for p00 = (opticl:pixel* image y x)
             for p01 = (opticl:pixel* image (1+ y) x)
             for p10 = (opticl:pixel* image y (1+ x))
             for p11 = (opticl:pixel* image (1+ y) (1+ x))
             when  (and nil
                        (dont-need-msdf p00)
                        (dont-need-msdf p01)
                        (dont-need-msdf p10)
                        (dont-need-msdf p11))
               do (let ((a (fourth p00)))
                    (format t "~s ~s ~s ~s~%" p00 p01 p10 p11)
                    (format t "~s: ~s ~s ~s~%"
                            (count 0 p00)
                            (count 0 p01)
                            (count 0 p10)
                            (count 0 p11))
                    (setf (opticl:pixel image y x)
                          (values a a a a)))
             else
               when (and  #++(plusp (count t (mapcar #'check4 p00 p01 p10 p11)))
                          (let ((l (mapcar #'check4b p00 p01 p10 p11)))

                            (and (or (plusp (count 0 l))
                                     (plusp (count 4 l)))
                                 (or (plusp (count 3 l))
                                     (plusp (count 1 l)))))
                          (or (check p00 p10)
                              (check p00 p01)
                              #++(check p00 p11)))
                 do (let ((a (fourth p00)))
                      (let ((l (mapcar #'check4b p00 p01 p10 p11)))
                        (format t "~s ~s ~s ~s~%" p00 p01 p10 p11)
                        (format t "~s: ~s ~s ~s ~s~%"
                                l
                                (plusp (count 0 l))
                                (plusp (count 4 l))
                                (plusp (count 3 l))
                                (plusp (count 1 l))))
                      (setf (opticl:pixel image y x)
                            (values a a a a)
                                        ;(values 0 0 0 0)
                            #++
                            (loop for i in p00
                                  collect (if (= i a) i 128)))))))

    )
  )
(defun msdf (font glyph font-scale ms-scale spread
             &key (mode (if (eql *backend* :sdf) :msdf *backend*)))
  (declare (ignore ms-scale font))
  (let* ((scale font-scale)
         (gw (- (xmax glyph) (xmin glyph)))
         (gh (- (ymax glyph) (ymin glyph)))
         (padding (+ 0 spread))
         (dw (ceiling (+ 2 (* 2 padding) (* gw scale))))
         (dh (ceiling (+ 2 (* 2 padding) (* gh scale))))
         (rx (* 0 (- (random 1.0) 0.5)))
         (ry (* 0 (- (random 1.0) 0.5))))

    (format t "~s / ~s ~sx~s~%" (zpb-ttf:postscript-name glyph)
            (zpb-ttf:code-point glyph) dw dh)

    (format t " :random offset ~x,~x~%" rx ry)
    (when (or (zerop gw) (zerop gh))
      (return-from msdf (values (ecase mode
                                  (:msdf
                                   (opticl:make-8-bit-rgb-image
                                    4 4 :initial-element 0))
                                  (:msdf+a
                                   (opticl:make-8-bit-rgba-image
                                    4 4 :initial-element 0)))
                                2)))
    (let* ((segments nil))
      (setf segments (translate-glyph glyph scale :filter 'assign-colors))
      (setf (car *f*) segments)
      (let* ((dest (opticl:make-8-bit-rgba-image
                    (ceiling dh) (ceiling dw) :initial-element 0)))
        (loop for y below (array-dimension dest 0)
              do (loop for x below (array-dimension dest 1)
                       for fx = (- x (- (/ (- dw (* scale gw) 1 rx)
                                           2)
                                        (* (xmin glyph) scale)))
                       for fy =  (- y (- (/ (- dh (* scale gh) 1 ry)
                                            2)
                                         (* (ymin glyph) scale)))
                       for (r g b a) = (dist/s (v2 fx fy) segments t)
                       for dy = (- dh 1 y)
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
                                    (random 255)))
                          (setf (aref dest dy x 3)
                                (min 255
                                     (max 0 (round
                                             (+ 128
                                                (* 128 (/ a spread)))))))
                       ))
        (when *postprocess*
          (postprocess dest))
        (ecase mode
          #++(eql *backend* :msdf)
          (:msdf
           (setf dest (opticl:convert-image-to-rgb dest)))
          (:msdf+a))
        (aa-misc:save-image "/tmp/font2a.pnm" dest :pnm)
        #++(aa-misc:save-image "/tmp/font2h.pnm" image :pnm)
        (values dest padding)))))
