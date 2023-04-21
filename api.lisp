(in-package #:sdf)

(defmacro with-glyph-data ((glyph metrics
                            &optional (sdf (gensym)) (padding (gensym)))
                           data
                           &body body)
  `(destructuring-bind (&key ((:glyph ,glyph))
                          ((:metrics ,metrics))
                          ((:sdf ,sdf))
                          ((:padding ,padding)) &allow-other-keys)
       ,data
     (declare (ignorable ,glyph ,metrics ,sdf ,padding))
     ,@body))


(defun make-kerning-table (glyph-data scale font)
  (loop with table = (make-hash-table :test 'equal)
        for d0 in glyph-data
        do (loop for d1 in glyph-data
                 do (with-glyph-data (g0 m0) d0
                      (with-glyph-data (g1 m1) d1
                        (when (and m0 m1)
                          (let ((offset (zpb-ttf:kerning-offset g0 g1 font)))
                            (unless (= offset 0)
                              (setf (gethash (cons (st::glyph-character m0)
                                                   (st::glyph-character m1))
                                             table)
                                    (* offset scale))))))))
        finally (return table)))

(defun make-metrics (size glyph-data scale ttf)
  (st::make-font-metrics
   :size size
   :glyphs (mapcar (lambda (g) (getf g :metrics)) glyph-data)
   :ascender (* scale (zpb-ttf:ascender ttf))
   :descender (* scale (zpb-ttf:descender ttf))
   :line-gap (* scale (zpb-ttf:line-gap ttf))
   :kerning-table (make-kerning-table glyph-data scale ttf)))

(defvar *backend* :sdf)

(defun obtain-glyph-data (string font-scale scale spread ttf &key verbose)
  (flet ((fscale (v)
           (ceiling (* v font-scale))))
    (loop for c across string
          for i from 0
          ;; possibly check zpb-ttf:glyph-exists-p
          ;; instead of storing box or whatever
          ;; missing chars get replaced with?
          for g = (zpb-ttf:find-glyph c ttf)
          for shape = (sdf/cleaner::fix-shape
                       (st::shape-from-glyph g))
          for sdf = (progn
                      (cond
                        ((eql verbose t)
                         (format t "~s/~s: index:~s code:#x~x ~s~%"
                                 i (length string)
                                 (zpb-ttf:font-index g) (char-code c)
                                 c))
                        ((eql verbose :dots)
                         (format t "."))
                        ((functionp verbose)
                         (funcall verbose :render i (length string))))
                      (b::make-sdf (ecase *backend*
                                     (:sdf :sdf)
                                     (:psdf :psdf)
                                     (:msdf :msdf)
                                     ((:mtsdf :msdf+a) :mtsdf)
                                     ;; todo
                                     #++ (:smsdf ()))
                                   shape
                                   :spread spread
                                   :scale scale
                                   :render t))
          for padding = 0
          for origin = (b::origin sdf)
          collect (list
                   :glyph g
                   :metrics (st::make-glyph-metrics
                             :character c
                             :origin (list (aref origin 0)
                                           (aref origin 1))
                             :advance-width (fscale (zpb-ttf:advance-width g))
                             :left-side-bearing (fscale (zpb-ttf:left-side-bearing g))
                             :right-side-bearing (fscale (zpb-ttf:right-side-bearing g)))
                   :sdf sdf))))

(defun find-available-glyphs (ttf)
  (let ((table (make-hash-table :test 'eql)))
    (loop for idx from 0 below (zpb-ttf:glyph-count ttf)
          for glyph = (zpb-ttf:index-glyph idx ttf)
          for cp = (zpb-ttf:code-point glyph)
          do (when (or #+sbcl (sb-unicode:whitespace-p (code-char cp))
                       (< 0 (length (zpb-ttf:contours glyph))))
               (setf (gethash cp table) cp)))
    (let ((string (make-string (hash-table-count table))))
      (loop for i from 0 below (length string)
            for cp being the hash-keys of table
            do (setf (aref string i) (code-char cp)))
      string)))

(defun fill-atlas (width height channels pack)
  ;; fixme: don't duplicate this loop?
  (if (= channels 1)
      (let ((out (make-array (list height width)
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
        (loop for rect in pack
              do (binpack:with-rect (g x y w h) rect
                   (with-glyph-data (glyph metrics sdf padding) g
                     (when metrics
                       (setf (st::glyph-bounding-box metrics)
                             (list x y (+ x w) (+ y h))))
                     (flet ((s (x)
                              (max 0
                                   (min 255
                                        (round (+ 128 (* 127 x)))))))
                       (loop with si of-type (simple-array single-float (* * 1))
                               = (b::image sdf)
                             for ox from x
                             for ix below w
                             do (loop for oy from y
                                      for iy below h
                                      do (setf (aref out oy ox)
                                               (s (aref (b::image sdf)
                                                        (- h iy 1) ix 0)))))))))
        out)
      (let ((out (make-array (list height width channels)
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
        (loop for rect in pack
              do (binpack:with-rect (g x y w h) rect
                   (with-glyph-data (glyph metrics sdf padding) g
                     (when metrics
                       (setf (st::glyph-bounding-box metrics)
                             (list x y (+ x w) (+ y h))))
                     (flet ((s (x)
                              (max 0
                                   (min 255
                                        (round (+ 128 (* 127 x)))))))
                       (loop with si of-type (simple-array single-float (* * *))
                               = (b::image sdf)
                             for ox from x
                             for ix below w
                             do (loop for oy from y
                                      for iy below h
                                      do (loop for i below channels
                                               do (setf (aref out oy ox i)
                                                        (s (aref (b::image sdf)
                                                                 (- h iy 1)
                                                                 ix i))))))))))
        out)))

(defun make-atlas (font-name pixel-size
                   &key (spread 8.5)
                     string
                     (width :auto) (height :auto)
                     ((:mode *backend*) :sdf)
                     (auto-size-granularity-x 1)
                     (auto-size-granularity-y 1)
                     (optimize-pack nil)
                     (trim nil)
                     verbose
                     ;; old options, no longer used
                     (expand-mode :restart) (scale 8))
  (declare (ignorable expand-mode scale))
  (zpb-ttf:with-font-loader (ttf font-name)
    (unless string
      (setf string (find-available-glyphs ttf)))
    (let* ((font-scale (/ pixel-size
                          (zpb-ttf:units/em ttf)))
           (max-width (if (numberp width) width 16384))
           (max-height (if (numberp height) height 16384))
           (glyph-data (obtain-glyph-data string
                                          font-scale
                                          (/ (zpb-ttf:units/em ttf)
                                             pixel-size)
                                          spread ttf
                                          :verbose verbose))
           (pixels 0)
           (rects (loop for g in glyph-data
                        for i fixnum from 0
                        for sdf = (getf g :sdf)
                        for w = (array-dimension (b::image sdf) 1)
                        for h = (array-dimension (b::image sdf) 0)
                        collect (binpack:rect g 0 0 w h)
                        do (incf pixels (* w h))))
           (w1 (if (numberp width)
                   width
                   (* auto-size-granularity-x
                      (ceiling (sqrt pixels) auto-size-granularity-x))))
           (h1 (if (numberp height)
                   height
                   (* auto-size-granularity-y
                      (ceiling (sqrt pixels) auto-size-granularity-y))))

           (%pack (let ((l (length rects)))
                    (cond
                      ((eql verbose t)
                       (format t "packing ~s glyphs = ~s pixels~%" l pixels))
                      ((eql verbose :dots)
                       (format t "P")))
                    (multiple-value-list
                     (binpack/2:auto-pack
                      rects
                      max-width max-height
                      :multipage nil
                      ;; w+h sort is usually best, and others tend to
                      ;; pack more slowly, so better results from
                      ;; spending the time doing a few extra w+h packs
                      ;; with different starting sizes when we want to
                      ;; optimize
                      :sorts '(binpack/2:sort-rects/w+h-desc)
                      :growth-policy
                      (when (or (eql width :auto)
                                (eql height :auto))
                        (make-instance 'binpack/2:shaping-quantized
                                       :w w1
                                       :h h1
                                       :dx auto-size-granularity-x
                                       :dy auto-size-granularity-y))))))
           (pack (car %pack))
           (dims (cdr %pack)))
      (when (eql verbose t)
        (format t " packed to ~s (~s,~s)~%"
                (* (first dims) (second dims)) (first dims) (second dims)))
      (when (and optimize-pack
                 (or (eql width :auto) (eql height :auto)))
        ;; optionally try packing into some other sizes, since initial
        ;; size affects heuristic behavior
        (when (eql verbose t)
          (format t "optimizing pack...~%"))
        (let ((best-dims dims)
              (best-pack pack)
              (n (if (integerp optimize-pack)
                     (alexandria:clamp optimize-pack 1 4)
                     2))
              (a1 (* (* auto-size-granularity-x
                        (ceiling (first dims) auto-size-granularity-x))
                     (* auto-size-granularity-y
                        (ceiling (second dims) auto-size-granularity-y)))))
          (flet ((try1 (i dx dy)
                   (let* ((w1 (* auto-size-granularity-x
                                 (max 1 (- (ceiling (first dims)
                                                    auto-size-granularity-x)
                                           dx))))
                          (h1 (* auto-size-granularity-y
                                 (max 1 (- (ceiling (second dims)
                                                    auto-size-granularity-y)
                                           dy))))
                          (p2 (progn
                                (when (eql verbose :dots)
                                  (format t "p"))
                                (multiple-value-list
                                 (binpack/2:auto-pack
                                  rects
                                  max-width max-height
                                  :multipage nil
                                  :sorts '(binpack/2:sort-rects/w+h-desc)
                                  :growth-policy
                                  (make-instance 'binpack/2:shaping-quantized
                                                 :w w1
                                                 :h h1
                                                 :dx auto-size-granularity-x
                                                 :dy auto-size-granularity-y)))))
                          (d2 (cdr p2))
                          (a2 (* (* auto-size-granularity-x
                                    (ceiling (first d2)
                                             auto-size-granularity-x))
                                 (* auto-size-granularity-y
                                    (ceiling (second d2)
                                             auto-size-granularity-y)))))
                     (when (eql verbose t)
                       (format t " repacked ~s(-~s,-~s) @ ~s,~s -> ~s (~s) = ~s~%"
                               i dx dy w1 h1 (* (first d2) (second d2)) a2 d2))
                     (when (< a2 a1)
                       (setf a1 a2
                             best-dims d2
                             best-pack (car p2))))))
            (loop with xb = n with yb = n
                  for i from 1 below (expt 2 (+ xb yb))
                  do (try1 i (ldb (byte xb 0) i) (ldb (byte yb xb) i))
                     (when (>= i (expt 2 (min xb yb)))
                       (try1 i i i))))
          (setf dims best-dims pack best-pack))
        (when (eql verbose t)
          (format t "best -> ~s ~s~%"  (* (first dims) (second dims)) dims)))
      (when (or (eql width :auto) optimize-pack)
        (setf width
              (* auto-size-granularity-x
                 (ceiling (first dims) auto-size-granularity-x))))
      (when (or (eql height :auto) optimize-pack)
        (setf height
              (* auto-size-granularity-y
                 (ceiling (second dims) auto-size-granularity-y))))
      (when trim
        (setf height (second dims))
        (unless (eql trim :y-only)
          (setf width (first dims))))
      (when (eql verbose t)
        (format t "final size = ~sx~s (~s pixels)~%"
                width height (* width height)))
      (let* ((out (fill-atlas width height (b::channels-for-type *backend*)
                              pack)))
        (st::%make-atlas *backend* spread out
                         (make-metrics pixel-size glyph-data font-scale ttf)
                         (st::make-atlas-padding :all spread))))))

(defun save-atlas (atlas png-filename)
  (opticl:write-image-file png-filename (st::atlas-image atlas)))
