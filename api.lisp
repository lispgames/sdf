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

(defun obtain-glyph-data (string font-scale scale spread ttf)
  (flet ((fscale (v)
           (ceiling (* v font-scale))))
    (loop for c across string
          ;; possibly check zpb-ttf:glyph-exists-p
          ;; instead of storing box or whatever
          ;; missing chars get replaced with?
          for g = (zpb-ttf:find-glyph c ttf)
          for shape = (st::shape-from-glyph g)
          for sdf = (b::make-sdf (ecase *backend*
                                   (:sdf :sdf)
                                   (:psdf :psdf)
                                   (:msdf :msdf)
                                   ((:mtsdf :msdf+a) :mtsdf)
                                   ;; todo
                                   #++ (:smsdf ()))
                                 shape
                                 :spread spread
                                 :scale scale
                                 #++(/ (zpb-ttf:units/em ttf) font-scale)
                                 :render t)
          for padding = 0
          for origin = (b::origin sdf)
          collect (list
                   :glyph g
                   :metrics (st::make-glyph-metrics
                             :character c
                             :origin #++(list (+ (ceiling (+ (* font-scale
                                                             (zpb-ttf:xmin g))))
                                              (aref origin 0))
                                           (+ (ceiling (+ (* font-scale
                                                             (zpb-ttf:ymin g))))
                                              (aref origin 1)))
                                     (list (aref origin 0)
                                           (aref origin 1)

                                           #++
                                           (- (array-dimension (b::image sdf) 0)
                                         (aref origin 1)))
                             :advance-width (fscale (zpb-ttf:advance-width g))
                             :left-side-bearing (fscale (zpb-ttf:left-side-bearing g))
                             :right-side-bearing (fscale (zpb-ttf:right-side-bearing g)))
                   :sdf sdf))))

#++
(defun find-available-glyphs (ttf)
  (let ((table (make-hash-table :test 'eql)))
    (loop for idx from 0 below (zpb-ttf:glyph-count ttf)
          for cp = (zpb-ttf:code-point (zpb-ttf:index-glyph idx ttf))
          do (setf (gethash cp table) cp))
    (let ((string (make-string (hash-table-count table))))
      (loop for i from 0 below (length string)
            for cp being the hash-keys of table
            do (setf (aref string i) (code-char cp)))
      string)))


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

(defun make-atlas (font-name pixel-size
                   &key (scale 8) (spread 8.5)
                     string
                     (width :auto) (height :auto)
                     ((:mode *backend*) :sdf)
                     (auto-size-granularity-x 1)
                     (auto-size-granularity-y 1)
                     (optimize-pack nil)
                     (trim nil)
                     (expand-mode :restart))
  (declare (ignorable expand-mode scale))
  (zpb-ttf:with-font-loader (ttf font-name)
    #++
    (format t "os2 = ~{~s = ~s~^~%   ~}~%" (zpb-ttf::load-os/2-info ttf))
    #++
    (format t "asc = ~s, desc = ~s, gap = ~s~%"
            (zpb-ttf:ascender ttf) (zpb-ttf:descender ttf) (zpb-ttf:line-gap ttf))
    (unless string
      (setf string (find-available-glyphs ttf)))
    (let* (#++(font-height (- (zpb-ttf:ascender ttf)
                              (zpb-ttf:descender ttf)))
           (font-scale (/ pixel-size
                          (zpb-ttf:units/em ttf)
                          #++ font-height))
           (max-width (if (numberp width) width 16384))
           (max-height (if (numberp height) height 16384))
           (glyph-data (time
                        (obtain-glyph-data string
                                           font-scale
                                           #++(/ (zpb-ttf:units/em ttf)
                                                 (- pixel-size (* 2 spread)))
                                           (/ (zpb-ttf:units/em ttf)
                                              pixel-size)
                                           #++ scale spread ttf)))
           (pixels 0)
           (rects (loop for g in glyph-data
                        for sdf = (getf g :sdf)
                        for w = (array-dimension (b::image sdf) 1)
                        for h = (array-dimension (b::image sdf) 0)
                        #++do (format t "pack ~s~%" (array-dimensions (b::image sdf)))
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

           (%pack (time
                   (multiple-value-list
                    (binpack/2:auto-pack
                     rects
                     max-width max-height
                     :multipage nil
                     :growth-policy (when (or (eql width :auto)
                                              (eql height :auto))
                                      (make-instance 'binpack/2::shaping-quantized
                                                     :w w1
                                                     :h h1
                                                     :dx auto-size-granularity-x
                                                     :dy auto-size-granularity-y))))))
           (pack (car %pack))
           (dims (cdr %pack)))
      #++(format t "em = ~s, f-h = ~s (~s ~s)~%"
                 (zpb-ttf:units/em ttf)
                 font-height (zpb-ttf:ascender ttf) (zpb-ttf:descender ttf))
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
      (time
       (let* ((out (make-array (list height width 4)
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
                        (loop with si of-type (simple-array single-float (* * 4))
                                = (b::image sdf)
                              for ox from x
                              for ix below w
                              do (loop for oy from y
                                       for iy below h
                                       do (loop for i below 4
                                                do (setf (aref out oy ox i)
                                                         (s (aref (b::image sdf)
                                                                  (- h iy 1)
                                                                  ix i))))))))))
         (st::%make-atlas *backend* spread out
                          (make-metrics pixel-size glyph-data font-scale ttf)
                          (st::make-atlas-padding :all spread)))))))

(defun save-atlas (atlas png-filename)
  (opticl:write-image-file png-filename (st::atlas-image atlas)))
