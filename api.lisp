(in-package #:sdf)

(defmacro with-glyph-data ((glyph metrics &optional (sdf (gensym)) (padding (gensym))) data &body body)
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
                        (let ((offset (zpb-ttf:kerning-offset g0 g1 font)))
                          (unless (= offset 0)
                            (setf (gethash (cons (glyph-character m0) (glyph-character m1)) table)
                                  (* offset scale)))))))
        finally (return table)))

(defun make-metrics (size glyph-data scale ttf)
  (make-font-metrics
   :size size
   :glyphs (mapcar (lambda (g) (getf g :metrics)) glyph-data)
   :ascender (* scale (zpb-ttf:ascender ttf))
   :descender (* scale (zpb-ttf:descender ttf))
   :line-gap (* scale (zpb-ttf:line-gap ttf))
   :kerning-table (make-kerning-table glyph-data scale ttf)))

(defun obtain-glyph-data (string font-scale scale spread ttf)
  (flet ((fscale (v)
           (ceiling (* v font-scale))))
    (loop for c across string
          ;; possibly check zpb-ttf:glyph-exists-p
          ;; instead of storing box or whatever
          ;; missing chars get replaced with?
          for g = (zpb-ttf:find-glyph c ttf)
          collect (multiple-value-bind (sdf padding)
                      (ecase *backend*
                        ((:sdf :psdf)
                         (sdf ttf g font-scale scale spread))
                        (:sdf-ms
                         (sdf/ms ttf g font-scale scale spread))
                        ((:msdf :msdf+a)
                         (msdf ttf g font-scale scale spread)))
                    (list
                     :glyph g
                     :metrics (make-glyph-metrics
                               :character c
                               :origin (list (+ (ceiling (- (* font-scale (xmin g)))) padding)
                                             (+ (ceiling (- (* font-scale (ymin g)))) padding))
                               :advance-width (fscale (zpb-ttf:advance-width g))
                               :left-side-bearing (fscale (zpb-ttf:left-side-bearing g))
                               :right-side-bearing (fscale (zpb-ttf:right-side-bearing g)))
                     :sdf sdf)))))

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

(defun make-atlas (font-name pixel-size
                   &key (scale 8) (spread 2.5)
                     string
                     (width :auto) (height :auto)
                     ((:mode *backend*) :sdf)
                     (auto-size-granularity-x 1)
                     (auto-size-granularity-y 1)
                     (optimize-pack nil)
                     (trim nil)
                     (expand-mode :restart))
  (zpb-ttf:with-font-loader (ttf font-name)
    (unless string
      (setf string (find-available-glyphs ttf)))
    (let* ((font-height (- (zpb-ttf:ascender ttf)
                           (zpb-ttf:descender ttf)))
           (font-scale (/ pixel-size font-height))
           (glyph-data (obtain-glyph-data string font-scale scale spread ttf))
           (%pack (multiple-value-list
                   (binpack:auto-pack
                    (loop for g in glyph-data
                          for sdf = (getf g :sdf)
                          collect (binpack:rect g 0 0
                                                (array-dimension sdf 1)
                                                (array-dimension sdf 0)))
                    :width width :height height
                    :auto-size-granularity-x auto-size-granularity-x
                    :auto-size-granularity-y auto-size-granularity-y
                    :optimize-pack optimize-pack
                    :expand-mode expand-mode)))
           (pack (car %pack))
           (dims (cdr %pack)))
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
       (let* ((out (aa-misc:make-image width  ;(first dims)
                                       height ;(second dims)
                                       #(0 0 0))))
         (loop for rect in pack
               do (binpack:with-rect (g x y w h) rect
                    (with-glyph-data (glyph metrics sdf padding) g
                      (setf (glyph-bounding-box metrics)
                            (list x y (+ x w) (+ y h)))
                      (loop for ox from x
                            for ix below w
                            do (loop for oy from y
                                     for iy below h
                                     do (loop for i below 3
                                              do (setf (aref out oy ox i)
                                                       (aref sdf iy ix i))))))))
         (%make-atlas *backend* spread out (make-metrics pixel-size glyph-data font-scale ttf)))))))

(defun save-atlas (atlas png-filename)
  (opticl:write-image-file png-filename (atlas-image atlas)))
