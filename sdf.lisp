(in-package #:sdf)

(defparameter *default-characters*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.,;:?!@#$%^&*()-_<>'\"[]/\\| ")


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

(defun make-metrics (glyph-data scale ttf)
  (make-font-metrics :glyphs (mapcar (lambda (g) (getf g :metrics)) glyph-data)
                     :ascender (* scale (zpb-ttf:ascender ttf))
                     :descender (* scale (zpb-ttf:descender ttf))
                     :line-gap (* scale (zpb-ttf:line-gap ttf))
                     :kerning-table (make-kerning-table glyph-data scale ttf)))

(defvar *backend* :direct)

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
                     ((:direct :psdf)
                      (sdf ttf g font-scale scale spread))
                     (:ms
                      (sdf/ms ttf g font-scale scale spread))
                     (:msdf
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


(defun make-atlas (font-name pixel-size
                   &key (scale 8) (spread 0.1)
                     (string *default-characters*)
                     width height
                     ((:backend *backend*) :direct))
  (zpb-ttf:with-font-loader (ttf font-name)
    (let* ((font-height (- (zpb-ttf:ascender ttf)
                           (zpb-ttf:descender ttf)))
           (font-scale (/ pixel-size font-height))
           (glyph-data (obtain-glyph-data string font-scale scale spread ttf))
           (pack (binpack:pack
                  (loop for g in glyph-data
                        for sdf = (getf g :sdf)
                        collect (list g (array-dimension sdf 1) (array-dimension sdf 0)))
                  :width width
                  :height height))
           (dims (loop for (nil x y w h) in pack
                    maximize (+ x w) into width
                    maximize (+ y h) into height
                    finally (return (list width height)))))
        (time
         (let* ((out (aa-misc:make-image width;(first dims)
                                         (second dims) #(0 0 0))))
           (loop for (g x y w h) in pack
              do (with-glyph-data (glyph metrics sdf padding) g
                   (setf (glyph-bounding-box metrics) (list x y (+ x w) (+ y h)))
                     (loop for ox from x
                        for ix below w
                        do (loop for oy from y
                              for iy below h
                                 do (loop for i below 3
                                          do (setf (aref out oy ox i)
                                                   (aref sdf iy ix i)))))))
           (%make-atlas out (make-metrics glyph-data font-scale ttf)))))))

(defun save-atlas (atlas png-filename metrics-filename)
  (declare (ignore metrics-filename))
  (opticl:write-image-file png-filename (atlas-image atlas)))
