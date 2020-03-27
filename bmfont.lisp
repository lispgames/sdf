(defpackage #:sdf-bmfont
  (:use :cl)
  (:export #:to-bmfont
           #:save-bmfont
           #:create-bmfont))
(in-package #:sdf-bmfont)

(defun to-bmfont (atlas)
  (let ((metrics (sdf:atlas-metrics atlas))
        (chars (make-hash-table :test 'eql))
        (dims (array-dimensions (sdf:atlas-image atlas))))
    (destructuring-bind (height width channels) dims
      (loop for glyph in (sdf:font-glyphs metrics)
            for char = (sdf:glyph-character glyph)
            for (x y w h) = (sdf:glyph-bounding-box glyph)
            for (xo yo) = (sdf:glyph-origin glyph)
            for i from 0
            do (setf (gethash char chars)
                     (list :id (char-code char)
                           :index i
                           :char (string char)
                           :x x
                           :y y
                           :width w
                           :height h
                           :xoffset xo
                           :yoffset yo
                           :xadvance (sdf:glyph-advance-width glyph)
                           :chnl (ecase channels (1 4) (2 6) (3 7) (4 15))
                           :page 0)))
      (make-instance '3b-bmfont-common:bmfont
                     :face NIL
                     :size (sdf:font-size metrics)
                     :padding '(0 0 0 0)
                     :spacing '(0 0)
                     :base (sdf:font-ascender metrics)
                     :line-height (+ (sdf:font-ascender metrics) (sdf:font-descender metrics) (sdf:font-line-gap metrics))
                     :stretch-h 100
                     :scale-w width
                     :scale-h height
                     :red-chnl (if (= 1 channels) :glyph :zero)
                     :green-chnl (if (= 2 channels) :glyph :zero)
                     :blue-chnl (if (= 3 channels) :glyph :zero)
                     :alpha-chnl (if (= 4 channels) :glyph :zero)
                     :chars chars
                     :pages (make-array 1 :initial-element (list :id 0 :file NIL))
                     :kernings (sdf:font-kerning-table metrics)
                     :distance-field (list :field-type (sdf:atlas-field-type atlas)
                                           :distance-range (sdf:atlas-distance-range atlas))))))

(defun save-bmfont (atlas atlas-file bmfont-file &key type)
  (let ((bmfont (to-bmfont atlas))
        (relpath (namestring (pathname-utils:relative-pathname bmfont-file atlas-file))))
    (setf (3b-bmfont-common:face bmfont) relpath)
    (setf (getf (aref (3b-bmfont-common:pages bmfont) 0) :file) relpath)
    (sdf:save-atlas atlas atlas-file)
    (3b-bmfont:write-bmfont bmfont bmfont-file :type type)))

(defun create-bmfont (font-file bmfont-file &rest args &key (size 24) type &allow-other-keys)
  (let ((atlas-args (copy-list args)))
    (remf atlas-args :size)
    (remf atlas-args :type)
    (save-bmfont (apply #'sdf:make-atlas font-file size atlas-args)
                 (make-pathname :type "png" :defaults bmfont-file)
                 bmfont-file
                 :type type)))
