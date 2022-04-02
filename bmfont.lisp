(defpackage #:sdf-bmfont
  (:use :cl)
  (:local-nicknames (#:st #:sdf/ttf))
  (:export #:to-bmfont
           #:save-bmfont
           #:create-bmfont))
(in-package #:sdf-bmfont)

(defun to-bmfont (atlas)
  (let* ((metrics (st::atlas-metrics atlas))
         (pad (st::atlas-padding atlas))
         (pad-left (st::atlas-padding-left pad))
         (pad-up (st::atlas-padding-up pad))
         (pad-down (st::atlas-padding-down pad))
         (pad-y (+ pad-up pad-down))
         (chars (make-hash-table :test 'eql))
         (dims (array-dimensions (st::atlas-image atlas)))
         (spread (st::atlas-distance-range atlas))
         (base (- (st::font-ascender metrics) pad-down)))
    (destructuring-bind (height width channels) dims
      (loop for glyph in (st::font-glyphs metrics)
            for char = (st::glyph-character glyph)
            for (x y x2 y2) = (st::glyph-bounding-box glyph)
            for w = (- x2 x)
            for h = (- y2 y)
            for (xo yo) = (st::glyph-origin glyph)
            for i from 0
            do (setf (gethash char chars)
                     (list :id (char-code char)
                           :index i
                           :char (string char)
                           :x x
                           :y y
                           :width w
                           :height h
                           :xoffset #++(+ (- (float xo)) spread)
                                    (- (float xo))
                           :yoffset (float (+ base
                                              (- yo h)
                                              spread))
                           :xadvance (st::glyph-advance-width glyph)
                           :chnl (ecase channels (1 4) (2 6) (3 7) (4 15))
                           :page 0)))
      (make-instance '3b-bmfont-common:bmfont
                     :face NIL
                     :size (st::font-size metrics)
                     :padding (st::padding-list pad)
                     :spacing '(0 0)
                     :base base
                     :line-height (+ (st::font-ascender metrics)
                                     (- (st::font-descender metrics))
                                     (st::font-line-gap metrics))
                     :stretch-h 100
                     :scale-w width
                     :scale-h height
                     :red-chnl (if (= 1 channels) :glyph :zero)
                     :green-chnl (if (= 2 channels) :glyph :zero)
                     :blue-chnl (if (= 3 channels) :glyph :zero)
                     :alpha-chnl (if (= 4 channels) :glyph :zero)
                     :chars chars
                     :pages (make-array 1 :initial-element (list :id 0 :file NIL))
                     :kernings (st::font-kerning-table metrics)
                     :distance-field (list :field-type (st::atlas-field-type atlas)
                                           :distance-range (* 2 (st::atlas-distance-range atlas)))))))

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
