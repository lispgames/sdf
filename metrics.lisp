(in-package :sdf/ttf)


(defstruct (font-metrics
            (:conc-name font-))
  (size nil :read-only t)
  (glyphs nil :read-only t)
  (ascender nil :read-only t)
  (descender nil :read-only t)
  (line-gap nil :read-only t)
  (kerning-table nil :read-only t))


(defstruct (glyph-metrics
             (:conc-name glyph-))
  (character nil :read-only t)
  (bounding-box nil)
  (origin nil :read-only t)
  (advance-width nil :read-only t)
  (left-side-bearing nil :read-only t)
  (right-side-bearing nil :read-only t))


(defstruct (atlas-padding (:constructor %make-atlas-padding))
  (up 0 :read-only t)
  (right 0 :read-only t)
  (down 0 :read-only t)
  (left 0 :read-only t))

(defun make-atlas-padding (&key up right down left (all 0))
  (%make-atlas-padding :up (or up all)
                       :right (or right all)
                       :down (or down all)
                       :left (or left all)))

(defun padding-list (ap)
  ;; return padding as list in order expected by bmfont format: (up,
  ;; right, down, left)."
  (list (atlas-padding-up ap)
        (atlas-padding-right ap)
        (atlas-padding-down ap)
        (atlas-padding-left ap)))

(defstruct (atlas
            (:constructor %make-atlas (field-type distance-range image metrics
                                                  padding)))
  (field-type nil :read-only t)
  (distance-range nil :read-only t)
  (image nil :read-only t)
  (metrics nil :read-only t)
  (padding (make-atlas-padding :all 0) :read-only t))
