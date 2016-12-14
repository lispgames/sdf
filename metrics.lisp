(in-package :sdf)


(defstruct (font-metrics
             (:conc-name font-))
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


(defstruct (atlas
             (:constructor %make-atlas (image metrics)))
  (image nil :read-only t)
  (metrics nil :read-only t))
