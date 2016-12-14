(in-package :sdf)


(defstruct (font-metrics
             (:conc-name font-))
  (glyphs nil :read-only t))


(defstruct (glyph-metrics
             (:conc-name glyph-))
  (bounding-box nil :read-only t))


(defstruct (atlas
             (:constructor %make-atlas (image metrics)))
  (image nil :read-only t)
  (metrics nil :read-only t))
