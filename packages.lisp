(defpackage #:sdf
  (:use :cl)
  (:export #:make-atlas
           #:save-atlas

           #:atlas-field-type
           #:atlas-distance-range
           #:atlas-image
           #:atlas-metrics

           #:font-size
           #:font-glyphs
           #:font-ascender
           #:font-descender
           #:font-line-gap
           #:font-glyphs
           #:font-kerning-table

           #:glyph-character
           #:glyph-advance-width
           #:glyph-left-side-bearing
           #:glyph-right-side-bearing
           #:glyph-origin
           #:glyph-bounding-box))
