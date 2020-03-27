(asdf:defsystem :sdf
  :description "Signed distance field glyph atlas generator"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>, Pavel Korolev <dev@borodust.org>"
  :license "MIT"
  :depends-on (zpb-ttf cl-vectors cl-paths cl-aa cl-aa-misc cl-paths-ttf opticl
                       binpack)
  :serial t
  :components ((:file "packages")
               (:file "metrics")
               (:file "v2")
               (:file "sdf-direct")
               (:file "sdf-ms")
               (:file "msdf")
               (:file "sdf")))

(asdf:defsystem :sdf/bmfont
  :description "Convert an SDF atlas to a bmfont structure"
  :version "0.0.1"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "MIT"
  :depends-on (3b-bmfont sdf pathname-utils)
  :serial t
  :components ((:file "bmfont")))
