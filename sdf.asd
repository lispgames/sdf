(asdf:defsystem :sdf
  :description "Signed distance field glyph atlas generator"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>, Pavel Korolev <dev@borodust.org>"
  :license "MIT"
  :depends-on (zpb-ttf cl-vectors cl-paths cl-aa cl-aa-misc cl-paths-ttf opticl)
  :serial t
  :components ((:file "packages")
               (:file "binpack")
               (:file "metrics")
               (:file "v2")
               (:file "sdf-direct")
               (:file "sdf-ms")
               (:file "sdf")))
