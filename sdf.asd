(asdf:defsystem :sdf/base
  :description "Signed distance field generator"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (cl-vectors cl-paths cl-aa cl-aa-misc parse-number
                          float-features)
  :serial t
  :components ((:file "packages-base")
               (:file "v2")
               (:file "geometry")
               (:file "quadratic-intersect-common")
               (:file "quadratic-intersect-geometry")
               (:file "quadratic-intersect")
               (:file "shape")
               (:file "edit-shape")
               (:file "shapedesc")
               (:file "shape-ops")
               (:file "sdf-base")
               (:file "edge-list")
               (:file "rb")
               (:file "df-queue")
               (:file "ff-queue")
               (:file "clean-shape")
               (:file "msdfec")
               (:file "msdf")
               (:file "sdf")))

(asdf:defsystem :sdf/ttf
  :description "utilities for SDF font generation"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (sdf/base zpb-ttf cl-paths-ttf)
  :serial t
  :components ((:file "packages-ttf")
               (:file "metrics")
               (:file "ttf")))

(asdf:defsystem :sdf
  :description "Signed distance field font glyph atlas generator."
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>, Pavel Korolev <dev@borodust.org>"
  :license "MIT"
  :depends-on (sdf/base sdf/ttf opticl binpack/2)
  :serial t
  :components ((:file "packages")
               (:file "api")))

(asdf:defsystem :sdf/bmfont
  :description "Convert an SDF atlas to a bmfont structure"
  :version "0.0.1"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "MIT"
  :depends-on (3b-bmfont sdf pathname-utils)
  :serial t
  :components ((:file "bmfont")))

(defsystem sdf/test
  :depends-on (sdf parachute md5 float-features)
  :serial t
  :perform
  (asdf:test-op (op c) (uiop:symbol-call :parachute :test :sdf/test))
  :components ((:file "test-package")
               ;; automated tests, (parachute:run :sdf/test)
               (:file "tests")
               (:file "edge-test")
               (:file "clean-regression")
               (:file "intersect-regression")

               ;; utilities/manual tests etc
               (:file "leak-check")
               (:file "clean-test")))
