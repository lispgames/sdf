(in-package #:sdf/base)

;; metadata for an sdf render

(defclass sdf ()
  ;; includes bounding box and vector data for shape (coordinate
  ;; system is assumed to be Y-up)
  ((shape :initarg :shape :reader shape)
   ;; :sdf, :psdf, :msdf, :mtsdf, :smsdf, :smtsdf, etc
   (sdf-type :initarg :sdf-type :reader sdf-type)
   ;; 2d or 3d array of pixel values after sdf is generated. 0,0 is
   ;; top left of SDF image
   (image :reader image :initform nil :initarg :image)
   ;; 2d bit array of flags indicating if a particular sample is
   ;; inside the shape (1 = in)
   (signs :Reader signs :initform nil :initarg :signs)
   ;; size of pixel of IMAGE in shape coordinate system
   (pixel-scale :reader pixel-scale :initform (v2 1 1) :initarg :pixel-scale)
   ;; position of origin of SHAPE coordinate system relative to center
   ;; of upper left pixel of IMAGE, in pixels
   (origin :initform (v2 0 0) :reader origin :initarg :origin)
   ;; distance in pixels corresponding to a pixel value of 0 (=
   ;; maximum representable distance in distance field)
   (spread :initform 2.5 :reader spread :initarg :spread)))
