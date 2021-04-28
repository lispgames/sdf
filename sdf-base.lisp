(in-package #:sdf/base)

;; metadata for an sdf render

(defclass sdf ()
  ;; includes bounding box and vector data for shape (coordinate
  ;; system is assumed to be Y-up)
  ((shape :initarg :shape :reader shape)
   (cleaned-shape :initarg :cleaned-shape :initform nil
                  :accessor %cleaned-shape)
   ;; :sdf, :psdf, :msdf, :mtsdf, :smsdf, :smtsdf, etc
   (sdf-type :initarg :sdf-type :reader sdf-type)
   ;; 2d or 3d array of pixel values after sdf is generated. 0,0 is
   ;; top left of SDF image
   (image :reader image :initform nil :initarg :image)
   ;; x or y coordinates of samples in image, as vector of real
   ;; (stored separately to make sure we always get same results in
   ;; various steps, and to allow calculating it with rationals to
   ;; reduce fp loss)
   (samples/x :reader samples/x :initform nil :initarg :samples/x)
   (samples/y :reader samples/y :initform nil :initarg :samples/y)

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

(defmethod cleaned-shape ((sdf sdf))
  (or (%cleaned-shape sdf)
      (setf (%cleaned-shape sdf)
            (clean-shape (shape sdf)))))
