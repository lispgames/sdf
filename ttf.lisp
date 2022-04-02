(in-package #:sdf/ttf)

(defun test-box (&key (x1 32) (y1 32) (wx 256) (wy 256))
  (sdf/base::with-shape-builder (b :metadata '(box))
    (sdf/base::start-contour x1 y1)
    (sdf/base::line-to x1 (+ y1 wy))
    (sdf/base::line-to (+ x1 wx) (+ y1 wy))
    (sdf/base::line-to (+ x1 wx) y1)
    (sdf/base::line-to x1 y1)
    (sdf/base::end-contour)))


(defvar *dump-ttf-points* nil)
(defun shape-from-glyph (glyph &key metadata)
  (flet ((xy (x) (when x (list (zpb-ttf:x x) (zpb-ttf:y x)))))
   (sdf/base::with-shape-builder (b :metadata metadata)
     (zpb-ttf:do-contours (c glyph)
       (let ((start t))
         (zpb-ttf:do-contour-segments (p0 p1 p2) c
           (when start
             (setf start nil)
             (sdf/base::start-contour (zpb-ttf:x p0) (zpb-ttf:y p0))
             (when *dump-ttf-points*
              (format t "start contour ~s~%" (xy p0))))
           (when *dump-ttf-points*
             (format t " [~s] ~s ~s~%" (xy p0) (xy p1) (xy p2)))
           (if p1
               (sdf/base::quadratic-to (zpb-ttf:x p1) (zpb-ttf:y p1)
                                       (zpb-ttf:x p2) (zpb-ttf:y p2))
               (sdf/base::line-to (zpb-ttf:x p2) (zpb-ttf:y p2))))
         (when *dump-ttf-points*
           (format t "end contour~%"))
         (sdf/base::end-contour))))))

