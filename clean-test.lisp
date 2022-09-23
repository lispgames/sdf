(in-package #:sdf/cleaner)

;; try to clean a shape, then render it and original shape to bitmap
;; and compare results.

(defun rasterize-shape (shape w h scale dx dy)
  (let* ((state (aa:make-state))
         (r (aa-misc:make-image w h #(255 255 255 255)))
         (put-pixel (aa-misc:image-put-pixel r #(0 0 0)))
         (lc -1)
         (path)
         (paths))
    (b::map-contour-segments
     shape
     (lambda (c n e)
       (labels ((s (x) (if scale (* scale x) x))
                (p (xy)
                  (let ((xy (b::p-dv xy)))
                    (paths:make-point (+ dx (s (b::vx xy)))
                                      (- h (+ dy (s (b::vy xy))) 1))))
                (start (p)
                  (when (/= lc c)
                    (setf lc c)
                    (setf path (paths:create-path :polygon))
                    (paths:path-reset path (p p)))))
         (etypecase n
           (b::point ;; do nothing
            )
           (b::segment
            (start (b::s-p1 n))
            (paths:path-extend path (paths:make-straight-line)
                               (p (b::s-p2 n))))
           (b::bezier2
            (start (b::b2-p1 n))
            (paths:path-extend path
                               (paths:make-bezier-curve (list (p (b::b2-c1 n))))
                               (p (b::b2-p2 n))))))
       (when e
         (push path paths))))
    (vectors:update-state state paths)
    (aa:cells-sweep state put-pixel)
    r))

(defun compare (a b)
  (assert (equalp (array-dimensions a) (array-dimensions b)))
  (let ((d 0))
    (opticl:do-pixels (i j) a
      (multiple-value-bind (ra ga ba) (opticl:pixel a j i)
        (multiple-value-bind (rb gb bb) (opticl:pixel b j i)
          (unless #++(and (= ra rb) (= ga gb) (= ba bb))
                  (and (< (abs (- ra rb)) 200)
                       (< (abs (- ga gb)) 200)
                       (< (abs (- ba bb)) 200))
                  (incf d)))))
    (if (zerop d) nil d)))

(defun diff (a b)
  (let ((d (make-array (array-dimensions a)
                       :element-type (array-element-type a))))
    (flet ((d3 (a b c d e f)
             (values (if (or (/= a d) (/= b e) (/= c f))
                         255 0)
                     a
                     0)))
      (opticl:do-pixels (i j) d
        (setf (opticl:pixel d j i)
              (multiple-value-call #'d3
                (opticl:pixel a j i)
                (opticl:pixel b j i)))))
    d))

(defun draw-shapes (shape done &key (pad 3) (size 128))
  (unless (or (zerop (length (b::contours shape)))
              (zerop (b::aabb-wx (b::bounding-box shape)))
              (zerop (b::aabb-wy (b::bounding-box shape))))
    (let* ((aabb (b::bounding-box shape))
           (x1 (b::aabb-x1 aabb))
           (y1 (b::aabb-y1 aabb))
           (wx (b::aabb-wx aabb))
           (wy (b::aabb-wy aabb))
           (scale (min (/ (- size (* 2 pad)) wx)
                       (/ (- size (* 2 pad)) wy)))
           (dx (- pad (* x1 scale)))
           (dy (- pad (* y1 scale))))
      (values (rasterize-shape shape size size scale dx dy)
              (rasterize-shape done size size scale dx dy)))))

(defun test-clean-shape (shape &key ((:check *check*) t) (size 128))
  ;; todo: switch this to use final api when there is one?
  (unless (or (zerop (length (b::contours shape)))
              (zerop (b::aabb-wx (b::bounding-box shape)))
              (zerop (b::aabb-wy (b::bounding-box shape))))
    (let* ((clean (b::clean-shape shape))
           (ev (make-events clean))
           (sweep (make-sweep)))
      (update-sweep sweep ev)
      (let* ((done (b::%edit-shape-to-shape (finished-contours sweep))))
        (multiple-value-bind (r1 r2)
            (draw-shapes shape done :size size)
          (let ((bad (compare r1 r2)))
            (when bad
              (b::%print-contours (b::contours (b::shape-to-edit-shape done)))
              (opticl:write-png-file "c:/tmp/sdf-clean-1.png" r1)
              (opticl:write-png-file "c:/tmp/sdf-clean-2.png" r2)
              (opticl:write-png-file "c:/tmp/sdf-clean-3.png" (diff r1 r2)))
            (assert (not bad))))
        done))))

(defun test-clean-font (path &key (size 128))
  (zpb-ttf:with-font-loader (ttf path)
    (loop for i below (zpb-ttf:glyph-count ttf)
          for g = (zpb-ttf:index-glyph i ttf)
          for s = (b::clean-shape
                   (sdf/ttf::shape-from-glyph g))
          do (unless (zerop (length (b::contours s)))
               (with-simple-restart (continue "skip")
                 (test-clean-shape s :size size))))))

#++
(time
 (test-clean-font #++ "c:/windows/fonts/bahnschrift.ttf"
                  #P"d:/dl/fonts/fa-brands-400.ttf"
                  #++ #P"d:/dl/fonts/Noto-unhinted-sans/NotoSansEthiopic-Regular.ttf" :size 64))

#++
(let ((img (rasterize-shape (b::parse-shape "{ 0,0;10,0;10,10;(5,5);0,10; #}") 32 32 2 5 5)))
  (loop for j below 32
        do (loop for i below 32
                 for r = (aref img j i 1)
                 do (format t "~a" (if (zerop r) "*" " ")))
           (terpri)))
