#++
(ql:quickload '(zpb-ttf sdf 3b-glim 3b-glim-ui sdf/test))
(defpackage #:cleaner-vis
  (:use :cl #:sdf/base)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:u #:3b-glim-ui)
                    (#:glim #:3b-glim/s)
                    (#:i #:sdf/cleaner)
                    (#:g #:sdf/base)
                    (#:rb #:sdf/rb)
                    (#:q #:sdf/f+f-pqueue)))
(in-package #:cleaner-vis)

(defclass foo (u:window u::bg-star)
  ())

(defclass bez (u::bezier)
  ((c :initform (let ((a (+ 0.5 (random 0.5))))
                  (list a (+ 0.5(random 1.0)) (+ 0.5 (random 0.5)) 1))
      :reader c)))

(defvar *txt* nil)
(defvar *g* nil)
(defvar *render* t)
(defvar *sdf* nil)
(defvar *sdf-spread* 4)
(defvar *sdf-scale* nil)
(defparameter *ref* nil)

(defun remove-texture ()
  (when (and *ref* (car *ref*))
    (gl:delete-texture (shiftf (car *ref*) nil))
    (setf *ref* nil)))

(defun update-texture/a (a)
  (remove-texture)
  (gl:pixel-store :unpack-alignment 1)
  (destructuring-bind (wy wx wc)
      (array-dimensions a)
    (let* ((tex (gl:create-texture :texture-2d)))
      (gl:texture-storage-2d tex 1 :rgba8 wx wy)
      (static-vectors:with-static-vector (s (* wx wy wc)
                                            :element-type '(unsigned-byte 8))
        (loop for sy below wy
              for dy = (* (- wy sy 1) wx wc)
              for r = (array-row-major-index a sy 0 0)
              do (loop for i below (* wx wc)
                       do (setf (aref s (+ dy i))
                                (row-major-aref a (+ r i)))))
        (format t "dims2 ~s ~s ~s~%" wx wy wc)
        (gl:texture-sub-image-2d tex 0 0 0 wx wy
                                 (ecase wc (1 :alpha) (2 :rg) (3 :rgb) (4 :rgba))
                                 :unsigned-byte
                                 (static-vectors:static-vector-pointer s)))
      (gl:texture-parameter tex :texture-min-filter :nearest)
      (gl:texture-parameter tex :texture-mag-filter :nearest)
      (gl:texture-parameter tex :texture-wrap-s :clamp-to-edge)
      (gl:texture-parameter tex :texture-wrap-t :clamp-to-edge)
      (setf *ref* (list tex wx wy)))))

(defun scale-sdf (sdf)
  (flet ((s (x)
           (max 0
                (min 255
                     (round (+ 128 (* 127 (/ x *sdf-spread*))))))))
    (destructuring-bind (h w &optional channels)
        (array-dimensions (g::image sdf))
      (let* ((wh (max w h))
             (out (make-array (list wh wh channels)
                              :element-type '(unsigned-byte 8)
                              :initial-element 0))
             (x (floor (- wh w) 2))
             (y (floor (- wh h) 2)))
        (format t "dims ~s ~s ~s~%" w h channels)
        (loop with si of-type (simple-array single-float (* * *))
                = (g::image sdf)
              for ox from x
              for ix below w
              do (loop for oy from y
                       for iy below h
                       do (loop for i below channels
                                do (setf (aref out oy ox i)
                                         (s (aref (g::image sdf)
                                                  (- h iy 1)
                                                  ix i))))))
        out)))
  )

(defun show (s &key verbose filter (render *render*) (sdf *sdf*))
  (setf *txt* nil)
  (setf *g* nil)
  (when s
    (let* ((s2 (sdf/base::clean-shape s))
           (events (i::make-events s2))
           (ne (q:size events))
           (sweep (i::make-sweep))
           (ns (length (rb::to-list (i::rb sweep))))
           (intersect (i::update-sweep sweep events :verbose verbose))
           (g nil))
      (when filter
        (setf intersect
              (loop for i in intersect
                    for (x y l r . more) = i
                    for le = (i::edge l)
                    for re = (when r (i::edge r))
                    when (and re
                              (or more
                                  (not (or (eql re (g::next2 s le))
                                           (eql re (g::prev2 s le))
                                           (eql re le)))))
                      collect i
                      and
                        do (when verbose
                             (format t "l = ~s~% ~s~%" l le)
                             (format t "r = ~s~% ~s~%" r re)
                             (loop for m in more
                                   do (format t " + ~s~% ~s~%"
                                              m (i::edge m)))
                             (format t " n ~s~% p ~s~%"
                                     (g::next2 s le) (g::prev2 s le))
                             (format t "-- ~s ~s ~s~%"
                                     (eql re (g::next2 s le))
                                     (eql re (g::prev2 s le))
                                     (eql re le))))))
      (flet ((s-to-graph (s)
               (let ((g nil))
                 (g::map-contour-segments
                  s (lambda (c n end)
                      (declare (ignore c end))
                      (etypecase n
                        (g::point)
                        (g::segment
                         (push
                          (make-instance
                           'bez :coefs (u::v2s (g::s-dx1 n) (g::s-dy1 n)
                                               (g::s-dx2 n) (g::s-dy2 n)))
                          g))
                        (g::bezier2
                         (push
                          (make-instance
                           'bez :coefs (u::v2s (g::b2-dx1 n) (g::b2-dy1 n)
                                               (g::b2-dxc n) (g::b2-dyc n)
                                               (g::b2-dx2 n) (g::b2-dy2 n)))
                          g)))))
                 g)))
        (setf g (s-to-graph s))
        (when intersect
          (push (make-instance 'u::points
                               :points (apply #'u::v2s
                                              (loop for (x y) in intersect
                                                    collect x collect y)))
                g))
        (let ((final (sdf/base::%edit-shape-to-shape
                      (i::finished-contours sweep))))
          (setf *txt*
                (format nil "~s events -> ~s~% ~s sweep -> ~s~% = ~s~%~s -> ~s contours~%~s -> ~s nodes~%"
                        ne (q:size events) ns (length (rb::to-list (i::rb sweep)))
                        (length intersect)
                        (length (g::contours s))
                        (length (g::contours final))
                        (hash-table-count (g::%prev s))
                        (hash-table-count (g::%prev final))))
          #++(Sdf/Base::%print-contours (sdf/base::contours (sdf/base::shape-to-edit-shape s)))
          #++(break "cd" s)
          (cond
            ((i::finished-contours sweep)
             (when render
               (multiple-value-bind (r1 r2)
                   (i::draw-shapes s final :size 256)
                 (let* ((c (i::compare r1 r2))
                        (d (i::diff r1 r2)))
                   (setf *txt* (format nil "~adiff = ~a~%" *txt* c))
                   (update-texture/a d))))
             (when sdf
               (let* ((scale (/ (max (g::aabb-wx (g::bounding-box final))
                                     (g::aabb-wy (g::bounding-box final)))
                                64))
                      (s (g::make-sdf :mtsdf (sdf/cleaner::fix-shape s)
                                      :spread *sdf-spread*
                                      :scale (or *sdf-scale* scale))))
                 (format t "sdf = ~s ~s~%" sdf s)
                 (update-texture/a (scale-sdf s))
                 (gl:texture-parameter (car *ref*) :texture-min-filter :linear)
                 (gl:texture-parameter (car *ref*) :texture-mag-filter :linear)))
             (when verbose
               (format t "got contours:~%")
               (Sdf/Base::%print-contours (i::finished-contours sweep)))
             (setf *g* (list (make-instance 'u::graph :graphs g :read-only nil)
                             (make-instance
                              'u::graph
                              :graphs (s-to-graph final)
                              :read-only nil)))
             (loop for i in *g*
                   do (u::fit-graph-to-handles i :expand 1.1)))
            (t
             (remove-texture)
             (setf *g* (make-instance 'u::graph :graphs g :Read-only nil))
             (u::fit-graph-to-handles *g* :expand 1.1))))))))

(defvar *r* (make-random-state))
(defmethod u::plot :around ((p bez) plotter subdivide)
  (apply #'u::color (c p))
  (call-next-method)
  #++(let* ((tx u:*theme*)
            (u:*theme* (lambda (k)
                         (format t "lookup ~s~%" k)
                         (if (or t (eql k :border))
                             (list 1 (random 1.0) (random 1.0) 1)
                             (let ((u:*theme* tx))
                               (u::theme-color k))))))
       (call-next-method)))
(defvar *n* 0)
(defvar *s* nil)
(pop *s*)
#++
(loop with q = (i::make-events (second (first *s*)))
      when (q:dequeue q) collect it
        until (zerop (q:size q)))
(defvar *slow* t)
(defvar *a* nil)
(defvar *b* nil)
(defvar *oa* nil)
(defvar *ob* nil)

(defclass btan (bez)
  ((d :initform (* 0.125 (1+ (random 3))) :accessor d)))
(defmethod u::plot :after ((p btan) plotter subdivide)
  (u::with-plotter (plotter)
    (glim:with-draw (:lines :shader 'u::solid)
      (flet ((v (x1 y1 x2 y2 &optional (n 1))
               (u::vertex (u::x->c x1) (+ 10 (u::y->c y1)
                                          (+ (* 1 n) (d p))))
               (u::vertex (u::x->c x2) (+ 10 (u::y->c y2)
                                          (+ (* 1 n) (d p)))))
             (d (x) (coerce x 'double-float))
             (dydx (at dt)
               (let* ((xy1 (u::eval-bezier p at))
                      (xy2 (u::eval-bezier p (+ at dt)))
                      (dx (- (aref xy2 0) (aref xy1 0)))
                      (dy (- (aref xy2 1) (aref xy1 1))))
                 (if (zerop dx)
                     (random 10.0)
                     (/ dy dx))))
             (d2ydx (at dt)
               (let* ((xy1 (u::eval-bezier p at))
                      (xy2 (u::eval-bezier p (+ at dt)))
                      (xy3 (u::eval-bezier p (+ at dt dt)))
                      (dx1 (- (aref xy2 0) (aref xy1 0)))
                      (dy1 (- (aref xy2 1) (aref xy1 1)))
                      (dx2 (- (aref xy3 0) (aref xy2 0)))
                      (dy2 (- (aref xy3 1) (aref xy2 1)))
                                        ;(dx3 (- (aref xy3 0) (aref xy1 0)))
                      )
                 (if (or (zerop dx1) (zerop dx2))
                     (random 1.0)
                     (/ (- (* dx2 dy1) (* dx1 dy2)) (* dx1 dx1 dx2))))))
        #++(let* ((x1 (u::x (aref (u::coefs p) 0)))
                  (y1 (u::y (aref (u::coefs p) 0)))
                  (xc (u::x (aref (u::coefs p) 1)))
                  (yc (u::y (aref (u::coefs p) 1)))
                  (x2 (u::x (aref (u::coefs p) 2)))
                  (y2 (u::y (aref (u::coefs p) 2)))
                  (x3 (- xc x1))
                  (y3 (- yc y1))
                  (x4 (- x2 xc))
                  (y4 (- y2 yc))
                  (x5 (- x4 x3))
                  (y5 (- y4 y3)))
             (v x1 y1
                (- x1 (if (zerop y3)
                          (/ x3 y5)
                          (/ x3 y3)))
                y1))
        (let* ((x1 (d (u::x (aref (u::coefs p) 0))))
               (y1 (d (u::y (aref (u::coefs p) 0))))
               (xc (d (u::x (aref (u::coefs p) 1))))
               (yc (d (u::y (aref (u::coefs p) 1))))
               (x2 (d (u::x (aref (u::coefs p) 2))))
               (y2 (d (u::y (aref (u::coefs p) 2))))
               (at 0d0))
          (let* (;; coefficients of 1st derivative, = linear bezier
                 (x3 (* 2 (- xc x1)))
                 (y3 (* 2 (- yc y1)))
                 (x4 (* 2 (- x2 xc)))
                 (y4 (* 2 (- y2 yc)))
                 ;; 1st derivative with respect to t
                 (dx (a:lerp at x3 x4))
                 (dy (a:lerp at y3 y4))
                 #++(l (sqrt (+ (expt dx 2) (expt dy 2))))
                 ;; normal/tangent
                 #++(tangent (g:v2n (g:v2 dx dy)))
                 #++(normal (g:v2n (g:v2 (- dy) dx)))
                 ;; 2nd derivative with respect to t
                 (d2x (- x4 x3))
                 (d2y (- y4 y3))
                 (d2 (g:v2 d2x d2y))
                 ;; derivatives in space defined by tan/normal
                 #++(dt 1)
                 #++(dn 0)
                 #++(d2t (g:v2. d2 tangent))
                 #++(d2n (g:v2. d2 normal))
                 #++(l2 (sqrt (+ (expt dx 2) (expt dy 2))))
                 ;; d²x/dy²
                 #++(d2x/dy2 ;; (actually d2y/dx2?)
                      (cond
                        #++((zerop d2x)
                            ;; linear in x
                            )
                        (t
                         (+ (/ d2x (expt dy 2))
                            (/ dx d2y))))


                      #++(if (zerop d2x)
                             ;;
                             (- (+ (/ d2x (expt dy 2))
                                   (/ dx d2y)))
                             (+ (/ d2y (expt dx 2))
                                (/ dy d2x))))
                 (d2x/dy2 ;; (actually d2y/dx2?)
                   (cond
                     ((zerop dy)
                      ;; if horizontal, use d2y/dx2
                      (+ (/ d2y (expt dx 2))
                         (/ dy d2x)))
                     (t
                      (+ (/ d2x (expt dy 2))
                         (/ dx d2y))))


                   #++(if (zerop d2x)
                          ;;
                          (- (+ (/ d2x (expt dy 2))
                                (/ dx d2y)))
                          (+ (/ d2y (expt dx 2))
                             (/ dy d2x))))
                 (k ;; curvature
                   (/ (- (* dx d2y)
                         (* dy d2x))
                      (expt (+ (expt dx 2)
                               (expt dy 2))
                            3/2)))
                 #++
                 (d2x/dy2
                   (+ (/ d2n (expt dt 2))
                      (/ dn d2t))))
            #++(format t "dy=~s,dx=~s~%d2y=~s,d2x=~s~% = ~s~%"
                       dy dx d2y d2x d2x/dy2)
            ;; d2x/dy2, approximation of d2x/dy2
            (v x1 y1 (+ x1 d2x/dy2) y1)
            (v x1 y1 (+ x1 (d2ydx 0d0 0.0001d0)) y1 4)
            ;; dydx, approximation of dy/dx
            (v x1 y1 (+ x1 (if (zerop dx) (random 1.0) (/ dy dx))) y1 10)
            (v x1 y1 (+ x1 (dydx 0d0 0.0001d0)) y1 14)
            ;; curvature
            (v x1 y1 (+ x1 k) y1 18)
            #++(v x1 y1 (+ x1 (* dx d2y)) y1 20)
            #++(v x1 y1 (+ x1 (expt (+ (expt dx 2) (expt dy 2)) 3/2))
                  y1 22)
            ;; dx/dt, dy/dt
            (v x1 y1 (+ x1 dx) y1 25)
            (v x1 y1 (+ x1 dy) y1 28)
            (v x1 y1 (+ x1 d2x) y1 32)
            (v x1 y1 (+ x1 d2y) y1 35)
            ;; update circle
            (let ((r (abs (/ k))))
              (when (and *oa* (eql p *a*) (not (zerop k)))
                (setf (u::radius *oa*) r)
                (setf (aref (u::center *oa*) 1) (- 1 r)))
              (when (and *ob* (eql p *b*) (not (zerop k)))
                (setf (u::radius *ob*) r)
                (setf (aref (u::center *ob*) 1) (- 1 r))))
            ;; d
            #++
            (unless (Zerop (aref tangent 1))
              (v x1 y1 (+ x1 (i::%div dx l)
                          #++(/ (aref tangent 0) (aref tangent 1)))
                 y1 3))
            (let ((d (g::v2n d2)))
              (v 1 2 (+ 1 (aref d 0)) (+ 2 (aref d 1))))))))))

(defvar *g2* nil)
(defvar *g3* nil)

(setf *g2* (make-instance
            'u::graph
            :graphs (list
                     (setf *a*
                           (make-instance 'btan :coefs (u::v2s 0 1 1 1 1 0)))
                     (setf *b*
                           (make-instance 'btan :coefs (u::v2s 0 1 1 1 1 0)))
                     (setf *oa*
                           (make-instance 'u::circle :radius 1d0 :center #(0d0 0d0)))
                     (setf *ob*
                           (make-instance 'u::circle :radius 1d0 :center #(0d0 0d0)))))
      *g3* (make-instance
            'u::graph :graphs (list *a* *b*)
            :bookmarks '((1000000 0 1))
            :read-only t :extra-y-scale 10))
(setf (slot-value *a* 'c) '(1 0 1 1))
(setf (slot-value *b* 'c) '(0 1 0 1))
(setf (d *a*) 0)
(setf (d *b*) 40)
(setf (aref (aref (u::coefs *b*) 0) 1) 1)
(setf (aref (aref (u::coefs *b*) 2) 0) 2)
(setf (u::extra-y-scale *g3*) 1000)
(push (make-instance 'u::bezier :coefs (u::v2s 0 0 1 1))
      (cddr (u::graphs *g2*)))
(pop (cddr (u::graphs *g3*)))
(setf *g2* (make-instance
            'u::graph
            :graphs (list
                     (setf *a*
                           #++(make-instance
                            'u::bezier :coefs (u::v2s 0 0 1 0 1 1))
                           (make-instance
                            'u::bezier :coefs (u::v2s 0 0 1 0 1 1)))
                     (setf *b*
                           (make-instance
                            'u::bezier :coefs (u::v2s 0 0 2 0 1 1))
                           #++(make-instance
                            'u::bezier :coefs (u::v2s
                                               0 0
                                               1.3750000000000038d0 0.0d0
                                               0.09375000000003611d0 0.4999999999999598d0))))))
(setf *g2* nil)
(defvar *ok* nil)
(defvar *tab* 0)
(defvar *show-g2* nil)

(defun add-font (f)
  (setf i::*shapes* nil)
  (zpb-ttf:with-font-loader (ttf f)
    (loop with cc = (zpb-ttf:glyph-count ttf)
          for i below cc
          do (with-simple-restart (continue "skip ~s" i)
               (unwind-protect
                    (let* ((g (zpb-ttf:index-glyph i ttf))
                           (s (sdf/ttf::shape-from-glyph g)))
                      (unless (zerop (length (sdf/base::contours s)))
                        (if (ignore-errors
                             (sdf/cleaner::fix-shape s))
                            (sb-ext:atomic-push
                             (list 0 s 0 0 f i
                                   (zpb-ttf:postscript-name g)
                                   (let ((c (zpb-ttf:code-point g)))
                                     (when c (code-char c))))
                             sdf/cleaner::*shapes*)
                            (sb-ext:atomic-push
                             (list 0 s 0 0 f i
                                   (zpb-ttf:postscript-name g)
                                   (let ((c (zpb-ttf:code-point g)))
                                     (when c (code-char c))))
                             sdf/cleaner::*error-shapes*))))
                 (format t "~&==~s / ~s==~%" i cc)))))
  )
#++
(add-font "c:/windows/fonts/comic.ttf")
#++
(add-font "d:/dl/fonts/JuliaMono-ttf/JuliaMono-Regular.ttf")
#++
(add-font #P"d:/tmp/ttf/ttf/01c8a011f5e8a37f6a6947f8bde1154cd34d7eca7274e14f7f0872c0b04feb83/DejaVuSansCondensed-Bold.ttf")
(defmethod u::draw-ui ((w foo) now)
  (let ((u:*theme* u::*dark-theme*)
        (*r* (make-random-state *r*))
        (u::*text-scale* 6/8))
    (u:row
      #++(when *g2*
           (let ((u::*show-handles* t))
             (setf (u::y (elt (u::coefs *a*) 1)) 1
                   (u::y (elt (u::coefs *b*) 1)) 1)
             (u::column
               (u::row
                 (u::set-height 2/3)
                 (u::draw-ui *g2* now))
               (u::draw-ui *g3* now))))
      (u:window (:wx 1/4)
        (when (u::button (if *slow* "SLOW!" "slow?"))
          (setf *slow* (not *slow*)))
        (when (u::button (format nil "show handles ~s" u::*show-handles*))
          (setf u::*show-handles* (not u::*show-handles*)))
        (when *slow* (sleep 0.1))
        (u:row
          (when (u::button (format nil "start (~s)"(length i::*shapes*)))
            (setf *s* i::*shapes*))
          (when (u::button "shuffle")
            (setf *s* (a:shuffle (copy-seq i::*shapes*))))
          (when (u::button "sort")
            (setf *s* (sort (copy-seq i::*shapes*) '> :key
                            (lambda (a) (or (third a) 0))
                            #++ 'third))))
        (when (u::button (format nil "start/err (~s)"
                                 (length i::*error-shapes*)))
          (setf *s* i::*error-shapes*))
        (u:row
          (when (u::button (format nil "next (~s)" (length *s*)))
            (let ((ok *ok*))
              (setf *ok* nil)
              (when (and ok (or (typep *g* 'u::graph)
                                (and (consp *g*) (typep (car *g*) 'u::graph))))
                (pop *s*))
              (when *s*
                (format t "~&load ~s~%   (~a)~%" (cddddr (car *s*))
                        (third (car *s*)))
                (time (show (second (car *s*)) :verbose (not ok) :filter t))
                (setf *ok* t))))
          (u::disable (not *s*)
            (when (u::button (format nil "rq"))
              (time (show (second (car *s*)) :verbose nil :filter nil))))
          (u::disable (not *s*)
            (when (u::button (format nil "retry"))
              (time (show (second (car *s*)) :verbose t :filter nil))))
          (u::disable (not *s*)
            (when (u::button (format nil "re/filter"))
              (time (show (second (car *s*)) :verbose t :filter t)))))
        (u::disable (not *s*)
          (when (u::button "reload from file")
            (let ((f (fifth (car *s*)))
                  (n (sixth (car *s*))))
              (when (and f n)
                (zpb-ttf:with-font-loader (l f)
                  (let* ((g (zpb-ttf:index-glyph n l))
                         (s (sdf/ttf::shape-from-glyph g)))
                    (show s :filter t))))))
          (when (u::button "print shape")
            (let ((f (fifth (car *s*)))
                  (n (sixth (car *s*))))
              (when (and f n)
                (zpb-ttf:with-font-loader (l f)
                  (let* ((g (zpb-ttf:index-glyph n l))
                         (s (sdf/ttf::shape-from-glyph g)
                            #++(sdf/base::clean-shape
                                (sdf/ttf::shape-from-glyph g))))
                    (map 'nil
                         (lambda (c)
                           (format t "~&---~%")
                           (g::map-modifying-contour
                            c (lambda (n)
                                (etypecase n
                                  (g::es-contour-vertex
                                   (format t "~&[~s]"
                                           (g::p-rv (g::point n))))
                                  (g::es-contour-segment
                                   (format t "-s-"))
                                  (g::es-contour-bezier2
                                   (format t "<~s>"
                                           (g::p-rv (g::control-point n)))))
                                n)))
                         (g::contours
                          (g::shape-to-edit-shape s)))))))))
        (when (u::button (format nil "next 9 (~s)" (length *s*)))
          (when (typep *g* 'function)
            (setf *s* (nthcdr 9 *s*)))

          (let ((a (time
                    (loop for i in *s*
                          repeat 9 while i
                          do (show (second i) :filter t)
                          collect *g*))))
            (setf *g* (lambda (n)
                        (flet ((n (x)
                                 (let ((g (nth x a)))
                                   (if (consp g)
                                       (nth (mod *tab* (length g)) g)
                                       g))))
                          (u:column
                            (u:row
                              (u:set-height 1/3)
                              (u:column (u:set-width 1/3)
                                (u:draw-ui (n 0) n))
                              (u:column (u:set-width 1/2)
                                (u:draw-ui (n 1) n))
                              (u:draw-ui (n 2) n))
                            (u:row
                              (u:set-height 1/2)
                              (u:column (u:set-width 1/3)
                                (u:draw-ui (n 3) n))
                              (u:column (u:set-width 1/2)
                                (u:draw-ui (n 4) n))
                              (u:draw-ui (n 5) n))
                            (u:row
                              (u:column (u:set-width 1/3)
                                (u:draw-ui (n 6) n))
                              (u:column (u:set-width 1/2)
                                (u:draw-ui (n 7) n))
                              (u:draw-ui (n 8) n))))))))
        (when (or (consp *g*)
                  (and (typep *g* 'function)
                       (consp (car *s*))))
          (u:row
            (loop for i below (if (consp *g*)
                                  (length *g*)
                                  2)
                  do (when (u::button (if (= i *tab*)
                                          (format nil "[~a]" i)
                                          (format nil "~a" i)))
                       (setf *tab* i)))))
        (when *g2*
          (when (u::button (if *show-g2* "[*g2*]" "*g2*"))
            (setf *show-g2* (not *show-g2*))))
        (u:row
          (when (u::button (if *render* "[diff]" "diff"))
            (setf *render* (not *render*))
            (setf *sdf* nil))
          (flet ((b (l)
                   (when (u::button (if (eql *sdf* l)
                                        (format nil "[~(~a~)]" l)
                                        (format nil "~(~a~)" l)))
                     (setf *render* nil
                           *sdf* (if (eql *sdf* l) nil l)))))
            (b :msdf)
            (b :sdf/a)
            (b :rgb)
            (b :a)))
        #++(sleep 0.08)
        (u::text (format nil "~{~s~%~}" (cddddr (car *s*))))
        (when *txt*
          (u::text *txt*)))
      (when (and *ref* (or *render* *sdf*))
        (flet ((q (x1 y1 x2 y2)
                 (u:color 1 1 1 1)
                 (u:uv 0 0) (u:vertex x1 y2)
                 (u:uv 0 1) (u:vertex x1 y1)
                 (u:uv 1 1) (u:vertex x2 y1)
                 (u:uv 1 0) (u:vertex x2 y2)))
          (glim:with-pushed-matrix (:modelview)
                                        ;(glim:scale 1 1 0)
                                        ;(glim:uniform 'u::modelview (glim:ensure-matrix :modelview))
            (3b-glim-ui:dispatch-draws w)
            (gl:bind-texture :texture-2d (car *ref*))
            (glim:uniform 'u::tex 0)
            (let* ((ww (min (u::pwx) (u::pwy)))
                   (x1 (- (+ (u::pcx) (u::pwx))
                          (* 2 ww)))
                   (x2 (+ x1 ww))
                   (y1 (+ (u::pcy) (- (u::pwy) ww)))
                   (y2 (+ y1 ww)))
              (cond
                (*render*
                 (glim:with-draw (:quads :shader 'u::sdf)
                   (q x1 y1 x2 y2)))
                ((eql *sdf* :msdf)
                 (glim:uniform 'u::expand 0)
                 (glim:uniform 'u::mode 0)
                 (glim:with-draw (:quads :shader 'u::sdf)
                   (q x1 y1 x2 y2)))
                ((eql *sdf* :sdf/a)
                 (glim:uniform 'u::expand 0)
                 (glim:uniform 'u::mode 1)
                 (glim:with-draw (:quads :shader 'u::sdf)
                   (q x1 y1 x2 y2)))
                ((eql *sdf* :rgb)
                 (glim:uniform 'u::expand 0)
                 (glim:uniform 'u::mode 3)
                 (glim:with-draw (:quads :shader 'u::sdf)
                   (q x1 y1 x2 y2)))
                ((eql *sdf* :a)
                 (glim:uniform 'u::expand 0)
                 (glim:uniform 'u::mode 4)
                 (glim:with-draw (:quads :shader 'u::sdf)
                   (q x1 y1 x2 y2)))))
            (3b-glim-ui:dispatch-draws w)
            (glim:uniform 'u::mode 0))))
      (if (and *g2* *show-g2*)
          (u::draw-ui *g2* now)
          (when *g*
            (if (consp *g*)
                (u::draw-ui (nth *tab* *g*) now)
                (u::draw-ui *g* now)))))))

#++
(ql:quickload '(3b-glim 3b-glim-ui sdf))
#++
(u:run 'foo :font "d:/tmp/arial32-5.json")
#++
(u:run 'foo :font "d:/tmp/OpenDyslexic-Regular32-4.json")
#++
(glut:show-window)
#++
(loop for i across glut::*id->window*
      when i do (glut:destroy-window (glut::id i)))
