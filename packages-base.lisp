(defpackage #:sdf/base
  (:use :cl)
  (:local-nicknames (#:a #:alexandria-2)
                    #++(#:q #:damn-fast-updatable-priority-queue))
  (:export #:render-sdf
           #:v2
           #:vx
           #:vy
           #:v2-
           #:v2+
           #:v2h*
           #:v2x
           #:v2.
           #:v2dist
           #:v2mag
           #:v2scale
           #:v2n
           #:v2rx
           #:make-aabb
           #:update-aabb
           #:aabb-p1
           #:aabb-p2
           #:aabb-x1
           #:aabb-y1
           #:aabb-x2
           #:aabb-y2
           #:make-point
           #:p-v
           #:p-x
           #:p-y
           #:with-point
           #:point=
           #:make-segment/p
           #:make-segment
           #:%make-segment
           #:s-p1
           #:s-p2
           #:s-x1
           #:s-y1
           #:s-x2
           #:s-y2))

(defpackage #:sdf/quadratic-intersect/int
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:b #:sdf/base))
  (:import-from #:sdf/base #:vx #:vy))

(defpackage #:sdf/quadratic-intersect
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:b #:sdf/base)
                    (#:d #:sdf/quadratic-intersect/int))
  (:import-from #:sdf/base #:vx #:vy))
