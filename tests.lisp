#++(ql:quickload '(sdf/test))
(in-package sdf/test)

(defun v2= (b a)
  (and (typep b 'b:v2)
       (eql (aref a 0) (b:vx b))
       (eql (aref a 1) (b:vy b))))

(defun v2~ (b a eps-mul)
  (or (eql a b)
      (and (< (abs (- (aref a 0) (b:vx b))) (* eps-mul double-float-epsilon))
           (< (abs (- (aref a 1) (b:vy b))) (* eps-mul double-float-epsilon)))))

(define-test v2
  ;;(fail (b:v2))
  ;;(fail (b:v2 1))
  (of-type (simple-array double-float (2)) (b:v2 1 2))
  (of-type b:v2 (b:v2 1 2))
  (is eql 1d0 (b:vx (b:v2 1 2)))
  (is eql 2d0 (b:vy (b:v2 1 2)))
  ;;(fail (b:v2 1 2 3))
  (is v2= #(2d0 3d0) (b:v2- (b:v2 6 5) (b:v2 4 2)))
  (is v2= #(4d0 6d0) (b:v2+ (b:v2 1 2) (b:v2 3 4)))
  (is v2= #(8d0 15d0) (b:v2h* (b:v2 2 3) (b:v2 4 5)))
  (is eql -2d0 (b:v2x (b:v2 2 3) (b:v2 4 5)))
  (is eql 23d0 (b:v2. (b:v2 2 3) (b:v2 4 5)))
  (is eql 2d0 (b:v2dist (b:v2 2 3) (b:v2 4 3)))
  (is eql 3d0 (b:v2dist (b:v2 0 0) (b:v2 0 3)))
  (is eql 5d0 (b:v2dist (b:v2 2 3) (b:v2 5 7)))
  (is eql (sqrt 2d0) (b:v2mag (b:v2 1 1)))
  (is v2= #(6d0 9d0) (b:v2scale (b:v2 2 3) 3d0))
  (is v2= #(0.6d0 0.8d0) (b:v2n (b:v2 15 20)))
  (is v2= #(-4d0 3d0) (b:v2rx (b:v2 3 4)))
  )
#++
(test 'v2)


(defun a= (aa r)
  (and (typep aa 'b::aabb)
       (v2= (b:aabb-p1 aa) (first r))
       (v2= (b:aabb-p2 aa) (second r))))

(defun p= (p r)
  (v2= (b::p-dv p) r))

(defun s= (s r)
  (and (typep s 'b::segment)
       (p= (b:s-p1 s) (first r))
       (p= (b:s-p2 s) (second r))))

(define-test geom
  (let ((a (finish (b:make-aabb 0 0 0 0))))
    (is a= '(#(0d0 0d0) #(0d0 0d0)) a)
    (finish (b:update-aabb a -1 0))
    (is a= '(#(-1d0 0d0) #(0d0 0d0)) a)
    (finish (b:update-aabb a 1 -2))
    (is a= '(#(-1d0 -2d0) #(1d0 0d0)) a)
    (finish (b:update-aabb a -2 2))
    (is a= '(#(-2d0 -2d0) #(1d0 2d0)) a)
    (finish (b:update-aabb a 0 0))
    (is a= '(#(-2d0 -2d0) #(1d0 2d0)) a)
    (finish (b:update-aabb a -3 -4))
    (is a= '(#(-3d0 -4d0) #(1d0 2d0)) a)
    (is eql -3d0 (b:aabb-x1 a))
    (is eql -4d0 (b:aabb-y1 a))
    (is eql 1d0 (b:aabb-x2 a))
    (is eql 2d0 (b:aabb-y2 a)))

  (is p= #(0d0 0d0) (b:make-point))
  (let ((p (finish (b:make-point 1 2))))
    (is p= #(1d0 2d0) p)
    (is eql 1d0 (b::p-dx p))
    (is eql 2d0 (b::p-dy p))
    (b::with-dpoint (p x y)
      (is eql 1d0 x)
      (is eql 2d0 y)
      (true (b:point= p (b:make-point 1 2)))
      (false (b:point= p (b:make-point 2 1)))
      (false (b:point= p (b:make-point 1 1)))
      (false (b:point= p (b:make-point 2 2)))
      #++ ;; not mutable for now
      (progn
        (finish (setf x 3d0 y 4d0))
        (is p= #(3d0 4d0) p))))

  (let ((s (finish (b:make-segment 1 2 3 4))))
    (is p= #(1d0 2d0) (b:s-p1 s))
    (is p= #(3d0 4d0) (b:s-p2 s))
    (is eql 1d0 (b::s-dx1 s))
    (is eql 2d0 (b::s-dy1 s))
    (is eql 3d0 (b::s-dx2 s))
    (is eql 4d0 (b::s-dy2 s))
    (is s= '(#(3d0 4d0) #(5d0 6d0)) (b:make-segment/p (b:make-point 3 4)
                                                      (b:make-point 5 6))))

  (flet ((test-tan (x1 y1 xc yc ax2 ay2 bx2 by2)
           (flet ((test-tan ()
                    (let* ((p1 (b::make-point x1 y1))
                           (c1 (b::make-point xc yc))
                           (a2 (b::make-point ax2 ay2))
                           (b2 (b::make-point bx2 by2))
                           (ba (b::%make-bezier2 p1 c1 a2))
                           (bb (b::%make-bezier2 p1 c1 b2))
                           (i (b::%intersect/tan p1 c1 a2 b2))
                           (eps (max (abs x1) (abs y1)
                                     (abs xc) (abs yc)
                                     (abs ax2) (abs ay2)
                                     (abs bx2) (abs by2)))
                           (pa (b::eval-at/b2/fast ba (aref i 2)))
                           (pb (b::eval-at/b2/fast bb (aref i 3))))
                      (true (v2~ (subseq i 0 2) pa eps)
                            "%intersect/tan eval @ a: ~s ~s ~s ~s ~s ~s ~s ~s:~%~s ~s"
                            x1 y1 xc yc ax2 ay2 bx2 by2
                            pa i)
                      (true (v2~ (subseq i 0 2) pb eps)
                            "%intersect/tan eval @ b: ~s ~s ~s ~s ~s ~s ~s ~s:~%~s ~s"
                            x1 y1 xc yc ax2 ay2 bx2 by2 pb i))))
             ;; test body in an flet to avoid dumping entire body into
             ;; test results
             (finish (test-tan))
             ;; try again with curves swapped
             (rotatef ax2 bx2) (rotatef ay2 by2)
             (finish (test-tan) "test-tan swapped")))
         (test-tan2 (x1 y1 axc ayc bxc byc ax2 ay2 bx2 by2)
           (flet ((test-tan2 ()
                    (let* ((p1 (b::make-point x1 y1))
                           (ac (b::make-point axc ayc))
                           (bc (b::make-point bxc byc))
                           (a2 (b::make-point ax2 ay2))
                           (b2 (b::make-point bx2 by2))
                           (ba (b::%make-bezier2 p1 ac a2))
                           (bb (b::%make-bezier2 p1 bc b2))
                           (i (b::%intersect/tan2 p1 ac bc a2 b2))
                           (eps (* 2 (max (abs x1) (abs y1)
                                          (abs axc) (abs ayc)
                                          (abs bxc) (abs byc)
                                          (abs ax2) (abs ay2)
                                          (abs bx2) (abs by2))))
                           (pa (b::eval-at/b2/fast ba (aref i 2)))
                           (pb (b::eval-at/b2/fast bb (aref i 3))))
                      (true (v2~ (subseq i 0 2) pa eps)
                            "%intersect/tan2 eval @ a: ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s:~%~s~%~s~%~s"
                            x1 y1 axc ayc bxc byc ax2 ay2 bx2 by2
                            pa i eps)
                      (true (v2~ (subseq i 0 2) pb eps)
                            "%intersect/tan2 eval @ b: ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s:~%~s ~s"
                            x1 y1 axc ayc bxc byc ax2 ay2 bx2 by2 pb i))))
             ;; test body in an flet to avoid dumping entire body into
             ;; test results
             (finish (test-tan2))
             ;; try again with curves swapped
             (rotatef axc bxc) (rotatef ayc byc)
             (rotatef ax2 bx2) (rotatef ay2 by2)
             (finish (test-tan2) "test-tan2 swapped"))))
    (macrolet ((test-tan/ni (x1 y1 xc yc ax2 ay2 bx2 by2)
                 `(progn
                    (is eql nil
                        (b::%intersect/tan (b::make-point ,x1 ,y1)
                                           (b::make-point ,xc ,yc)
                                           (b::make-point ,ax2 ,ay2)
                                           (b::make-point ,bx2 ,by2)))
                    (is eql nil
                        (b::%intersect/tan (b::make-point ,x1 ,y1)
                                           (b::make-point ,xc ,yc)
                                           (b::make-point ,bx2 ,by2)
                                           (b::make-point ,ax2 ,ay2)))))
               (test-tan2/ni (x1 y1 axc ayc bxc byc ax2 ay2 bx2 by2)
                 `(progn
                    (is eql nil
                        (b::%intersect/tan2 (b::make-point ,x1 ,y1)
                                            (b::make-point ,axc ,ayc)
                                            (b::make-point ,bxc ,byc)
                                            (b::make-point ,ax2 ,ay2)
                                            (b::make-point ,bx2 ,by2)))
                    (is eql nil
                        (b::%intersect/tan2 (b::make-point ,x1 ,y1)
                                            (b::make-point ,bxc ,byc)
                                            (b::make-point ,axc ,ayc)
                                            (b::make-point ,bx2 ,by2)
                                            (b::make-point ,ax2 ,ay2))))))
      (test-tan 0 0 1 0 2 2 1 1)
      (test-tan 1 1 2 1 2 2 3 3)
      (test-tan 1 1 3 2 4 4 2.6 2.2)
      (test-tan/ni 347 359 348 359 348 360 349 360)
      (test-tan2/ni 108 119 109 121 110 123 107.5d0 118.5d0 111.5d0 128)
      (test-tan2 108 119 109 121 110 123 107.5d0 118.5d0 103 124)
      (test-tan2 108 119 109 121 110 123 107.5d0 118.5d0 107.75d0 118.72d0)
      (test-tan2 0 0 1 0 2 0 1 1 0.5d0 0.5d0)
      ;; 2 shared endpoints = no intersections for this function
      (test-tan2/ni 0 0 1 0 2 0 1 1 1 1)
      (test-tan2 0 0 1 0 1.3 0 1 1 0 1)
      (test-tan2/ni 359 218 376 218 350 218 385 194 345 209)

      )
    )



  )
#++
(test 'geom)

(define-test shapes
  (is = 1 (length (b::contours (b::parse-shape "{ 0,0; 10,0; 10,10; 0,10; # }"))))
  (is = 2 (length (b::contours (b::parse-shape "{ 0,0; 10,0; 10,10; 0,10; # } { 2,2; 8,2; 5,8; # }"))))

  (is string= "{ 0,0; 10,0; 10,10; 0,10; # }"
      (b::serialize-shape (b::parse-shape "{0,0;10,0;10,10;0,10; # }")))
  (is string= "{ 0,0; 10,0; 10,10; 0,10; # }
{ 2,2; 8,2; 5,8; # }"
      (b::serialize-shape
       (b::parse-shape "{0, 0;10,0;10,10;0,10;#}{2,2;8,2;5,8;#}")))

  (is = 8 (hash-table-count
           (b::%prev (b::parse-shape "{ 0,0; 10,0; 10,10; 0,10; # }"))))
  (is = 14 (hash-table-count (b::%prev (b::parse-shape "{ 0,0; 10,0; 10,10; 0,10; # } { 2,2; 8,2; 5,8; # }"))))

  (is string= "{ 0,0; 0,10; 10,10; 10,0; # }"
      (b::serialize-shape
       (b::%edit-shape-to-shape
        (list (b::%reverse-contour
               (first (b::contours (b::shape-to-edit-shape (b::parse-shape "{ 0,0; 10,0; 10,10; 0,10; # }")))))))))


  )

#++
(test 'shapes)

(define-test quadratic-intersect-internals-tests
  (labels ((same-parabola-p (a1 a2 a3 b1 b2 b3 e)
             (qii::same-parabola-p (b::make-bezier2/v2 a1 a2 a3)
                                   (b::make-bezier2/v2 b1 b2 b3)
                                    e))
           (dx (v) (b::v2 (+ (b::vx v) (/ (expt 2 20)))
                          (b::vy v)))
           (dy (v) (b::v2 (b::vx v)
                          (+ (b::vy v) (/ (expt 2 20)))))
           (same-parabola (a b)
             (let* ((eps (* double-float-epsilon
                            (max (reduce 'max a :key 'abs)
                                 (reduce 'max b :key 'abs))))
                    (a1 (b::v2 (first a) (second a)))
                    (a2 (b::v2 (third a) (fourth a)))
                    (a3 (b::v2 (fifth a) (sixth a)))
                    (b1 (b::v2 (first b) (second b)))
                    (b2 (b::v2 (third b) (fourth b)))
                    (b3 (b::v2 (fifth b) (sixth b))))
               (true (same-parabola-p a1 a2 a3 b1 b2 b3 eps)
                     "~s ~s" a b)
               ;; should still be same parabola with 1 or both
               ;; reversed
               (true (same-parabola-p a1 a2 a3 b3 b2 b1 eps)
                     "r1 ~s ~s" a b)
               (true (same-parabola-p a3 a2 a1 b1 b2 b3 eps)
                     "r2 ~s ~s" a b)
               (true (same-parabola-p a3 a2 a1 b3 b2 b1 eps)
                     "r12 ~s ~s" a b)
               ;; slight permutations should be different parabolas
               (false (same-parabola-p (dx a1) a2 a3 b1 b2 b3 eps)
                      "dx1 ~s ~s" a b)
               (false (same-parabola-p a1 (dx a2) a3 b1 b2 b3 eps)
                      "dx2 ~s ~s" a b)
               (false (same-parabola-p a1 a2 (dx a3) b1 b2 b3 eps)
                      "dx3 ~s ~s" a b)
               (false (same-parabola-p a1 a2 a3 (dx b1) b2 b3 eps)
                      "dx4 ~s ~s" a b)
               (false (same-parabola-p a1 a2 a3 b1 (dx b2) b3 eps)
                      "dx5 ~s ~s" a b)
               (false (same-parabola-p a1 a2 a3 b1 b2 (dx b3) eps)
                      "dx6 ~s ~s" a b)
               (false (same-parabola-p (dy a1) a2 a3 b1 b2 b3 eps)
                      "dy1 ~s ~s" a b)
               (false (same-parabola-p a1 (dy a2) a3 b1 b2 b3 eps)
                      "dy2 ~s ~s" a b)
               (false (same-parabola-p a1 a2 (dy a3) b1 b2 b3 eps)
                      "dy3 ~s ~s" a b)
               (false (same-parabola-p a1 a2 a3 (dy b1) b2 b3 eps)
                      "dy4 ~s ~s" a b)
               (false (same-parabola-p a1 a2 a3 b1 (dy b2) b3 eps)
                      "dy5 ~s ~s" a b)
               (false (same-parabola-p a1 a2 a3 b1 b2 (dy b3) eps)
                      "dy6 ~s ~s" a b)))
           (same-parabola-splits (a &rest splits)
             (let* ((eps (* double-float-epsilon (reduce 'max a :key 'abs)))
                    (a1 (b::v2 (first a) (second a)))
                    (a2 (b::v2 (third a) (fourth a)))
                    (a3 (b::v2 (fifth a) (sixth a)))
                    (ab (b::make-bezier2/v2 a1 a2 a3)))
               (loop for s in splits
                     do (let ((b1 a1)
                              (b2 (b::%%trim-b2-p1 ab 0 s))
                              (bx (b::eval-at/b2/fast ab s))
                              (b3 (b::%%trim-b2-p1 ab s 1))
                              (b4 a3))
                          (true (same-parabola-p a1 a2 a3 b1 b2 bx eps)
                                "~s ~s ~s | ~s | ~s ~s ~s"
                                a1 a2 a3 s b1 b2 bx)
                          (true (same-parabola-p a1 a2 a3 bx b3 b4 eps)
                                "~s split ~s-1" a s)
                          (true (same-parabola-p b1 b2 bx bx b3 b4 eps)
                                "~s split 0-~s vs ~s-1" a s s))))))
    (finish (same-parabola '(877 30 873 34 869 40)
                           '(869 40 871 37 873 34.5d0)))
    (finish (same-parabola '(877 30 873 34 869 40)
                           '(873 34.5d0 875 32 877 30)))
    (finish (same-parabola '(869 40 871 37 873 34.5d0)
                           '(873 34.5d0 875 32 877 30)))
    (finish (same-parabola-splits '(877 30 873 34 869 40)
                                  1/2 1/3 1/4 1/5 2/5 1/7 2/7 3/7))))
#++
(test 'quadratic-intersect-internals-tests)
