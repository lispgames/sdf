#++ (ql:quickload '(sdf/test))

(in-package #:sdf/test)

(defun v= (a b)
  ;; not comparing length currently since switched to returning 4
  ;; values (x,y,t1,t2) but haven't updated most tests to match
  ;; yet. Want to test t values for some new tests though, so can't
  ;; just keep using v2
  (if (and a b)
      (loop for i across a
            for j across b
            always (= i j))
      (eql a b)))

(defmacro ix (a b &rest r)
  `(is-values (g::intersect/range ,a ,b 0 1 0 1)
     ,@ (loop for i in r collect `(v= ,i))))

(defmacro ixr (a a0 a1 b b0 b1 &rest r)
  `(is-values (g::intersect/range ,a ,b ,a0 ,a1 ,b0 ,b1)
     ,@ (loop for i in r collect `(v= ,i))))

(defun p (x1 y1)
  (g::make-point x1 y1))

(defun s (x1 y1 x2 y2)
  (g::make-segment x1 y1 x2 y2))

(defun b2 (x1 y1 xc yc x2 y2)
  (g::make-bezier2 x1 y1 xc yc x2 y2))

(defun s2e (x1 y1 x2 y2 &optional (t1 0) (t2 1))
  (make-instance 'i::intersect-edge
                 :edge (g::make-segment x1 y1 x2 y2)
                 :t1 t1 :t2 t2))

(defun s2e* (x1 y1 x2 y2 t1 t2 &key xy)
  (let ((a (make-instance 'i::intersect-edge
                          :edge (g::make-segment x1 y1 x2 y2)
                          :t1 t1 :t2 t2)))
    (when xy
      (setf (slot-value a 'i::x) (aref xy 0)
            (slot-value a 'i::y) (aref xy 1)))
    a))

(defun b2e (x1 y1 xc yc x2 y2 &optional (t1 0) (t2 1) at s)
  (let ((e (make-instance 'i::intersect-edge
                          :edge (g::make-bezier2 x1 y1 xc yc x2 y2)
                          :t1 t1 :t2 t2)))
    (when s (setf (slot-value e 'i::split-point)
                  (g::eval-at/b2/fast (i::edge e) s)))
    (when at
      (let ((xy (g::eval-at/b2/fast (i::edge e) at)))
        (setf (slot-value e 'i::x) (aref xy 0)
              (slot-value e 'i::y) (aref xy 1)
              (slot-value e 'i::at) at)))
    e))

(defun b2e* (x1 y1 xc yc x2 y2 t1 t2 &key xy)
  (let ((a (make-instance 'i::intersect-edge
                          :edge (g::make-bezier2 x1 y1 xc yc x2 y2)
                          :t1 t1 :t2 t2)))
    (when xy
      (setf (slot-value a 'i::x) (aref xy 0)
            (slot-value a 'i::y) (aref xy 1)))
    a))

(defun *e (e t1 t2)
  (make-instance 'i::intersect-edge
                 :edge e
                 :t1 (df t1) :t2 (df t2)))

(defmacro ix2 (a b &rest r)
  (flet ((x (y)
           (if (= (length y) 6)
               `(b2 ,@y)
               `(s ,@y))))
    `(ix ,(x a) ,(x b) ,@r)
    `(ix ,(x b) ,(x a) ,@(loop for v in r
                               collect (if (= 4 (length v))
                                           (let ((c (copy-seq v)))
                                             (rotatef (aref c 2) (aref c 3))
                                             c)
                                           v)))))
(defmacro ix2r (a a0 a1 b b0 b1 &rest r)
  (flet ((x (y)
           (if (= (length y) 6)
               `(b2 ,@y)
               `(s ,@y))))
    `(ixr ,(x a) ,a0 ,a1 ,(x b) ,b0 ,b1 ,@r)))

(defun check-bclip (a a0 a1 b b0 b1)
  (let ((i (finish (qi::intersect
                    (qi::make-bcs
                     (qii::b2 (g::b2-dx1 a) (g::b2-dy1 a)
                              (g::b2-dxc a) (g::b2-dyc a)
                              (g::b2-dx2 a) (g::b2-dy2 a))
                     :t1 a0 :t2 a1)
                    (qi::make-bcs
                     (qii::b2 (g::b2-dx1 b) (g::b2-dy1 b)
                              (g::b2-dxc b) (g::b2-dyc b)
                              (g::b2-dx2 b) (g::b2-dy2 b))
                     :t1 b0 :t2 b1)))))
    (loop for x in i
          for at0 = (qi::at0 x)
          for at1 = (qi::at1 x)
          for bt0 = (qi::bt0 x)
          for bt1 = (qi::bt1 x)
          do (true (<= a0 at0 at1 a1))
             (true (<= b0 bt0 bt1 b1)))))

(define-test find-t
  (is = 0.21428571428571427d0
      (g::%b2-find-t -50d0 -53d0 -42d0 -50.642857142857146d0))
  (is = 786.2133324337005d0
      (i::x-at (b2e 974 934 886 1004 771 999 0.9333333373069763d0 1)
               999.3333333333334d0))
  (is = 450.02571166207537d0
      (i::x-at (b2e 447 534 497 551 542 535 0 0.5151515151515151d0) 535))

  (is = 579.0d0 (i::x-at (b2e 579 533 586 575 620 585 0 1) 533))
  (is = 579.1930773391023d0
      (i::x-at (b2e 576 461 580 540 579 533 0 0.9186046511627907d0) 533))
  (is = 579.0 (i::x-at (b2e 576 461 580 540 579 533 0.9186046511627907d0 1) 533))
  (is = 348.7551020408163d0 (i::x-at (b2e 331 421 342 426 357 424 1 0.7142857142857143d0) 424.57142857142856d0))
  (is = 348.7551020408163d0(i::x-at (b2e 331 421 342 426 357 424 0 0.7142857142857143d0) 424.57142857142856d0))
  (true (= (i::x-at (b2e 331 421 342 426 357 424 1 0.7142857142857143d0) 424.57142857142856d0)
           (i::x-at (b2e 331 421 342 426 357 424 0 0.7142857142857143d0) 424.57142857142856d0)))

  (true (= (i::x-at (b2e 328 442 317 450 306 445 1 0.6153846153846154d0)
                    446.9230769230769d0)
           (i::x-at (b2e 328 442 317 450 306 445 0 0.6153846153846154d0)
                    446.9230769230769d0)))
  #++(i::x-at (b2e 1627 673 1628 673 1629 672 0 1) 672.9999999999998d0)
  (is = 0d0 (g::%b2-find-t 673 673 672 672.9999999999998d0))
  (is = 0.9622641801834106d0
      (i::t-at (b2e 399 14 297 -37 173 -35 0 0.9622641801834106d0
                    nil 0.9622641801834106d0)
               -35.07547169811316d0))
  (is = 182.3271554883751d0
      (i::x-at (b2e 399 14 297 -37 173 -35 0 0.9622641801834106d0
                    nil 0.9622641801834106d0)
               -35.07547169811316d0)))

#++
(test 'find-t)

(define-test sort-rb
  ;; fixme: pass ID explicitly to make it not match desired order
  (is eql T (i::%sort-rb (s2e 532 331 532 601 0 1)
                         (b2e 554 362
                              532 333
                              532 331 1 0)
                         331))
  (is eql nil (i::%sort-rb (b2e 554 362 532 333 532 331 1 0)
                           (s2e 532 331 532 601 0 1)
                           331))
  (is eql nil (i::%sort-rb (b2e 579 533 586 575 620 585 0 1)
                           (b2e 576 461 580 540 579 533 1 0.9186046511627907d0)
                           533))

  (is eql nil (i::%sort-rb (s2e 484 137 484 136 0 1)
                           (b2e 484 136 484 137 482 137 0 1)
                           136))
  (is eql t (i::%sort-rb (b2e 484 136 484 137 482 137 0 1)
                         (s2e 484 137 484 136 0 1)
                         136))

  (true (eql (i::%sort-rb (s2e 484 137 484 136 0 1)
                          (b2e 484 136 484 137 482 137 0 1)
                          136)
             (i::%sort-rb (s2e 484 137 484 136 0 1)
                          (b2e 484 136 484 137 482 137 0 1)
                          137)))

  (true (eql (i::%sort-rb (b2e 484 136 484 137 482 137 0 1)
                          (s2e 484 137 484 136 0 1)
                          136)
             (i::%sort-rb (b2e 484 136 484 137 482 137 0 1)
                          (s2e 484 137 484 136 0 1)
                          137)))


  (is eql t (i::%sort-rb (s2e 100 137 100 136 0 1)
                         (s2e 484 137 484 136 0 1)
                         136))

  (is eql t (i::%sort-rb (b2e 3 7 -2 0 2 0 1 0)
                         (b2e 2 0 7 1 18 5 0 1)
                         0))
  (is eql nil (i::%sort-rb (b2e 2 0 7 1 18 5 0 1)
                           (b2e 3 7 -2 0 2 0 1 0)
                           0))

  (is eql nil (i::%sort-rb (b2e 246 271 165 266 110 228 1 0)
                           (b2e 110 228 95 244 72 244 0 1)
                           228))
  (is eql t (i::%sort-rb (b2e 110 228 95 244 72 244 0 1)
                         (b2e 246 271 165 266 110 228 1 0)
                         228))

  (is eql nil (i::%sort-rb (s2e 286 162 110 46 1 0)
                           (s2e 110 46 226 222 0 1)
                           46))
  (is eql T (i::%sort-rb (s2e 110 46 226 222 0 1)
                         (s2e 286 162 110 46 1 0)
                         46))

  ;; intersects at 52.023461716252754d0 and 52.526236835510716d0
  ;; (< 91|55.5 16|31):
  ;; 52 t
  ;; 52.023461716252754d0 -> NIL
  ;; 52.4 NIL
  ;; 52.526236835510716d0 -> T
  ;; 53 T
  (is eql NIL (i::%sort-rb/a
               (b2e 91 55.5d0 89 52 87 52 1 0)
               (b2e 16 31 83 45 123 70 0 1)
               52.023461716252754d0))
  (is eql T (i::%sort-rb/a
             (b2e 91 55.5d0 89 52 87 52 1 0)
             (b2e 16 31 83 45 123 70 0 1)
             52.526236835510716d0))

  (let ((b (b2 576 461 580 540 579 533)))
    (is eql nil (i::%sort-rb (*e b 0 0.9186046511627907d0)
                             (*e b 1 0.9186046511627907d0)
                             533)))
  (is eql nil (i::%sort-rb (b2e 576 461 580 540 579 533 0 0.9186046511627907d0)
                           (b2e 576 461 580 540 579 533 1 0.9186046511627907d0)
                           533))
  (true (eql (let ((b (b2 576 461 580 540 579 533)))
               (i::%sort-rb (*e b 0d0 0.9186046511627907d0)
                            (*e b 1d0 0.9186046511627907d0)
                            533))
             (i::%sort-rb (b2e 576 461 580 540 579 533 0 0.9186046511627907d0)
                          (b2e 576 461 580 540 579 533 1 0.9186046511627907d0)
                          533)))
  (true (eql (let ((b (b2 576 461 580 540 579 533)))
               (i::%sort-rb (*e b 1d0 0.9186046511627907d0)
                            (*e b 0d0 0.9186046511627907d0)
                            533))
             (i::%sort-rb (b2e 576 461 580 540 579 533 1 0.9186046511627907d0)
                          (b2e 576 461 580 540 579 533 0 0.9186046511627907d0)
                          533)))
  (is eql nil (i::%sort-rb (s2e 206 91 208 92 0 1)
                           (b2e 210 94 208 92 206 91 1 0)
                           91))
  (is eql T (i::%sort-rb (b2e 210 94 208 92 206 91 1 0)
                         (s2e 206 91 208 92 0 1)
                         91))
  (is eql nil (i::%sort-rb (s2e 206 91 208 92 0 1)
                           (b2e 206 91
                                208 92
                                210 94
                                0 1)
                           91))


  (is eql T (i::%sort-rb (s2e 718 19 718 -191 1 0)
                         (b2e 730 -101 718 -127 718 -163 1 0)
                         -163))
  (is eql NIL (i::%sort-rb (b2e 730 -101 718 -127 718 -163 1 0)
                           (s2e 718 19 718 -191 1 0)
                           -163))

  (is eql nil
      (i::%sort-rb (b2e 400 719 400 754 402.5d0 788 0.0d0 1.0d0)
                   (b2e 436 986.5d0 400 858 400 719 1.0d0 0.0d0)
                   719))
  (is eql t
      (i::%sort-rb (b2e 436 986.5d0 400 858 400 719 1.0d0 0.0d0)
                   (b2e 400 719 400 754 402.5d0 788 0.0d0 1.0d0)
                   719))
  (is eql nil (i::%sort-rb
               (b2e 2412 719 2412 858 2376 986.5d0 0.0d0 1.0d0)
               (b2e 2409.5d0 788 2412 754 2412 719 1.0d0 0.0d0)
               719))
  (is eql T (i::%sort-rb
             (b2e 2409.5d0 788 2412 754 2412 719 1.0d0 0.0d0)
             (b2e 2412 719 2412 858 2376 986.5d0 0.0d0 1.0d0)
             719))
  (is eql nil (i::%sort-rb
               (s2e -37 -150 1291 1584 0.0d0 1.0d0)
               (s2e 813 1055 551 567 1.0d0 0.0d0)
               1051))
  (is eql nil (i::%sort-rb
               (s2e* 1465 1584 137 -150 1.0d0 0.0d0
                     :xy #(1147.7710501092563d0 1169.7868982601285d0))
               (s2e* 1110 1434 1315 0 1.0d0 0.0d0
                     :xy #(1147.7710501092563d0 1169.7868982601285d0))
               1169.7868982601285d0))


  (is eql nil (i::%sort-rb
               (b2e 708 992 708 1023 701 816 0.0d0 0.13025210084033614d0)
               (b2e 680.5d0 1063 708 1035 708 992 1.0d0 0.0d0)
               992))
  (is eql T (i::%sort-rb
             (b2e 680.5d0 1063
                  708 1035
                  708 992
                  1.0d0 0.0d0)
             (b2e 708 992 708 1023 701 816 0.0d0 0.13025210084033614d0)
             992))

  (is eql nil (i::%sort-rb
               (b2e 896.5d0 991.5d0 897 991 896 991 1 0)
               (b2e 896 991 897 991 877 1008 0 1)
               991))
  (is eql t (i::%sort-rb
             (b2e 896 991 897 991 877 1008 0 1)
             (b2e 896.5d0 991.5d0 897 991 896 991 1 0)
             991))

  ;; 2nd deriv tests
  (macrolet ((st (left right y &optional d)
               `(progn
                  (is eql t (i::%sort-rb ,left ,right ,y)
                      ,d)
                  (is eql t (i::%sort-rb ,left ,right ,(+ y 0.0001d0))
                      "~a @ y + 0.0001d0" ,d)
                  (is eql nil (i::%sort-rb ,right ,left ,y)
                      "~a swapped" ,d))))
    ;; vertical tangent
    (st (b2e 0 0 0 1 -1 1)
        (b2e 0 0 0 1 1 1)
        0
        "left and right of vertical")
    (st (b2e 0 0 0 1 -2 1)
        (b2e 0 0 0 1 -1 1)
        0
        "both left of vertical")
    (st (b2e 0 0 0 1 1 1)
        (b2e 0 0 0 1 2 1)
        0
        "both right of vertical")
    (st (b2e 0 0 0 1 -1 1)
        (s2e 0 0 0 1)
        0 "left of line")
    (st (s2e 0 0 0 1)
        (b2e 0 0 0 1 1 1)
        0 "right of line")
    ;; diagonal tangent
    (st (b2e 0 0 -1 1 -2 1)
        (b2e 0 0 -1 1 -2 3)
        0 "left diagonal, both sides")
    (st (b2e 0 0 -1 1 -2 0.1)
        (b2e 0 0 -1 1 -2 1)
        0 "left diagonal, left")
    (st (b2e 0 0 -1 1 -2 2.5)
        (b2e 0 0 -1 1 -2 3)
        0 "left diagonal, right")
    (st (b2e 0 0 -1 1 -2 1)
        (s2e 0 0 -1 1)
        0 "left of left diagonal line")
    (st (s2e 0 0 -1 1)
        (b2e 0 0 -1 1 -2 3)
        0 "right of left diagonal line")

    (st (b2e 0 0 1 1 2 3)
        (b2e 0 0 1 1 2 1)
        0 "right diagonal, both sides")
    (st (b2e 0 0 1 1 2 1)
        (b2e 0 0 1 1 2 0.1)
        0 "right diagonal, right")
    (st (b2e 0 0 1 1 1 2)
        (b2e 0 0 1 1 2 1)
        0 "right diagonal, left")
    (st (s2e 0 0 1 1)
        (b2e 0 0 1 1 2 1)
        0 "right of right diagonal line")
    (st (b2e 0 0 1 1 1 2)
        (s2e 0 0 1 1)
        0 "left of right diagonal line")

    ;; horizontal tangent
    (st (b2e 0 0 -1 0 -2 1)
        (b2e 0 0 -1 0 -2 2)
        0 "left horizontal")
    ;; can't test this one with ST since one ends at Y
    (is eql t (i::%sort-rb
               (b2e 0 0 -1 0 -2 -1)
               (b2e 0 0 -1 0 -2 1 1 0)
               0)
        "left horizontal, both sides")
    ;; don't currently add horizontal lines, do probably don't need
    ;; this one (and can't sort by X, so correct answer is arguable
    ;; anyway)
    (is eql t (i::%sort-rb (s2e* 0 0 -1 0 0 1 :xy #(0 0))
                           (b2e 0 0 -1 0 -2 2)
                           0)
        "left horizontal above line")
    (st (b2e 0 0 1 0 2 2)
        (b2e 0 0 1 0 2 1)
        0 "right horizontal")
    ;; can't test this one with ST since one ends at Y
    (is eql t (i::%sort-rb
               (b2e 0 0 1 0 2 1 1 0)
               (b2e 0 0 1 0 2 -1)
               0)
        "right horizontal, both sides"))
  ;; more regressions
  (macrolet ((st (left right y &optional (d "sort test"))
               `(progn
                  (is eql t (i::%sort-rb ,left ,right ,y)
                      ,d)
                  (is eql t (i::%sort-rb ,left ,right ,(+ y 0.00001d0))
                      "~a @ y + 0.00001d0" ,d)
                  (is eql nil (i::%sort-rb ,right ,left ,y)
                      "~a swapped" ,d)))
             (sti (left right y &optional (d "sort test"))
               `(progn
                  (is eql t (i::%sort-rb/a ,left ,right ,y)
                      ,d)
                  (is eql t (i::%sort-rb ,left ,right ,(+ y 0.00001d0))
                      "~a @ y + 0.00001d0" ,d)
                  (is eql nil (i::%sort-rb/a ,right ,left ,y)
                      "~a swapped" ,d))))
    ;; tangent, but don't detect that due to fp error
    (st (b2e 71 678 88 678 107 659 1 0)
        (b2e 107 659 89 677 89 694 0 1)
        659
        "within epsilon of tangent")
    (st (b2e 107 659 88 678 71 678 0 1)
        (b2e 107 659 89 677 89 694 0 1)
        659
        "within epsilon of tangent")

    (sti (b2e 256 556 236 553 236 552 1 0)
         (s2e 236 552 239 622)
         552)
    (sti (s2e 236 552 239 622 0 1)
         (b2e 256 556 236 553 236 552 1 0)
         552.0086453729758d0)

    (sti (b2e 238 242 237 244 253 264 0 1)
         (s2e 238 275 238 242 1 0)
         242
         "intersection @ 242.7")

    (sti (b2e 347 359 348 359 348 360 0 1)
         (b2e 347 359 348 359 349 360 0 1)
         359)

    (sti (b2e 347 359 348 359 348 360 0 1)
         (b2e 349 360 348 359 347 359 1 0)
         359)



    ;; sort LEFT,RIGHT->T, RIGHT,LEFT->NIL
    ))
#++
(test 'sort-rb)

(define-test intersect-regression
  (ix (s 2370 -42 1965 363)
      (b2 2534.5d0 -34.0d0
          2556.0d0 -87.0d0
          2556.0d0 -144.0d0)
      nil)
  (ix (s 1837.0d0 -144.0d0 1837.0d0 1581.0d0)
      (b2 1748.0d0 -84.0d0 1731.0d0 -123.0d0 1702.0d0 -152.0d0)
      nil)
  (ix2 (108.0d0 -72.0d0 108.0d0 659.0d0)
       (209.0d0 94.0d0 209.0d0 71.0d0 226.0d0 57.0d0)
       nil)
  (ix2 (1452.0d0 817.0d0 1423.0d0 842.0d0)
       (1423.0d0 842.0d0 1408.0d0 735.0d0 1401.0d0 596.0d0)
       #(1423d0 842d0))
  (ix2 (1790.0d0 1082.0d0 1625.0d0 1086.0d0)
       (1625.0d0 1086.0d0 1629.0d0 1085.0d0 1639.0d0 1078.0d0)
       #(1625d0 1086d0))
  (ix2 (454.0d0 1484.0d0 454.0d0 0.0d0)
       (515.0d0 491.0d0 473.0d0 491.0d0 431.0d0 509.5d0)
       #(454.0d0 500.75602324263036d0))
  (is eql nil (g::intersect/range
               (b2 334.0d0 191.0d0 396.0d0 251.0d0 475.0d0 248.0d0)
               (b2 475.0d0 248.0d0 520.0d0 248.0d0 565.0d0 229.0d0)
               0 0.9523809523809523d0 0 1))
  (let* ((b (finish (b2 334.0d0 191.0d0 396.0d0 251.0d0 475.0d0 248.0d0))))
    (g::intersect/range b b 0d0 0.9523809523809523d0 0.9523809523809523d0 1d0))

  (ix2r (175/2 835/2 93 400 88 417) 0.5072463768115942d0 1d0
        (88 417 102 369 106 346) 0d0 1d0
        #(90.08448239189605d0 409.7988069570175d0)
        #(88.0d0 417.0d0))
  (check-bclip (b2 175/2 835/2 93 400 88 417) 0.5072463768115942d0 1d0
               (b2 88 417 102 369 106 346) 0d0 1d0)
  (ix2 (447 534 497 551 542 535)
       (542 535 363 409)
       #(542d0 535d0))
  (ix2r (447 534 497 551 542 535) 0 0.5151515151515151d0
        (542 535 363 409) 0d0 1d0
        nil)

  (ix2r (383 641 408 620 383 640) 0.5121951219512195d0 0
        (463 557 431 601 383 641) 0d0 1d0

        #(387.7204806973672d0 637.0236553002751d0)
        #(395.4592130911219d0 630.3120063958172d0)
        #(383.0d0 641.0d0))

  (ix2 (136.0d0 63.0d0 136.0d0 68.0d0)
       (136.0d0 68.0d0 186.0d0 47.0d0 247.0d0 44.0d0)
       #(136d0 68d0))
  (ix2r (670.5d0 424.5d0 671 424 671 425) 0.3333333333333333d0 1d0
        (671 425 670 425 670.5d0 424.5d0) 1 0
        #(671.0d0 425.0d0))
  (ix2r (671 425 670 425 670.5d0 424.5d0) 1 0
        (670.5d0 424.5d0 671 424 671 425) 0.3333333333333333d0 1d0
        #(671.0d0 425.0d0))
  (ix2 (671.0d0 425.0d0 671.0d0 424.0d0 677.0d0 409.0d0)
       (680.0d0 403.0d0 671.0d0 425.0d0)
       #(671d0 425d0))
  (ix2 (1558 795 1481 848 1399 903)
       (1571 786 1522 820 1522 819)
       #(1543.5512012583395d0 804.932663997823d0)
       #(1558 795))

  (ix2 (708.5d0 642 758 672 821.5d0 686)
       (717 650 717 412)
       #(717d0 646.9752745954174d0))
  ;; should snap to split point
  (ix2r (47 128 36 129 28 126) 1 0.25d0
        (47 128 36 129 28 126) 0 0.25d0
        #(41.6875d0 128.25d0))
  (ix2r ( -1 0 0 2 1 0) 0 0.5
        (1 0 -1 1 1 2) 0 1
        #(0 1))
  (ix2r (896 991 896 992 896.5d0 991.5d0) 0 1
        (896 991 897 991 877 1008) 0 1

        #(896.0031397887908d0 991.1490682051495d0)
        #(896.0d0 991.0d0))
  (ix2 (16 119 35 119)
       (17 118 7 128)
       #(16.0d0 119.0d0))
  (ix2r (364 1038 364 136) 1 0
        (364 383 364 281 416 230) 1 0
        #(364 383))
  (ix2 (364 136 364 1038)
       (416 230 364 281 364 383)
       #(364 383))

  (ix2r (211 272 230 284 254 274) 0 0.5454545454545454d0
        (254 274 254 255) 1 0
        nil)

  (ix2r (70 62.5d0 71 53 68.5d0 50) 1 0
        (70 -8 70 135) 0 1
        #(70.0d0 62.5d0)
        #(70.0d0 53.765306122448976d0))


  (ix2r (107 659 89 677 89 694) 0 1
        (71 678 88 678 107 659) 1 0
        #(107.0d0 659.0d0))
  (ix2 (107 659 89 677 89 694)
       (107 659 88 678 71 678)
       #(107.0d0 659.0d0))

  (ix2 (238 242 237 244 253 264)
       (238 275 238 242)
       #(238.0d0 242.0d0)
       #(238.0d0 242.71972318339095d0))


  (ix2 (323 821 300 821 254.5d0 818.5d0)
       (791 820 323 821)
       ;; should snap T values near endpoints
       #(323.0d0 821.0d0 0d0 1.0d0))

  (ix2 (479 -7 462 -24 443 -39.5d0)
       (478 -8 479 -7)
       ;; one solution
       #(479.0d0 -7.0d0 0.0d0 1.0d0))

  (ix2 (926 642 934 631 934 623)
       (926.5d0 641.5d0 926 642)
       #(926.0d0 642.0d0 0.0d0 1d0))


  (ix2 (398 696 396 696 378 688)
       (630 695 398 696)
       #(398.0d0 696.0d0 0d0 1.0d0))


  (ix2 (349 360 348 359 347 359)
       (347 359 348 359 348 360)
       #(347.0d0 359.0d0 1.0d0 0.0d0))
  (ix2 (359 218 376 218 385 194)
       (359 218 350 218 345 209)
       #(359.0d0 218.0d0 0.0d0 0.0d0))

  (ix2 (921 -81 916 -98 885 -78)
       (921 -81 926 -64 902 -37)
       #(921.0d0 -81.0d0 0.0d0 0.0d0))
  (ix2 (921 -81 926 -64 902 -37)
       (921 -81 916 -98 885 -78)
       #(921.0d0 -81.0d0 0.0d0 0.0d0))

  (ix2 (651 745 682 755 674 752)
       (674 752 690 758 684 763)
       #(674.0d0 752.0d0 1.0d0 0.0d0)
       #(674.3749854598021d0 752.1416314778505d0
         0.9750455131253722d0 0.0118142545383981d0))

  (ix2r (651 745 682 755 674 752)0 0.7692307692307693d0
        (674 752 690 758 684 763)0 1
        ;; other side of split curve doesn't intersect
        nil)

  (ix2r (651 745 682 755 674 752)0.7692307692307693d0 1
        (674 752 690 758 684 763)0 1
        #(674.0d0 752.0d0 1.0d0 0.0d0)
        #(674.3749854598021d0 752.1416314778505d0 0.9750455131253722d0
          0.0118142545383981d0))

  (ix2r (663 -28 611 -1 611 -3) 0 0.9310344827586207d0
        (611 -3 611 -2 657 -9) 1 0.125d0
        nil)
  (ix2r (663 -28 611 -1 611 -3) 0 0.9310344827586207d0
        (611 -3 611 -2 657 -9) 0 0.125d0
        #(611.4583209632425d0 -2.880073335284507d0
          0.9061178149075162d0 0.09981732916143762d0))
  (ix2r (663 -28 611 -1 611 -3) 1 0.9310344827586207d0
        (611 -3 611 -2 657 -9) 1 0.125d0
        nil)
  (ix2r (663 -28 611 -1 611 -3) 1 0.9310344827586207d0
        (611 -3 611 -2 657 -9) 0 0.125d0
        #(611.0 -3.0 1.0 0.0))

  (ix2r (733.5d0 496.5d0 727 489 719 481) 1 0
        (719 481 730 492 723 485.5d0) 0 0.6285714285714286d0
        #(719.0d0 481.0d0 1.0d0 0.0d0)
        #(725.7209569614156d0 487.9128440877246d0
          0.5619507718200542d0 0.6194951594791709d0)
        ;; from old algorithm
        #++
        #(725.7209569614155d0 487.9128440877246d0
          0.5619507718200566d0 0.6194951594791851d0))
  (ix2r (733.5d0 496.5d0 727 489 719 481) 1 0
        (719 481 730 492 723 485.5d0) 1 0.6285714285714286d0
        nil)

  (ix2r (823 626 837 608 851 623) 0.5454545454545454d0 1
        (823 626 837 608 851 623) 0.5454545454545454d0 1
        ;; duplicated split shouldn't return out of range endpoint
        #(851.0d0 623.0d0 1.0d0 1.0d0))
  (ix2r (823 626 837 608 851 623) 0.5454545454545454d0 0
        (823 626 837 608 851 623) 0.5454545454545454d0 1
        #(838.2727272727273d0 616.1818181818181d0
          0.5454545454545454d0 0.5454545454545454d0))
)
#++
(test 'intersect-regression)

(define-test intersect-misc

  (is equalp '((0 0.4))
      (qii::%union-regions '((-1 0.3)) '((0.2 0.4)) 0 1))
  (is equalp '((0 6) (6.1 6.2) (7 10))
      (qii::%union-regions '((0 1) (2 3) (4 6))
                           '((1 2) (3 4.5) (6.1 6.2) (7 11)) 0 10))
  (is equalp '((0.4166521260385379d0 0.5190293038832544d0)
               (0.5600539560841017d0 0.6624311339288182d0)
               (0.8976927766598904d0 0.919808696873577d0))
      (qii::%union-regions '((0.4264745512690546d0 0.448590471482741d0)
                             (0.8976927766598904d0 0.919808696873577d0))
                           '((0.4166521260385379d0 0.5190293038832544d0)
                             (0.5600539560841017d0 0.6624311339288182d0))
                           0d0 1d0)))
#++
(test 'intersect-misc)

#++
(test (find-package 'sdf/test))
