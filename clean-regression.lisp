#++ (ql:quickload '(sdf/test))
(in-package #:sdf/test)

(defun df (x) (coerce x 'double-float))

(defun cs (desc)
  (string-trim '(#\space #\newline #\return)
               (b::serialize-shape (b::clean-shape (b::parse-shape desc))
                                   :allow-ratios t :normalize-order t
                                   )))

(defun test-clean (result desc)
  (let ((es (finish (b::shape-to-edit-shape (b::parse-shape desc)))))
    ;; test is designed for shapes with single contour
    (is = 1 (length (b::contours es)))
    (finish
     (loop with n1 = (first (b::contours es))
           for i from 1
           for n = n1 then (b::enext n)
           until (eql (b::enext n) n1)
           when (typep n 'b::es-contour-vertex)
             do (is equal result
                    (b::serialize-shape
                     (i::fix-shape
                      (b::%edit-shape-to-shape (list n)))
                     :allow-ratios t :normalize-order t)
                    "rotation ~s = ~a"
                    i (b::serialize-shape
                       (b::%edit-shape-to-shape (list n))
                       :allow-ratios t))))))

(defun test-clean/m (result desc)
  (is equal result
      (b::serialize-shape
       (i::fix-shape (b::parse-shape desc))
       :allow-ratios t :normalize-order t)
      desc))

(define-test clean-misc
  ;; same split point regardless of curve direction
  (flet ((ev (s)
           (let ((q (sdf/f+f-pqueue::make-queue))
                 (s (b::parse-shape s)))
             (i::make-events-1 q s (aref (b::contours s) 0))
             (i::split-point (i::start (sdf/f+f-pqueue:dequeue q)))
             #++(sdf/f+f-pqueue:dequeue q)))
         )
    ;; original problem was that single-floats were being stored in rv
    ;; field of points so some calculations were done with those.
    #++(is v2= #(1968.7777776055866d0 991.6666666666666d0)
           (ev "{ 1975, 991; (1970, 992); 1966,991.5; #}"))
    #++(is v2= #(1968.77777726055866d0 991.6666666666666d0)
           (ev "{ 1966,991.5; (1970, 992); 1975, 991; #}"))
    ;; make sure we get full precision even with single-float inputs
    (is v2= #(1968.7777777777778d0 991.6666666666666d0)
        (ev "{ 1975, 991; (1970, 992); 1966,991.5; #}"))
    (is v2= #(1968.7777777777778d0 991.6666666666666d0)
        (ev "{ 1966,991.5; (1970, 992); 1975, 991; #}"))
    ;; and same results in both orders from something where it matters
    ;; even with full precision
    (is v2= #(3304.6332179930796d0 987.8235294117648d0)
        (ev "{ 2111,777; (2123,1001 ); 3457, 987; #}"))
    (is v2= #(3304.6332179930796d0 987.8235294117648d0)
        (ev "{ 3457, 987; (2123,1001 ); 2111,777; #}"))))
#++
(test 'clean-misc)

(define-test clean
  ;; simple contour
  (test-clean "{ 0,0; -10,10; 5,20; # }" "{0,0; -10,10; 5,20; # }")
  ;; simple contour duplicated
  (test-clean/m "{ 0,0; -10,10; 5,20; # }"
                "{0,0; -10,10; 5,20; # } {0,0; -10,10; 5,20; # }")
  (test-clean/m "{ 0,0; -10,10; 5,20; # }"
                "{0,0; -10,10; 5,20; # } {0,0; -10,10; 5,20; # } {0,0; -10,10; 5,20; # }")

  ;; simple contour with horizontal start
  (test-clean "{ 0,0; 5,10; 10,0; # }" "{0,0; 10,0; 5,10; #}")
  ;; simple contour with horizontal end
  (test-clean "{ 0,0; -5,10; 5,10; # }" "{0,0; -5,10; 5,10; #}")
  ;; simple self intersection, -> 2 contour
  (test-clean "{ 5,5; 0,10; 10,10; # }
{ 0,0; 5,5; 10,0; # }"
              "{0,0; 10,0; 0,10; 10,10; #}")
  ;; simple self intersection -> 2 contours
  (test-clean  "{ 10,0; 5,5; 10,10; # }
{ 0,0; 0,10; 5,5; # }"
              "{0,0; 0,10; 10,0; 10,10; #}")

  (test-clean "{ 0,0; 0,10; 2,10; # }"
              "{0,0; 0,10; (0,10); 1,10; (1,10); 1,10; 2,10; # }")
  (test-clean "{ 0,0; (1,2); 5,7; 15,7; (2,1); # }"
              "{ 0,0; (2, 1); 15, 7; 14, 7; 14, 8; 14, 7; 5, 7; (1, 2); # }")

  (test-clean "{ 0,5; (5,10); 10,5; (5,0); # }"
              "{ 0,5; (5,0); 10,5; (5,10); # }")

  (is string= "{ 0,0; -5,10; 5,10; # }"
      (b::serialize-shape
       (b::clean-shape (b::parse-shape "{0,0;-5,10;-2,10;0,12;-2,10;5,10;#}"))
       :normalize-order t))

  (test-clean "{ 0,0; -5,10; 5,10; # }"
              "{ 0,0; -5,10; -2,10; 0,12; -2,10; 5,10; # }")

  )

#++
(test 'clean)

#++
(test 'sdf/test)
