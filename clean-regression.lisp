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
  ;; simple self intersection, -> 1 contour
  (test-clean "{ 0,0; 5,5; 0,10; 10,10; 5,5; 10,0; # }"
              "{0,0; 10,0; 0,10; 10,10; #}")
  ;; simple self intersection -> 2 contours
  (test-clean "{ 0,0; 0,10; 5,5; 10,10; 10,0; 5,5; # }"
              "{0,0; 0,10; 10,0; 10,10; #}")

  (test-clean "{ 0,0; 0,10; 2,10; # }"
              "{0,0; 0,10; (0,10); 1,10; (1,10); 1,10; 2,10; # }")
  (test-clean "{ 0,0; (1,2); 5,7; 15,7; (2,1); # }"
              "{ 0,0; (2, 1); 15, 7; 14, 7; 14, 8; 14, 7; 5, 7; (1, 2); # }")

  (test-clean "{ 0,5; (5,10); 10,5; (5,0); # }"
              "{ 0,5; (5,0); 10,5; (5,10); # }")

  )

#++
(test 'clean)

