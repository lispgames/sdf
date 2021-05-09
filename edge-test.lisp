#++(ql:quickload '(sdf/test))
#++(asdf:test-system 'sdf)
(defpackage #:sdf/edge-test
  (:use :parachute :cl)
  (:local-nicknames (:b :sdf/base)
                    (:f :sdf/ttf)
                    (:s :sdf)))
(in-package sdf/edge-test)


(defun ulp+ (f n)
  (etypecase f
    (single-float
     (float-features:bits-single-float
      (+ (float-features:single-float-bits f) n)))
    (double-float
     (float-features:bits-double-float
      (+ (float-features:double-float-bits f) n)))))


(defun make-samples (ref skip type box)
  (assert (plusp skip))
  (assert (< (b::raabb-y1 box) (b::raabb-y2 box)))
  (destructuring-bind (type &optional bits)
      (alexandria:ensure-list type)
    (let* ((y1 (b::raabb-y1 box))
           (y2 (b::raabb-y2 box))
           (j12 (cond
                  ((< ref y1)
                   (loop for j from 0
                         for y = (+ ref (* skip j))
                         for ny = (+ ref (* skip (1+ j)))
                         when (and (<= y y1) (< y1 ny))
                           collect j
                         when (and (< y y2) (<= y2 ny))
                           collect (1+ j)
                           and do (loop-finish)))
                  ((> ref y2)
                   (reverse
                    (loop for j downfrom 0
                          for y = (+ ref (* skip j))
                          for ny = (+ ref (* skip (1+ j)))
                          when (and (< y y2) (<= y2 ny))
                            collect (1+ j)
                          when (and (<= y y1) (< y1 ny))
                            collect j
                            and do (loop-finish))))
                  (t
                   (list
                    (loop for j downfrom 0
                          for y = (+ ref (* skip j))
                          for ny = (+ ref (* skip (1+ j)))
                          when (and (<= y y1) (< y1 ny))
                            return j)
                    (loop for j from 0
                          for y = (+ ref (* skip j))
                          for ny = (+ ref (* skip (1+ j)))
                          when (and (< y y2) (<= y2 ny))
                            return (1+ j)))
                   )))
           (j1 (- (first j12) 2))
           (j2 (+ (second j12) 2))
           (ns (abs (1+ (- j2 j1))))
           (s (make-array ns :initial-element nil)))
      (loop for j below ns
            for y = (coerce (+ ref (* skip (+ j1 j))) type)
            do (setf (aref s j) (if bits (ulp+ y bits) y)))
      s)))

#++
(make-samples 40/3 1/10 'single-float (b::make-raabb 0 6 0 9))
#++
(make-samples 40/3 1/10 `(single-float -1) (b::make-raabb 0 6 0 9))

(defun shape-max-length (s)
  (let ((l 0)
        (m 0))
    (b::map-contour-segments
     s
     (lambda (c a e)
       (declare (ignore c))
       (when (typep a 'b::point)
         (incf l))
       (when e
         (setf m (max l m))
         (setf l 0))))
    m))
(defun rotate-shape (s n)
  (let ((new (make-instance 'b::shape)))
    (setf (slot-value new 'b::contours) (copy-seq (b::contours s)))
    (setf (slot-value new 'b::%next) (alexandria:copy-hash-table (b::%next s)))
    (setf (slot-value new 'b::%prev) (alexandria:copy-hash-table (b::%prev s)))
    (setf (slot-value new 'b::bounding-box) (b::bounding-box s))
    (setf (slot-value new 'b::rbounding-box) (b::rbounding-box s))
    (loop with c = (b::contours new)
          with next = (b::%next new)
          for i below (length c)
          do (loop repeat n
                   do (setf (aref c i) (gethash (aref c i) next))
                      (setf (aref c i) (gethash (aref c i) next)))
             (unless (typep (aref c i) 'b::point)
               (format t "rotated ~s by ~s = ~s~%" i n (aref c i)))
          #++(assert (typep (aref c i) 'b::point)))
    new))

(defmacro do-rotated-shapes ((var orig &optional (rot (gensym "I"))) &body body)
  (alexandria:with-gensyms (l)
    `(let ((,l (shape-max-length ,orig)))
       (dotimes (,rot ,l)
         (let ((,var (rotate-shape ,orig ,rot)))
           ,@body)))))

(defun test-shape1 (shape samples)
  (let ((edges (b::%make-edge-list shape samples)))
    (flet ((edge-ok (e)
             (when e
               (assert (evenp (length e)))
               (let ((up (count :up e :key 'car))
                     (down (count :down e :key 'car)))
                 (assert (= up down)))
               #++
               (loop for (a b) on e
                     while b
                     do (assert (not (eql (first a) (first b))))
                     #++
                      (assert (>= (second b) (second a))))
               )
             t
             #++
             (or
              (not e)
              (and (evenp (length e))
                   (loop for (a b) on e
                         while b
                         always (and (not (eql (first a) (first b)))
                                     (> (second b) (second a))))))))
      (assert (every #'edge-ok edges))))
  t)
#++
(let ((s (b::parse-shape "{ 0, 0; 10, 0; 10, 10;  5, 15;  0, 10; # }")))
  (test-shape1 s (make-samples 1/3 1/9 'single-float
                               (print (b::rbounding-box s)))))



(defun %test-shape (shape ref skip)
  (loop for sample-type
          in `(real double-float single-float
                    ;; floats with +- 1 bit offset from exact value
                    (double-float 1) (double-float -1)
                    (single-float 1) (single-float -1))
        for samples = (make-samples ref skip sample-type
                                    (b::rbounding-box shape))
        do (loop for shape-type in '(real double-float single-float)
                 for shape2 = (b::coerce-shape shape shape-type)
                 do (do-rotated-shapes (rshape shape2 rot)
                      (test-shape1 rshape samples)
                      "test-shape1 sample=~s @~s+~s, shape=~s,rot~s"
                      sample-type ref skip shape-type rot))))

(defun parse-shape (desc dx dy &optional transpose)
  (let ((a (b::parse-shape desc)))
    (b::clean-shape (b::translate-shape (if transpose
                                            (b::transpose-shape a)
                                            a)
                                        dx dy))))

(defun test-shape (desc ref skip &optional (dx 1023) (dy 1023))
  (%test-shape (parse-shape desc dx dy) ref skip)
  (%test-shape (parse-shape desc dx dy t) ref skip))

(define-test shapes
  (finish
   (test-shape "{ 0, 0; 10, 0; 10, 10;  5, 15;  0, 10; # }" 0 1/8))
  (finish
   (test-shape "{ 0, 0; 10, 0; 10, 10;  5, 15;  0, 10; # }" 1/3 1/11))
  (finish
   (test-shape "{ 0,0; (0,2); 5,5; (10,10); 20,20; (20, 10); 20,0; 20,-10; (10,-10); #}"
               0 1/40))
  (finish
    (test-shape "{ 313, 903; (291, 819); 292, 739; (292, 695); 296, 651; (301, 607); 306, 575;  # }"
               232 30 0 0)
   )
)

#++
(let ((*break-on-signals* t))
  (test 'shapes))

