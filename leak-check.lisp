#++
(ql:quickload '(sdf-test no-sleep))
(defpackage #:sdf-leak-test
  (:use :cl)
  (:local-nicknames (:a :alexandria-2)
                    (:b :sdf/base)
                    (:t :sdf/ttf)))
(in-package #:sdf-leak-test)

(defvar *shape-failures* (make-hash-table :test 'equalp))
(defvar *failures* (make-hash-table :test 'equalp))
(defvar *break-on-mismatch* nil)
(defvar *runs* nil)

(defun md5 (string)
  (format nil "~(~{~2,'0X~}~)"
          (coerce (md5:md5sum-string string) 'list)))

(defun try? (x)
  (and *runs*
       (shiftf (gethash (md5 (format nil "~s" x)) *runs*) t)))

(defmacro with-dupe-test (()&body body)
  `(let ((*runs* (or *runs* (make-hash-table :test 'equal))))
     ,@body))

(defun check-leaks (signs)
  (let* ((wx (array-dimension signs 1))
         (wy (array-dimension signs 0)))
    (and
     (loop with y = (1- wy)
           for x below wx
           always (zerop (aref signs 0 x))
           always (zerop (aref signs y x)))
     (loop with x = (1- wx)
           for y below wy
           always (zerop (aref signs y 0))
           always (zerop (aref signs y x))))))

(defvar *md* 0)
(defvar *dh* (make-hash-table))
(defvar *mismatch* (make-hash-table :test 'equalp))
(defvar *print-leaks* nil)

(defun leak-test-transpose (sdf &optional key)
  (let* ((*print-right-margin* 256)
         (mask1 (b::signs sdf))
         (wy (array-dimension mask1 0))
         (wx (array-dimension mask1 1))
         (*print-leaks* *print-leaks*)
         (mask2 (b::make-transpose-mask-for-sdf sdf)))
    (unless (equalp mask1 mask2)
      (let ((md 0))
        (loop for y below wy
              do(loop for x below wx
                      for a = (aref mask1 y x)
                      for b = (aref mask2 y x)
                      for d = (when (/= a b)
                                (/ (b::distance-to-shape
                                    (b::cleaned-shape sdf)
                                    (aref (b::samples/x sdf) x)
                                    (aref (b::samples/y sdf) y))
                                   (b::pixel-scale sdf)))
                      do (incf (gethash d *dh* 0))
                      when (and d (not (zerop d)))
                        do (setf md (max md d)
                                 *md* (max *md* d))))
        #++(unless (zerop md)
             (format t "~&### ~s~%" md))
        (when (> md 1.0e-6)
          (format t "~&### ~s~%" md))
        (when (> md 0.00001d0)
          (setf *print-leaks* t)
          (setf (gethash key *mismatch*)
                (max md (gethash key *mismatch* 0) 0))
          #++(break "md ~s~%" md))))
    (when *print-leaks*
      (unless (equalp mask1 mask2)
        (format t "mismatch:~%")
        (let ((dc 0))
          (loop for y below wy
                for dx = nil
                do (format t " ")
                   (loop for x below wx
                         for a = (aref mask1 y x)
                         for b = (aref mask2 y x)
                         for d = (when (/= a b)
                                   (/ (b::distance-to-shape
                                       (b::cleaned-shape sdf)
                                       (aref (b::samples/x sdf) x)
                                       (aref (b::samples/y sdf) y))
                                      (b::pixel-scale sdf)))
                         do (when (and d (> d 0.001))
                              (when (and *break-on-mismatch*
                                         (numberp *break-on-mismatch*)
                                         (> d *break-on-mismatch*))
                                (break "~s,~s = ~s,~s (~f,~f)~% d1 = ~s~% dc = ~s~%ds = ~s @ ~s~%~s"
                                       x y
                                       (aref (b::samples/x sdf) x)
                                       (aref (b::samples/y sdf) y)
                                       (float (aref (b::samples/x sdf) x))
                                       (float (aref (b::samples/y sdf) y))
                                       (float (b::distance-to-shape
                                               (b::shape sdf)
                                               (aref (b::samples/x sdf) x)
                                               (aref (b::samples/y sdf) y)))
                                       (float (b::distance-to-shape
                                               (b::cleaned-shape sdf)
                                               (aref (b::samples/x sdf) x)
                                               (aref (b::samples/y sdf) y)))
                                       (float (/ (b::distance-to-shape
                                                  (b::cleaned-shape sdf)
                                                  (aref (b::samples/x sdf) x)
                                                  (aref (b::samples/y sdf) y))
                                                 (b::pixel-scale sdf)))
                                       (b::pixel-scale sdf)
                                       (b::serialize-shape (b::cleaned-shape sdf))))
                              (incf dc))
                            (format t "~a" (cond
                                             ((= a b 0) " ")
                                             ((= a b 1) ".")
                                             ((< d 0.001)
                                              "~")
                                             (d
                                              (push d dx)
                                              (if (= a 1) "A" "B"))
                                             #++((= a 1) "a")
                                             #++((= b 1) "b"))))
                   (format t "|~@[~{~,4f~}~]~%"
                           (reverse dx)))
          #++
          (let ((diff (make-array (array-dimensions mask1) :initial-element 0)))
            (loop for i below (array-total-size mask1)
                  for a = (row-major-aref mask1 i)
                  for b = (row-major-aref mask2 i)
                  when (/= a b)
                    do (setf (row-major-aref diff i)
                             (if (zerop a) 2 1))
                  else do (unless (zerop a)
                            (setf (row-major-aref diff i) 9)))
            (format t " mismatch:~% ~s~%" diff)
            #++(format t " mismatch:~% ~s~% ~s~%" mask1 mask2)
            #++(format t " mismatch:~% ~s~% ~s~%" mask1 mask2))
          (unless (zerop dc)
            (format t "h edges:~%")
            (let ((b::*dump-mask* nil))
              (loop for i from 0
                    for e across (b::make-edge-list sdf)
                    for y = (aref (b::samples/y sdf) i)
                    do (format t "  ~s : ~,3f : ~s~%" i y e))
              (format t "v edges:~%")
              (loop for i from 0
                    for e across (b::make-transpose-edge-list sdf)
                    for x = (aref (b::samples/x sdf) i)
                    do (format t "  ~s : ~,3f : ~s~%" i x e)))
            (when *break-on-mismatch*
              (break "mismatch"))))))
    mask2))

(defun leak-test-file* (font collection size spread &key (start 0))
  (let ((c1 0)
        (c2 0)
        (c3 0)
        (c4 0)
        (c0 0)
        (ncol 0))
    (zpb-ttf:with-font-loader (ttf font :collection-index collection)
      (setf ncol (zpb-ttf:collection-font-count ttf))
      (loop for index from start below (zpb-ttf:glyph-count ttf)
            for g = (zpb-ttf:index-glyph index ttf)
            when (zerop (mod index 100))
              do (format t ".") (force-output t)
            do (incf c0)
               (multiple-value-bind (shape err)
                   (ignore-errors
                    (t::shape-from-glyph g))
                 (unless (try? (list size spread (b::serialize-shape shape)))
                   (cond
                     (err
                      (incf c1)
                      (setf (gethash (list font collection index)
                                     *shape-failures*)
                            err))
                     (t
                      (let* ((em (- size (* 2 spread)))
                             (scale (/ (zpb-ttf:units/em ttf) em)))
                        (multiple-value-bind (sdf err)
                            (ignore-errors
                             (b::make-sdf :sdf shape
                                          :spread spread
                                          :scale scale
                                          :render nil))
                          (cond
                            (err
                             (incf c2)
                             (setf (gethash
                                    (list
                                     font collection index scale spread nil
                                     (b::serialize-shape shape :allow-ratios t))
                                    *failures*)
                                   err))
                            ((not (check-leaks (b::signs sdf)))
                             (incf c3)
                             (setf (gethash (list
                                             font collection index scale spread
                                             (b::serialize-shape shape
                                                                 :allow-ratios t)
                                             (b::origin sdf))
                                            *failures*)
                                   :leaks))
                            (t
                             (unless (check-leaks (leak-test-transpose
                                                   sdf
                                                   (list
                                                    font collection
                                                    index scale spread
                                                    (b::origin sdf)
                                                    (b::serialize-shape
                                                     shape :allow-ratios t))))
                               (incf c4)
                               (setf (gethash (list
                                               font collection index scale spread
                                               (b::origin sdf)
                                               (b::serialize-shape shape
                                                                   :allow-ratios t))
                                              *failures*)
                                     :transpose-leaks)))))))))))
      (if (= 0 c1 c2 c3 c4)
          (format t "~&~s ~s/~s ok (~s)~%" font collection ncol c0)
          (format t "~&~s ~s/~s:~%!!!errors: ~s ~s ~s ~s / ~s~%"
                  font collection ncol c1 c2 c3 c4 c0))
      (list  c0 c1 c2 c3 c4))))




(defun leak-test-file (font size spread &key (start 0))
  (with-dupe-test ()
    (multiple-value-bind (collections err)
        (ignore-errors
         (zpb-ttf:with-font-loader (ttf font)
           (zpb-ttf:collection-font-count ttf)))
      (if err
          (format t "zpb-ttf failed to open ~s~%" font)
          (if collections
              (dotimes (i collections)
                (leak-test-file* font i size spread :start start))
              (leak-test-file* font nil size spread :start start))))))

#++
(leak-test-file "d:/dl/fonts/Bravura.ttf" 56 3)
#++
(leak-test-file "c:/windows/fonts/arial.ttf" 56 3)

(defparameter *stop* t)
#++
(no-sleep:without-sleep ()
  (with-dupe-test ()
    (setf *stop* nil)
    (unwind-protect
         (loop until *stop*
               for size = (+ 20 (random 50))
               for spread = (if (plusp (random 2))
                                (1+ (random 5.0))
                                (+ 1 (/ (random 10)
                                        (1+ (random 5)))))
               do (sb-ext:gc :full t)
                  (format t "testing at ~s ~s~%" size spread)
                  (time
                   (loop for d in (append (directory "c:/Windows/fonts/*.ttf")
                                          (directory "c:/Windows/fonts/*.ttc")
                                          (directory "d:/dl/fonts/*.ttf")
                                          (directory "d:/dl/fonts/*.ttc"))
                         do (leak-test-file d size spread)))
                  (format t " ~s shapes tested~%" (hash-table-count *runs*)))
      (format t " ~s shapes tested~%" (hash-table-count *runs*)))))

(defun save-regressions (file)
  (let ((h (make-hash-table :test 'equalp)))
    (flet ((hash (x)
             (destructuring-bind (f c i scale spread origin s)
                 x
               ;; font,collection,index are just for reference, we
               ;; only care about other args for regression testing
               ;; (and don't care about same shape from multiple
               ;; files)
               (declare (ignore f c i))
               (md5 (format nil "~s ~s ~s ~s" scale spread origin s)))))
      (when (probe-file file)
        (with-open-file (f file :direction :input)
          (loop for x = (read f nil f)
                until (eq x f)
                do (let ((hh (hash x)))
                     (when (gethash hh h)
                       (format t "duplicate hash ~s?~%  ~s~%"
                               hh x))
                     (setf (gethash hh h) t)))))
      (with-open-file (f file :direction :output
                              :if-does-not-exist :create
                              :if-exists :append)
        (loop for i in (a:hash-table-keys *failures*)
              for hh = (hash i)
              unless (gethash hh h)
                do (print i f)
                   (setf (gethash hh h) t))))))
#++
(save-regressions "c:/tmp/sdf-leak-test-regressions.lisp")
