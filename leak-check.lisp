#++
(ql:quickload 'sdf-test)
(defpackage #:sdf-leak-test
  (:use :cl)
  (:local-nicknames (:a :alexandria-2)
                    (:b :sdf/base)
                    (:t :sdf/ttf)))
(in-package #:sdf-leak-test)

(defvar *shape-failures* (make-hash-table :test 'equalp))
(defvar *failures* (make-hash-table :test 'equalp))


(defun check-leaks (sdf)
  (let* ((signs (b::signs sdf))
         (wx (array-dimension signs 1))
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

(defun leak-test-file* (font collection size spread)
  (let ((c1 0)
        (c2 0)
        (c3 0)
        (c0 0)
        (ncol 0))
    (zpb-ttf:with-font-loader (ttf font :collection-index collection)
      (setf ncol (zpb-ttf:collection-font-count ttf))
      (loop for index below (zpb-ttf:glyph-count ttf)
            for g = (zpb-ttf:index-glyph index ttf)
            when (zerop (mod index 100))
              do (format t ".") (force-output t)
            do (incf c0)
               (multiple-value-bind (shape err)
                   (ignore-errors
                    (t::shape-from-glyph g))
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
                          ((not (check-leaks sdf))
                           (incf c3)
                           (setf (gethash (list
                                           font collection index scale spread
                                           (b::serialize-shape shape
                                                               :allow-ratios t)
                                           (b::origin sdf))
                                          *failures*)
                                 :leaks)))))))))
      (if (= 0 c1 c2 c3)
          (format t "~&~s ~s/~s ok (~s)~%" font collection ncol c0)
          (format t "~&~s ~s/~s:~%!!!errors: ~s ~s ~s / ~s~%"
                  font collection ncol c1 c2 c3 c0))
      (list  c0 c1 c2 c3))))


(defun leak-test-file (font size spread)
  (multiple-value-bind (collections err)
      (ignore-errors
       (zpb-ttf:with-font-loader (ttf font)
         (zpb-ttf:collection-font-count ttf)))
    (if err
        (format t "zpb-ttf failed to open ~s~%" font)
        (if collections
            (dotimes (i collections)
              (leak-test-file* font i size spread))
            (leak-test-file* font nil size spread)))))

#++
(leak-test-file "d:/dl/fonts/Bravura.ttf" 56 3)
#++
(leak-test-file "c:/windows/fonts/arial.ttf" 56 3)
#++ *failures* 9
#++ *shape-failures*
#++
(loop for d in (reverse (append (directory "c:/Windows/fonts/*.ttf")
                        (directory "c:/Windows/fonts/*.ttc")
                        (directory "d:/dl/fonts/*.ttf")
                        (directory "d:/dl/fonts/*.ttc")))
      do (leak-test-file d 56 3))

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
               (format nil "~(~{~2,'0X~}~)"
                       (coerce
                        (md5:md5sum-string
                         (format nil "~s ~s ~s ~s" scale spread origin s))
                        'list)))))
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
