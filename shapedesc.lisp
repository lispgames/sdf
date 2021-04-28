(in-package #:sdf/base)
(defun serialize-shape (shape &key allow-ratios)
  (with-output-to-string (s)
    (flet ((p (p)
             (format s "~s, ~s"
                     (if (or (integerp (p-rx p)) allow-ratios)
                         (p-rx p)
                         (p-dx p))
                     (if (or (integerp (p-ry p)) allow-ratios)
                         (p-ry p)
                         (p-dy p)))))
      (let ((prev-contour nil))
        (map-contour-segments
         shape
         (lambda (c# node end)
           (unless (eql c# prev-contour)
             (setf prev-contour c#)
             (format s "{ "))
           (etypecase node
             (point
              (p node)
              (format s "; "))
             (segment
              ;; nothing to do
              )
             (bezier2
              (format s "(")
              (p (b2-c1 node))
              (format s "); ")))
           (when end
             (format s "# }~%"))))))))

(defun parse-shape (str)
  (with-input-from-string (s str)
    (with-shape-builder (shape)
      (labels ((color ()
                 (let ((n (peek-char t s)))
                   (when (position n "cmyw" :test 'char-equal)
                     (list :color
                           (intern (string-upcase (read-char s)) :keyword)))))
               (number ()
                 (let ((x (loop for n = (peek-char t s)
                                while (or (digit-char-p n)
                                          (position n "+-.edsf/"
                                                    :test 'char-equal))
                                collect (read-char s))))
                   (when x (parse-number:parse-number (coerce x 'string)))))
               (c (x)
                 (let ((n (peek-char t s)))
                   (when (char= n x)
                     (read-char s))))
               (\, () (c #\,))
               (\; () (c #\;))
               (\( () (c #\())
               (\) () (c #\)))
               (\{ () (c #\{))
               (\} () (c #\}))
               (\# () (when (c #\#) '(:hash)))
               (pair ()
                 (let ((a (number)))
                   (when a
                     (unless (\,)
                       (error "expected , after ~s while parsing point?" a))
                     (let ((b (number)))
                       (unless b
                         (error "expected number after ~s while parsing point?"
                                a))
                       (list :p a b)))))
               (control ()
                 (when (\()
                   (let ((c1 (pair))
                         (c2 (when (\;)
                               (pair))))
                     (unless (\))
                       (error "expected ) after reading control point ~s,~s?"
                              c1 c2))
                     (list :control c1 c2))))
               (contour ()
                 (when (peek-char t s nil nil)
                   (unless (\{)
                     (error "got ~s instead of contour?" (read-char s)))
                   (let ((p1 (cdr (pair))))
                     (assert p1)
                     (start-contour (first p1) (second p1))
                     (loop with cont = nil
                           with color = nil
                           for s = (\;)
                           for (x . p) = (or (pair) (control) (color) (\#))
                           do (case x
                                ((or :p :hash)
                                 (when (eql x :hash)
                                   (setf p p1))
                                 ;; todo: handle colors?
                                 (if cont
                                     (quadratic-to
                                      (first cont) (second cont)
                                      (first p) (second p))
                                     (line-to (first p) (second p)))
                                 (setf cont nil
                                       color nil)
                                 (when (eql x :hash)
                                   ;; #\# must be last thing in contour
                                   ;; if present
                                   (loop-finish)))
                                (:control
                                 (when (second p)
                                   (error "too manu points in control point?~%~s" p))
                                 (when cont
                                   (error "got 2 control points in a row?~% ~s ~s"
                                          cont p))
                                 (setf cont (cdr (first p))))
                                (:color
                                 (setf color p))))
                     (let ((end (\})))
                       (unless end
                         (error "expected } at end of contour, got ~s?"
                                (read-char s)))
                       (end-contour)))
                   t)))
        (loop while (contour))))))

#++
(loop with sf = "c:/tmp/leak-test-shapedesc.txt"
      with *read-default-float-format* = 'double-float
      for (f c i scale spread s o) in (alexandria:hash-table-keys
                                       sdf-leak-test::*failures*)
      for hash = (format nil "~(~{~2,'0X~}~)"
                         (coerce
                          (md5:md5sum-string
                           (format nil "~s ~s ~s ~s" scale spread o s))
                          'list))

      do (alexandria:write-string-into-file (sdf/base::serialize-shape
                                             (sdf/base::parse-shape s))
                                            sf
                                            :if-exists :supersede)
         (uiop:run-program
          #++(format nil "msdfgen -autoframe -pxrange ~a -scale ~f -translate ~f ~f -o c:/tmp/leak-test-~a.png -shapedesc ~a "
                     spread scale (aref o 0) (aref o 1)
                     hash
                     sf)
          (print
           (format nil "msdfgen -autoframe -pxrange ~a  -o c:/tmp/leak-test-~a.png -testrender c:/tmp/leak-test-r-~a.png 256 256 -shapedesc ~a"
                   spread hash hash sf))
          :output t))
