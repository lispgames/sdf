(in-package #:sdf/base)

(defun serialize-shape (shape &key allow-ratios normalize edge-colors normalize-order)
  (let ((ox nil)
        (oy nil))
    ;; when normalize-order is true, start all contours at point with lowest
    ;; Y then lowest X
    (when normalize-order
      (let ((es (shape-to-edit-shape shape))
            (changed nil))
        (setf (contours es)
              (loop for c1 in (contours es)
                    for c2 = (if (typep c1 'es-contour-vertex) c1 (eprev c1))
                    for x1 = (p-rx (point c2))
                    for y1 = (p-ry (point c2))
                    do (unless (eql c2 c1)
                         (setf changed t))
                       (map-modifying-contour
                        c1 (lambda (n)
                             (when (typep n 'es-contour-vertex)
                               (let* ((pn (point n))
                                      (py (p-ry pn)))
                                 (when (or (< py y1)
                                           (and (= py y1)
                                                (<  (p-rx pn) x1)))
                                   (setf changed t
                                         c2 n
                                         x1 (p-rx pn)
                                         y1 py))))
                             n))
                    collect c2))
        (when changed
          (setf shape (edit-shape-to-shape es)))))
    (with-output-to-string (s)
      (labels ((pf (X)
                 ;; sbcl is doing odd things with ~e, ~f prints too
                 ;; many 0s for large/small #s, and ~g is confusing.
                 ;; prin1 seems to work well enough though?
                 (let ((*read-default-float-format* 'double-float))
                   (prin1 x s))
                 ;; (format nil "~e" 1.5d0)->"1.4999999999999997d+0"
                 ;; (format nil "~f" 1d30)->"1000000000000000000000000000000.0"
                 ;; (format nil "~g" 1.5d0)->"1.5    "
                 ;;(let ((*read-default-float-format* 'double-float))
                 ;;   (list (prin1-to-string 1.5d0)
                 ;;         (prin1-to-string 1d30)) -> ("1.5d0" "1.0d30")
                 #++
                 (let ((fx (if (floatp x) "~,,,,,,'ee" "~s"))
                       (fy (if (floatp y) "~,,,,,,'ee" "~s")))
                   (format s "~@?, ~@?" fx x fy y)))
               (p (p)
                 (let ((x (if (or (integerp (p-rx p)) allow-ratios)
                              (p-rx p)
                              (p-dx p)))
                       (y (if (or (integerp (p-ry p)) allow-ratios)
                              (p-ry p)
                              (p-dy p))))
                   (when normalize
                     ;; when normalize is true, subtract first point
                     ;; from all points, and turn 1.0, 1.0 into 1, 1 (or
                     ;; if allow-ratios is true, rationalize all floats)
                     (unless ox
                       (setf ox x)
                       (setf oy y))
                     (setf x (- x ox)
                           y (- y oy)))
                   (if allow-ratios
                       (setf x (rationalize x)
                             y (rationalize y))
                       (let ((fx (floor x))
                             (fy (floor y)))
                         (when (= x fx)
                           (setf x fx))
                         (when (= y fy)
                           (setf y fy))))
                   (pf x) (format s ",") (pf y)))
               (c (n &optional semi)
                 (let ((e (gethash n edge-colors)))
                   (when e
                     (cond
                       ((or (equalp e '(t nil t))
                            (eql e #b101))
                        (format s "m"))
                       ((or (equalp e '(nil t t))
                            (eql e #b011))
                        (format s "c"))
                       ((or (equalp e '(t t nil))
                            (eql e #b110))
                        (format s "y"))
                       (t (format s "w")))
                     (when semi
                       (format s "; "))))))
        (let ((prev-contour nil))
          (map-contour-segments
           shape
           (lambda (c# node end)
             (unless (eql c# prev-contour)
               (setf prev-contour c#)
               (format s "~&{ "))
             (etypecase node
               (point
                (p node)
                (format s "; "))
               (segment
                (when edge-colors
                  (c node t))
                ;; nothing to do
                )
               (bezier2
                (when edge-colors
                  (c node))
                (format s "(")
                (p (b2-c1 node))
                (format s "); ")))
             (when end
               (format s "# }")))))))))

(defparameter *dump-parse* nil)
#++ (setf *dump-parse* t)

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
                   (when x
                     (when *dump-parse*
                       (format t "   number = ~s~%" x))
                     (parse-number:parse-number (coerce x 'string)))))
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
                       (when *dump-parse*
                         (format t "   pair = ~s ~s~%" a b))
                       (list :p a b)))))
               (control ()
                 (when (\()
                   (when *dump-parse*
                     (format t " ("))
                   (let ((c1 (pair))
                         (c2 (when (\;)
                               (pair))))
                     (unless (\))
                       (error "expected ) after reading control point ~s,~s?~% got ~s"
                              c1 c2 (peek-char nil s)))
                     (when *dump-parse*
                       (format t "   control = ~s ~s~%" c1 c2))
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
                           while x
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
