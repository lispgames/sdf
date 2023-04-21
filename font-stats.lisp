#++
(ql:quickload '(sdf/test no-sleep))
#++
(ql:quickload '(alexandria no-sleep zpb-ttf))
(defpackage #:sdf-font-stats
  (:use :cl)
  (:local-nicknames (:a :alexandria-2)
                    (:z :zpb-ttf)))
(in-package #:sdf-font-stats)

;;; collect various stats of fonts
;; # of collections # of chars
;; total+avg # of contours, lines, and beziers
;; (histograms of above?)
;; indices of glyphs with top 10 or so of each of above? (or seg+bez combined)?

(defvar *contour-hist* (make-hash-table))
(defvar *total-hist* (make-hash-table))
(defvar *line-hist* (make-hash-table))
(defvar *curve-hist* (make-hash-table))

(defun count-file (font-file)
  (with-simple-restart (continue "continue")
    (z:with-font-loader (ttf font-file)
      (format t "(~s~%  :collections ~s :glyphs ~s~%" font-file
              (z:collection-font-count ttf)
              (z:glyph-count ttf))
      (let* ((count (z:glyph-count ttf))
             (counts (make-array count :initial-element nil)))
        (loop for i below count
              for g = (z:index-glyph i ttf)
              for cc = 0
              for sc = 0
              for bc = 0
              do (z:do-contours (c g)
                   (incf cc)
                   (z:do-contour-segments (p0 p1 p2) c
                     p0 ;; can't DECLARE IGNORE these, so pretend to
                     p2 ;; use them
                     (if p1
                         (incf bc)
                         (incf sc))))
              do (setf (aref counts i) (list i cc sc bc))
                 (incf (gethash cc *contour-hist* 0))
                 (incf (gethash sc *line-hist* 0))
                 (incf (gethash bc *curve-hist* 0))
                 (incf (gethash (+ sc bc) *total-hist* 0)))
        (let ((mc (reduce 'max counts :key 'second))
              (ms (reduce 'max counts :key 'third))
              (mb (reduce 'max counts :key 'fourth))
              (sc (reduce '+ counts :key 'second))
              (ss (reduce '+ counts :key 'third))
              (sb (reduce '+ counts :key 'fourth)))
          (flet ((a (x) (float (/ x count) 1.0)))
            (format t "  :count ~s~%" count)
            (format t "  :max-contours ~s :max-lines ~s :max-curve ~s~%" mc ms mb)
            (format t "  :avg-contours ~s :avg-lines ~s :avg-curve ~s~%"
                    (a sc) (a ss) (a sb))
            (format t "  :dev-contours ~s :dev-lines ~s :dev-curve ~s~%"
                    (a:standard-deviation (map 'vector 'second counts))
                    (a:standard-deviation (map 'vector 'third counts))
                    (a:standard-deviation (map 'vector 'fourth counts)))
            (format t "  :max-total ~s :avg-total ~s :dev-total ~s~%"
                    (reduce 'max counts :key (lambda (a)
                                               (+ (third a) (fourth a))))
                    (a (reduce '+ counts :key (lambda (a)
                                                (+ (third a) (fourth a)))))
                    (a:standard-deviation
                     (map 'vector (lambda (a) (+ (third a) (fourth a)))
                          counts)))
            (format t "  :med-contours ~s :med-lines ~s :med-curve ~s :med-total ~s~%"
                    (a:median (map 'vector 'second counts))
                    (a:median (map 'vector 'third counts))
                    (a:median (map 'vector 'fourth counts))
                    (a:median (map 'vector (lambda (a) (+ (third a) (fourth a)))
                                   counts)))
            (setf counts (sort counts '> :key 'second))
            (format t "~&  :max-contours ~:<~@{~a~^ ~:_~}~:>"
                    (map 'list (lambda (a) (list (first a) (second a)))
                         (subseq counts 0 10)))
            (setf counts (sort counts '> :key 'third))
            (format t "~&  :max-lines ~:<~@{~a~^ ~:_~}~:>"
                    (map 'list (lambda (a) (list (first a) (third a)))
                         (subseq counts 0 10)))
            (setf counts (sort counts '> :key 'fourth))
            (format t "~&  :max-curves ~:<~@{~a~^ ~:_~}~:>"
                    (map 'list (lambda (a) (list (first a) (fourth a)))
                         (subseq counts 0 10))))
          (setf counts (sort counts '> :key (lambda (a)
                                              (+ (third a) (fourth a)))))
          (format t "~&  :max-segments ~:<~@{~a~^ ~:_~}~:>"
                  (map 'list (lambda (a) (list (first a)
                                               (+ (third a) (fourth a))))
                       (subseq counts 0 10)))))
      (format t ")~%"))))

(defparameter *stop* t)
(defmacro ignore-ttf-errors (&body body)
  `(handler-case (ignore-errors ,@body)
     (zpb-ttf::regrettable-value (c) (values nil c))))

(defvar *fonts*
  (time
   (let ((f nil)
         (n 0))
     (loop for d in (append
                     (directory "c:/Windows/fonts/*.ttf")
                     (directory "c:/Windows/fonts/*.ttc")
                     (directory "d:/dl/fonts/*.ttf")
                     (directory "d:/dl/fonts/*.ttc")
                     (directory "d:/dl/fonts/**/*.ttf")
                     (directory "d:/dl/fonts/**/*.ttc"))
           do (incf n)
              (format t "~s~%" d)
              (multiple-value-bind (r e)
                  (ignore-ttf-errors
                    (z:with-font-loader (ttf d)
                      (push d f)))
                (declare (ignore r))
                (when e (format t "~a~%" e))))
     (format t "found ~s (out of ~s)~%" (length f) n)
     (nreverse f))))

#++
(let ((fn (asdf:system-relative-pathname 'sdf "stats/stats.txt")))
  (ensure-directories-exist fn)
  (with-open-file (f fn :direction :output
                        :if-does-not-exist :create
                        :if-exists :error)
    (no-sleep:without-sleep ()
      (setf *stop* nil)
      (clrhash *total-hist*)
      (clrhash *contour-hist*)
      (clrhash *curve-hist*)
      (clrhash *line-hist*)
      (time
       (let ((*standard-output* (make-broadcast-stream *standard-output* f)))
         (loop for d in *fonts*
               until *stop*
               do (count-file d)))))))

(format t "~{~a~%~}" (sort (a:hash-table-alist *total-hist*) '> :key 'car))

(defvar *stats* (make-hash-table))
#++
(let ((fn (asdf:system-relative-pathname 'sdf "stats/stats.txt")))
  (with-open-file (f fn)
    (loop for s = (read f nil f)
          until (eql s f)
          do (setf (gethash (car s) *stats*) s))))
(defun ss (x &key (f #'>))
  (let* ((v (a:hash-table-values *stats*))
         (r (car v)))
    (loop for s in v
          when (funcall f (getf (cdr s) x) (getf (cdr r) x))
            do (setf r s))
    r))
(defun sf (x v &key (f #'>))
  (loop for s in (a:hash-table-values *stats*)
        when (funcall f (if (consp x)
                            (loop for i in x sum (getf (cdr s) i))
                            (getf (cdr s) x))
                      v)
          collect s))

#++
(length (sf '(:avg-contours :dev-contours) 10))
#++
(length (sf '(:med-total) 50))
(mapcar 'car (sf '(:avg-total) 100))
(mapcar 'car (sf '(:med-total) 100))
(format t "~{~a~%~}"
        (sort (loop for a in (a:hash-table-values *stats*)
                    collect (list (getf (cdr a) :avg-total)
                                  (getf (cdr a) :med-total)
                                  (car a)
                                  (/ (abs (- (getf (cdr a) :avg-total)
                                             (getf (cdr a) :med-total)))
                                     (getf (cdr a) :dev-total))
                                  (getf (cdr a) :dev-total)))
              '> :key 'fourth))
#++
(ss :max-contours)
#++
(ss :max-total)
#++
(#P"c:/Windows/Fonts/seguiemj.ttf"
   :COLLECTIONS NIL :GLYPHS 12189 :COUNT 12189
   :MAX-CONTOURS 93 :MAX-LINES 447 :MAX-CURVE 1151
   :AVG-CONTOURS 3.4773157 :AVG-LINES 13.785544 :AVG-CURVE 45.58077
   :DEV-CONTOURS 4.4587145 :DEV-LINES 18.52961 :DEV-CURVE 61.872967
   :MAX-TOTAL 1219 :AVG-TOTAL 59.366314 :DEV-TOTAL 73.04836
   :MAX-CONTOURS '((6868 93) (7194 50) (7199 50) (7188 44) (8458 41)
                   (7981 41) (7966 41) (8456 41) (8454 41) (7979 41))
   :MAX-LINES '((6872 447) (6868 429) (7139 195) (7144 188) (3141 187)
                (10343 164) (1708 157) (7133 148) (1933 147) (6099 139))
   :MAX-CURVES '((7199 1151) (7194 1008) (7205 905) (7188 876) (7182 874)
                 (7198 780) (7175 731) (7187 700) (7193 680) (7169 598))
   :MAX-SEGMENTS '((7199 1219) (7194 1064) (7205 967) (7188 934) (7182 890)
                   (7198 830) (7175 743) (7193 730) (7187 708) (7169 615)))
#++
(ss :avg-total)
#++
(#P"c:/Windows/Fonts/mingliub.ttc"
   :COLLECTIONS 3 :GLYPHS 49246 :COUNT 49246
   :MAX-CONTOURS 32 :MAX-LINES 260 :MAX-CURVE 448
   :AVG-CONTOURS 7.548471 :AVG-LINES 48.2361 :AVG-CURVE 106.758194
   :DEV-CONTOURS 3.392299 :DEV-LINES 19.50369 :DEV-CURVE 37.768124
   :MAX-TOTAL 656 :AVG-TOTAL 154.9943 :DEV-TOTAL 48.68391
   :MAX-CONTOURS '((46821 32) (41989 31) (37784 31) (1792 28) (20183 28)
                   (17221 27) (20181 27) (20182 27) (48206 27) (34249 26))
   :MAX-LINES '((1792 260) (43114 208) (27530 189) (34249 177) (34244 174)
                (27529 173) (5853 158) (37784 157) (36346 156) (29528 154))
   :MAX-CURVES '((43114 448) (37785 372) (37783 340) (38952 339) (8661 331)
                 (29533 321) (43091 319) (42639 317) (36354 311) (3420 310))
   :MAX-SEGMENTS '((43114 656) (37785 504) (38952 479) (1792 457) (29533 440)
                   (38953 440) (27529 436) (42639 424) (37783 423) (31140 410)))
