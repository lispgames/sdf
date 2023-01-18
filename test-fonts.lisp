#++(ql:quickload '(sdf lparallel ironclad))
(defpackage #:sdf/test-fonts
  (:use :cl)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:b #:sdf/base)))

;;; try to render all glyphs from a set of fonts, storing shapes for
;;; all glyphs that ERROR or BREAK (where possible), or that have
;;; positive distance on perimeter of (m)sdf

(defmacro with-error-on-break (() &body body)
  ;; sbcl only for now
  #+sbcl
  `(let ((sb-ext:*invoke-debugger-hook*
           (lambda (c p)
             (declare (ignore p))
             (error "break ~a" c))))
     ,@body)
  #-sbcl
  `(progn ,@body))

(defmacro ignore-conditions (&body body)
  `(handler-case (ignore-errors ,@body)
     (zpb-ttf::regrettable-value (c) (values nil c))
     (simple-condition (c) (values nil c))))

(defmacro ignore-everything (&body body)
  `(ignore-conditions (with-error-on-break () ,@body)))
#++
(ignore-conditions (with-error-on-break () (break "foo")))

(defmacro ignore-ttf-errors (&body body)
  `(handler-case (ignore-errors ,@body)
     (zpb-ttf::regrettable-value (c) (values nil c))))

(defmacro with-kernel ((threads) &body body)
  `(let ((lparallel:*kernel* (lparallel:make-kernel ,threads)))
     (unwind-protect (progn ,@body)
       (lparallel:end-kernel))))

(defun file-size (f)
  (with-open-file (s f :element-type '(unsigned-byte 8))
    (file-length s)))

(defparameter *stop* t)

(defvar *hashes* (make-hash-table :test 'equalp))
(defvar *error-shapes* nil)

(defun hash-string (s)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256 (babel:string-to-octets s))))

(defvar *pixel-size* 24)
(defvar *spread* 3)

(defparameter *stop* t)

(defun parse-font (f)
  (unless *stop*
    (multiple-value-bind (r err)
        (ignore-everything
         (zpb-ttf::with-font-loader (ttf f)
           (loop with scale = (/ (zpb-ttf::units/em ttf)
                                 *pixel-size*)
                 for i below (zpb-ttf:glyph-count ttf)
                 for g = (progn
                                        ;(format t "i = ~s~%" i) (finish-output)
                           (zpb-ttf:index-glyph i ttf))
                 for (s se) = (progn
                                #++(when (= i 731)
                                     (zpb-ttf:do-contours (c g)
                                       (format t "contour~%")
                                       (zpb-ttf:do-contour-segments (a b c) c
                                         (format t "~s ~s ~s~%" a b c)))
                                     (break "~s" g))
                                        ;(format t "shape~%") (finish-output)
                                (multiple-value-list
                                 (ignore-everything
                                  (sdf/ttf::shape-from-glyph g))))
                 for h = (when (and s (plusp (length (sdf/base::contours s))))
                                        ;(format t "ser ~s~%" (length (sdf/base::contours s))) (finish-output)
                           (hash-string (sdf/base::serialize-shape s)))
                                        ;do (format t "~s~%" h) (finish-output)
                 when (and h (not (gethash h *hashes*)))
                   collect (if se
                               (list :errored :error se :font f :index i)
                               (list :parsed :shape s :hash h :font f :index i
                                             :scale scale :spread *spread*))
                 when h do (setf (gethash h *hashes*) t))))
      (list :parse f r err))))

(setf *stop* nil)

(defvar *parse-failed* nil)

(defun test-shape (spec)
  (destructuring-bind (&key shape hash font index scale spread) spec
    (declare (ignorable shape hash font index scale spread))
    (let* ((s (sdf/cleaner::fix-shape shape))
           (sdf (sdf/base::make-sdf :mtsdf s :spread spread :scale scale))
           (img (sdf/base::image sdf)))
      (declare (type (simple-array single-float (* * 4)) img))
      (unless (zerop (length (sdf/base::contours s)))
        (destructuring-bind (wy wx c) (array-dimensions img)
          (declare (ignore c))
          (flet ((med3 (x y)
                   (let ((a (aref img y x 0))
                         (b (aref img y x 1))
                         (c (aref img y x 2)))
                     (max (min a b) (min (max a b) c)))))
            (loop for i below wx
                  do (assert (minusp (med3 i 0)))
                     (assert (minusp (med3 i (1- wy))))
                     (assert (minusp (aref img 0 i 3)))
                     (assert (minusp (aref img (1- wy) i 3))))
            (loop for j below wy
                  do (assert (minusp (med3 0 j)))
                     (assert (minusp (med3 (1- wx) j)))
                     (assert (minusp (aref img j 0 3)))
                     (assert (minusp (aref img j (1- wx) 3))))))))
    :ok))

#++
(test-shape
 (list :shape (sdf/base::parse-shape "{ -40,-40; 10,0; 40,40; 0,10; # }")
       :hash "a" :font "b" :index 2 :scale 2 :spread 3))

(defun test-shapes (shapes)
  (unless *stop*
    (multiple-value-bind (r e)
        (ignore-everything
         (loop for spec in shapes
               for (ok e) = (multiple-value-list
                             (ignore-everything (test-shape spec)))
               collect (list* :ok ok :err e spec)))
      (list :test r e))))

(defun test-fonts (font-list error-file
                   &key shape-hash-file (clear-shapes t) (workers 6)
                     sort)
  (when clear-shapes
    (clrhash *hashes*)
    (setf *error-shapes* nil))
  (setf *parse-failed* nil)
  (when error-file
    ;; don't bother testing exiting shapes again, don't need another
    ;; copy in the error file if it is still bad
    (with-open-file (f error-file)
      (loop for i = (read f nil f)
            for s = (when (consp i) (getf i :shape))
            until (eql i f)
            when s
              do (setf (gethash (hash-string s) *hashes*) t))))
  (when shape-hash-file
    (with-open-file (f shape-hash-file)
      (loop for l = (read-line f nil f)
            until (eql l f)
            do (setf (gethash l *hashes*) t))))
  (setf *stop* nil)
  (with-open-file (err-stream error-file
                              :direction :output
                              :if-exists :append
                              :if-does-not-exist :create)
    (with-open-stream (hash-stream (if shape-hash-file
                                       (open shape-hash-file
                                             :direction :output
                                             :if-exists :append
                                             :if-does-not-exist :create)
                                       (make-broadcast-stream)))
      (with-kernel (workers)
        (time
         (let* ((ff (if sort
                        (sort (copy-seq font-list) '> :key 'file-size)
                        font-list))
                (nf (length ff))
                (batch 128)
                (limit (floor 65536 batch))
                (channel (lparallel:make-channel))
                (work nil)
                (parse nil)
                (wait 0)
                (done 0)
                (errors 0)
                (total 0)
                (fonts-parsed 0)
                (fonts-error 0))
           (when ff
             (labels ((submit-parse ()
                        (when ff
                          (let ((f (pop ff)))
                            (format t "parse ~s ~s/~s~%" f
                                    (+ 1 fonts-parsed fonts-error) nf)
                            (let ((lparallel:*task-priority* :default))
                              (setf parse t)
                              (lparallel:submit-task channel
                                                     (lambda ()
                                                       (parse-font f)))))))
                      (submit-test ()
                        (let ((w (loop repeat batch
                                       while work
                                       collect (pop work))))
                          (when w
                            ;; lower priority than parsing
                            (let ((lparallel:*task-priority* :low))
                              (incf wait)
                              (lparallel:submit-task channel
                                                     (lambda ()
                                                       (test-shapes w)))))))
                      (submit-work ()
                        (when (and ff (not parse) (< wait (* 0.75 limit)))
                          (submit-parse))
                        (loop while (and (< wait limit) work)
                              do (submit-test)))
                      (add-work (new)
                        (loop for (k . r) in new
                              do (ecase k
                                   (:parsed
                                    (push r work))
                                   (:errored
                                    (push r *parse-failed*)))))
                      (handle-test (spec)
                        (destructuring-bind (&key ok err shape hash font
                                               index scale spread)
                            spec
                          (format hash-stream "~a~%" hash)
                          (finish-output hash-stream)
                          (incf total)
                          (cond
                            (ok (incf done))
                            (t
                             (incf errors)
                             (let ((er (list :font font :index index
                                             :scale scale :spread spread
                                             :error (format nil "~a" err)
                                             :shape (ignore-everything
                                                     (sdf/base::serialize-shape shape)))))
                               (push er *error-shapes*)
                               (write er :stream err-stream)
                               (finish-output err-stream)
                               (terpri err-stream))))))
                      (handle-tests (r)
                        (map 'nil #'handle-test r)))
               ;; start parsing first font
               (submit-parse)
               ;; main loop
               (loop while (or parse work (not (zerop wait)))
                     for (k . rest) = (lparallel:receive-result channel)
                     do (ecase k
                          (:parse
                           (incf fonts-parsed)
                           (setf parse nil)
                           (destructuring-bind (f parsed err) rest
                             (cond
                               (err
                                (incf fonts-error)
                                (format t "failed to parse ~s~%  ~a~%" f err))
                               (t
                                (format t "parsed ~s (~s(-~s)/~s~%"
                                        f fonts-parsed fonts-error nf)
                                (add-work parsed)))))
                          (:test
                           (decf wait)
                           (destructuring-bind (r err) rest
                             (cond
                               (err
                                (format t "test errored ~a?" err))
                               (t (handle-tests r))))
                           (format t "~s (- ~s) done (wait ~s/~s)~%"
                                   total errors wait limit)))
                        (submit-work)
                     until *stop*)))))))))

(defvar *fonts* (directory "d:/tmp/ttf/ttf/*/*.*"))

(defvar *fonts2*
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
                  (ignore-everything
                   (zpb-ttf:with-font-loader (ttf d)
                     (push d f)))
                (declare (ignore r))
                (when e (format t "~a~%" e))))
     (format t "found ~s (out of ~s)~%" (length f) n)
     (nreverse f))))

#++
(test-fonts (append *fonts* *fonts2*)
            "/tmp/test-fonts-error.lisp" :clear-shapes t
                                         :shape-hash-file "/tmp/test-fonts-hash.txt"
                                         :workers 6)

#++
(hash-table-count *hashes*)
#++
(length *error-shapes*)

(defun retry-test (spec)
  (destructuring-bind (&key font index scale spread error shape) spec
    (declare (ignorable font index scale spread error shape))
    (test-shape (list :shape (sdf/base::parse-shape shape)
                      :index index :scale scale :spread spread
                      :hash (hash-string shape)
                      :font font))))

(defun retry-tests (file)
  (with-open-file (f file)
    (loop with b:*check* = t
          for i from 0
          for x = (read f nil f)
          for ok = nil
          until (eql x f)
          do (format t "~s~%" i)
             (with-simple-restart (skip "skip ~s" i)
               (retry-test x)
               (setf ok t))
             (if ok
                 (push (list 0 (sdf/base::parse-shape (getf x :shape))
                             0 0 (getf x :font) (getf x :index)
                             nil 0 0 0 0)
                       sdf/cleaner::*shapes*)
                 (push (list 0 (sdf/base::parse-shape (getf x :shape))
                             0 0 (getf x :font) (getf x :index)
                             nil 0 0 0 0)
                       sdf/cleaner::*error-shapes*)))))
#++
(let ((skipped 0))
  (setf sdf/cleaner::*error-shapes* nil
        sdf/cleaner::*shapes* nil)
  (handler-bind ((error (lambda (c)
                          (format t "~a~%" c)
                          (let ((r (find-restart 'skip)))
                            (when r
                              (incf skipped)
                              (invoke-restart r))))))
    (retry-tests "/tmp/test-fonts-error.lisp"))
  (format t " skipped ~s~%" skipped))

(defun save-png (desc)
  (let* ((s (sdf/base::parse-shape (getf desc :shape)))
         (sp (getf desc :spread))
         (sc (getf desc :scale))
         (sdf (sdf/base::make-sdf :mtsdf (sdf/cleaner::fix-shape s)
                                  :spread sp :scale sc))
         (img1 (sdf/base::image sdf))
         (img (make-array (array-dimensions img1)
                          :element-type '(unsigned-byte 8))))
    (flet ((s (x)
             (max 0
                  (min 255
                       (round (+ 128 (* 127 x)))))))
      (loop for i below (array-total-size img)
            do (setf (row-major-aref img i)
                     (s (row-major-aref img1 i)))))
    (opticl:write-png-file "/tmp/test-fonts-sdf.png" img)))
