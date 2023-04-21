(defpackage sdf/f+f-pqueue
  (:use #:cl)
  (:shadow #:map)
  (:local-nicknames (#:a #:alexandria))
  (:export #:queue
           #:make-queue
           #:copy-queue
           #:enqueue
           #:dequeue
           #:peek
           #:size
           #:trim
           #:map
           #:do-queue
           #:queue-size-limit-reached
           #:queue-size-limit-reached-queue
           #:queue-size-limit-reached-object))

(in-package sdf/f+f-pqueue)

(deftype data-type () 't)

(deftype data-vector-type () '(simple-array data-type (*)))

(deftype extension-factor-type () '(integer 2 256))

(declaim (inline %make %data-vector %prio-vector %size %extension-factor))

(defstruct (queue (:conc-name #:%)
                  (:constructor %make)
                  (:predicate nil)
                  (:copier nil))
  (data-vector (make-array 256 :element-type 'data-type)
   :type data-vector-type)
  (prio-vector1 (make-array 256 :element-type 'double-float)
   :type (simple-array double-float (*)))
  (prio-vector2 (make-array 256 :element-type 'double-float)
   :type (simple-array double-float (*)))
  (size 0 :type a:array-length)
  (extension-factor 2 :type extension-factor-type)
  (extend-queue-p t :type boolean))

(defmacro %with-queue ((q) &body body)
  (a:with-gensyms (data-vector prio-vector1 prio-vector2)
    `(let ((,data-vector (%data-vector ,q))
           (,prio-vector1 (%prio-vector1 ,q))
           (,prio-vector2 (%prio-vector2 ,q)))
       (declare
        (ignorable ,data-vector ,prio-vector1 ,prio-vector2))
       (flet ((%prio-< (i1 i2)
                (or (< (aref ,prio-vector1 i1) (aref ,prio-vector1 i2))
                    (and (= (aref ,prio-vector1 i1) (aref ,prio-vector1 i2))
                         (< (aref ,prio-vector2 i1) (aref ,prio-vector2 i2)))))
              (%prio-<= (i1 i2)
                (or (< (aref ,prio-vector1 i1)
                       (aref ,prio-vector1 i2))
                    (and (= (aref ,prio-vector1 i1)
                            (aref ,prio-vector1 i2))
                         (<= (aref ,prio-vector2 i1)
                             (aref ,prio-vector2 i2)))))
              (%rotate (i1 i2)
                (rotatef (aref ,data-vector i1) (aref ,data-vector i2))
                (rotatef (aref ,prio-vector1 i1) (aref ,prio-vector1 i2))
                (rotatef (aref ,prio-vector2 i1) (aref ,prio-vector2 i2)))
              (%copy (i1 i2)
                (setf (aref ,data-vector i1) (aref ,data-vector i2))
                (setf (aref ,prio-vector1 i1) (aref ,prio-vector1 i2))
                (setf (aref ,prio-vector2 i1) (aref ,prio-vector2 i2)))
              (%adjust (size)
                (setf (%data-vector ,q) (adjust-array ,data-vector size))
                (setf ,data-vector (%data-vector ,q))
                (setf (%prio-vector1 ,q)
                      (adjust-array ,prio-vector1 size))
                (setf (%prio-vector2 ,q) (adjust-array ,prio-vector2 size))
                (setf ,prio-vector1 (%prio-vector1 ,q))
                (setf ,prio-vector2 (%prio-vector2 ,q)))
              (%set (index object priority1 priority2)
                (setf (aref ,data-vector index) object)
                (setf (aref ,prio-vector1 index) priority1)
                (setf (aref ,prio-vector2 index) priority2))
              (%data (x)
                (aref ,data-vector x)))
         (declare
          (ignorable (function %prio-<) (function %prio-<=) (function %rotate)
                     (function %adjust) (function %copy) (function %set)
                     (function %data))
          (inline %prio-< %prio-<= %rotate %adjust %copy %set %data))
         ,@body))))

(deftype extension-factor-type () '(integer 2 256))

(declaim (inline make-queue copy-queue))

(declaim (ftype
          (function (&optional a:array-index extension-factor-type boolean)
                    (values queue &optional))
          make-queue))

(defun make-queue (&optional (initial-storage-size 256) (extension-factor 2)
                     (extend-queue-p t))
  (declare (type extension-factor-type extension-factor))
  (declare (optimize (speed 3)))
  (%make :extension-factor extension-factor
         :data-vector (make-array initial-storage-size
                                  :element-type 'data-type)
         :extend-queue-p extend-queue-p
         :prio-vector1 (make-array initial-storage-size
                                   :element-type 'double-float)
         :prio-vector2 (make-array initial-storage-size
                                   :element-type 'double-float)))

(defmethod print-object ((object queue) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~d)" (%size object))))

(declaim (ftype (function (queue) (values queue &optional)) copy-queue))

(defun copy-queue (queue)
  (declare (type queue queue))
  (declare (optimize (speed 3)))
  (%make :extension-factor (%extension-factor queue) :size (%size queue)
         :extend-queue-p (%extend-queue-p queue) :data-vector
         (copy-seq (%data-vector queue)) :prio-vector1
         (copy-seq (%prio-vector1 queue)) :prio-vector2
         (copy-seq (%prio-vector2 queue))))

(declaim (inline heapify-upwards enqueue))

(declaim (ftype (function (queue a:array-length) (values null &optional))
                heapify-upwards))

(defun heapify-upwards (queue index)
  (declare (type queue queue))
  (declare (type a:array-length index))
  (declare (optimize (speed 3)))
  (do ((child-index index parent-index)
       (parent-index (ash (1- index) -1) (ash (1- parent-index) -1)))
      ((= child-index 0))
    (%with-queue (queue)
      (cond
        ((%prio-< child-index parent-index) (%rotate parent-index child-index))
        (t (return))))))

(declaim (ftype (function (queue t double-float double-float)
                          (values null &optional))
                enqueue))

(defun enqueue (queue object prio-vector1 prio-vector2)
  (declare (type queue queue))
  (declare (type double-float prio-vector1)
           (type double-float prio-vector2))
  (declare (optimize (speed 3)))
  (%with-queue (queue)
    (let ((size (%size queue)) (length (array-total-size (%data-vector queue))))
      (when (>= size length)
        (unless (%extend-queue-p queue)
          (error 'queue-size-limit-reached :queue queue :element object))
        (let* ((extension-factor (%extension-factor queue))
               (new-length
                 (max 1 (mod (* length extension-factor) (ash 1 64)))))
          (declare (type a:array-length new-length))
          (when (<= new-length length)
            (error "integer overflow while resizing array: new-length ~d is ~
                    smaller than old length ~d"
                   new-length length))
          (%adjust new-length)))
      (%set size object prio-vector1 prio-vector2)
      (heapify-upwards queue size)
      (incf (%size queue))
      nil)))

(declaim (inline heapify-downwards dequeue))

(declaim (ftype (function (queue a:array-index) (values null &optional))
                heapify-downwards))

(defun heapify-downwards (queue size)
  (declare (type queue queue))
  (declare (optimize (speed 3)))
  (let ((parent-index 0))
    (%with-queue (queue)
      (loop
        (let* ((left-index (+ (* parent-index 2) 1))
               (left-index-validp (< left-index size))
               (right-index (+ (* parent-index 2) 2))
               (right-index-validp (< right-index size)))
          (flet ((swap-left ()
                   (%rotate parent-index left-index)
                   (setf parent-index left-index))
                 (swap-right ()
                   (%rotate parent-index right-index)
                   1
                   (setf parent-index right-index)))
            (declare (inline swap-left swap-right))
            (when (and (not left-index-validp) (not right-index-validp))
              (return))
            (when
                (and left-index-validp (%prio-< parent-index left-index)
                     (or (not right-index-validp)
                         (%prio-< parent-index right-index)))
              (return))
            (if (and right-index-validp (%prio-<= right-index left-index))
                (swap-right)
                (swap-left))))))))

(declaim (ftype (function (queue) (values t boolean &optional)) dequeue))

(defun dequeue (queue)
  (declare (type queue queue))
  (declare (optimize (speed 3)))
  (if (= 0 (%size queue))
      (values nil nil)
      (%with-queue (queue)
        (multiple-value-prog1 (values (%data 0) t)
          (let ((new-size (decf (%size queue))))
            (%copy 0 new-size)
            (heapify-downwards queue new-size))))))

(declaim (inline peek size trim map))

(declaim (ftype (function (queue) (values t boolean &optional)) peek))

(defun peek (queue)
  (declare (type queue queue))
  (declare (optimize (speed 3)))
  (if (= 0 (%size queue))
      (values nil nil)
      (values (aref (%data-vector queue) 0) t)))

(declaim (ftype (function (queue) (values a:array-length &optional)) size))

(defun size (queue)
  (declare (type queue queue))
  (declare (optimize (speed 3)))
  (%size queue))

(declaim (ftype (function (queue) (values null &optional)) trim))

(defun trim (queue)
  (declare (type queue queue))
  (declare (optimize (speed 3)))
  (let ((size (%size queue)))
    (%with-queue (queue) (%adjust size))
    nil))

(declaim (ftype (function (queue (function (t) t)) (values null &optional))
                map))

(defun map (queue function)
  (dotimes (i (%size queue))
    (funcall function (aref (%data-vector queue) i))))

(defmacro do-queue ((object queue &optional result) &body body)
  (multiple-value-bind (forms declarations)
      (a:parse-body body)
    (a:with-gensyms (i)
      (a:once-only (queue)
        `(dotimes (,i (%size ,queue) ,result)
           (let ((,object (aref (%data-vector ,queue) ,i)))
             ,@declarations
             (tagbody ,@forms)))))))

(defun report-queue-size-limit-reached (condition stream)
  (let ((queue (queue-size-limit-reached-queue condition))
        (element (queue-size-limit-reached-object condition)))
    (format stream "size limit (~d) reached for non-extensible ~
                    queue ~s while trying to enqueue element ~s onto it."
            (length (%data-vector queue)) queue element)))

(define-condition queue-size-limit-reached
    (error)
  ((%queue :reader queue-size-limit-reached-queue :initarg :queue)
   (%object :reader queue-size-limit-reached-object :initarg :element))
  (:default-initargs :queue (a:required-argument :queue) :object
   (a:required-argument :object))
  (:report report-queue-size-limit-reached))
