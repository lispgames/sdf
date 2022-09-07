(in-package #:sdf/base)

;;; experimental alternative to SHAPE class that supports in-place
;;; editing. Currently only intended for implementing a few functions
;;; that are easier that way, but might eventually replace SHAPE.

(defclass es-contour-node ()
  ;; contours are stored as doubly linked list,
  ;; alternating vertex and edge nodes
  ((prev :initarg :prev :accessor eprev)
   (next :initarg :next :accessor enext)))

(defclass es-contour-vertex (es-contour-node)
  ((point :initarg :point :reader point)))

(defclass es-contour-edge (es-contour-node)
  ;; not sure if we need a common base class for edges or not?
  ())

(defclass es-contour-segment (es-contour-edge)
  ;; just a line segment between adjacent points
  ())

(defclass es-contour-bezier2 (es-contour-edge)
  ;; curve adds a control point between the adjacent points
  ((control-point :initarg :control-point :reader control-point :initform nil)))

(defun enl (n)
  (flet ((p? (p)
           (cond ((eql p n) :<@>)
                 ((eql p :removed) :removed)
                 ((typep p 'es-contour-vertex) (nl (point p)))
                 (t :???))))
    (etypecase n
      (null nil)
      (es-contour-vertex
       (list :v (nl (point n))))
      (es-contour-bezier2
       (list :b (p? (eprev n))
             (when (control-point n) (nl (control-point n)))
             (p? (enext n))))
      (es-contour-segment
       (list :s (p? (eprev n)) (p? (enext n)))))))

(defmethod initialize-instance :after ((n es-contour-node) &key)
  ;; if only 1 of next is bound, set other to same thing. if neither
  ;; is bound, set both to node

  ;; todo: decide if setting only 1 should be an error instead, or if
  ;; we should initialize to NIL when not in a list
  (unless (slot-boundp n 'next)
    (setf (enext n) (if (slot-boundp n 'prev) (eprev n) n)))
  (unless (slot-boundp n 'prev)
    (setf (eprev n) (enext n))))

(defun add-after (node new)
  ;; if new is multiple nodes, return the other end, so calling
  ;; add-after on result will add after all of NEW
  (let ((r (eprev new)))
    (when node
      (psetf (eprev new) node
             (enext node) new
             (enext (eprev new)) (enext node)
             (eprev (enext node)) (eprev new)))
    r))

(defun add-before (node new)
  (let ((r (enext new)))
    (when node
      (psetf (enext new) node
             (eprev node) new
             (eprev (enext new)) (eprev node)
             (enext (eprev node)) (enext new)))
    r))

(defun add-after-* (node &rest new)
  (loop for i in new
        do (setf node (add-after node i))))


(defclass edit-shape ()
  ((contours :initarg :contours
             :initform nil ;; not sure if this is list or vector yet,
             ;; will need to add/remove contours
             :accessor contours)))

(defun make-edit-contour (shape contour)
  (let ((new nil))
    (loop for n = contour then nn
          for nn = (next shape n)
          for end = (eql nn contour)
          do (etypecase n
               (point
                (setf new
                      (add-after new
                                 (make-instance 'es-contour-vertex
                                                :point n))))
               (segment
                (setf new
                      (add-after new
                                 (make-instance 'es-contour-segment))))
               (bezier2
                (setf new
                      (add-after new
                                 (make-instance 'es-contour-bezier2
                                                :control-point (b2-c1 n))))))
          until end)
    new))

(defun degenerate-edit-contour (c)
  (or
   ;; not a contour
   (not c)
   ;; just a point
   (eql c (enext c))
   ;; same start and end points
   (eql (enext c) (eprev c))))

(defun %delete-node (ecn)
  ;; remove ecn from doubly linked list, and return previous node
  (when ecn
    (check-type ecn es-contour-node)
    (if (eql ecn (eprev ecn))
        nil ;; removed last node
        (let ((r (eprev ecn)))
          (psetf (enext (eprev ecn)) (enext ecn)
                 (eprev (enext ecn)) (eprev ecn)
                 ;; not sure if we should try to leave removed node in
                 ;; a valid state so it can be reinserted, or
                 ;; invalidate it to catch problems where we kept
                 ;; reference to it

                 ;; (eprev ecn) ecn
                 ;; (enext ecn) ecn

                 ;; for now making it invalid
                 (eprev ecn) :removed
                 (enext ecn) :removed)
          r))))

(defun collapse-edge (edge)
  (check-type edge es-contour-edge)
  ;; for now just using average of end points, but might be slightly
  ;; better to point on curve at t=0.5 for beziers?
  (unless (degenerate-edit-contour edge)
    (let ((x1 (p-rx (point (eprev edge))))
          (y1 (p-ry (point (eprev edge))))
          (x2 (p-rx (point (enext edge))))
          (y2 (p-ry (point (enext edge))))
          (tmp (enext edge)))
      (assert (typep tmp 'es-contour-vertex))
      (setf tmp (%delete-node tmp))
      (assert (eql tmp edge))
      (setf tmp (%delete-node tmp))
      (assert (typep tmp 'es-contour-vertex))
      (setf tmp (%delete-node tmp))
      (setf edge
            (add-after tmp
                       (make-instance 'es-contour-vertex
                                      :point (make-point (/ (+ x1 x2) 2)
                                                         (/ (+ y1 y2) 2))))))))

(defun map-modifying-contour (contour fun)
  ;; iterate over doubly linked list contour, calling FUN with each
  ;; node. If FUN returns something other than NODE, continue iteration
  ;; with the returned node, otherwise with NEXT node.

  ;; not sure if this should be allowed to visit nodes multiple times,
  ;; or if it can stop at first already visited node without using
  ;; some extra ram to track visited nodes.

  ;; Might be worth visiting multiple times if for example FUN wants to
  ;; match patterns that extend over multiple nodes, and might match
  ;; at a visited node after modifications to a subsequent node.

  ;; Detecting earliest stop without storing all visited nodes might
  ;; be hard if FUN tries to delete more than just the current node,
  ;; so for now just storing a visited hash...

  (when contour
    (let ((visited (make-hash-table)))
      (loop with n = contour
            for n2 = (funcall fun n)
            do (setf (gethash n visited) t)
               (if (eql n n2)
                   (setf n (enext n))
                   (setf n n2))
            while n
            until (and (gethash n visited)
                       ;; stop when current and next node have been
                       ;; visited, so FUN can backtrack by 1 if needed
                       (gethash (enext n) visited))
            finally (return n)))))

(defun %print-contour (start &key max)
  (let ((c 0)
        (m2 (when max (ceiling max 2))))
    (sdf/base::map-modifying-contour
     start (lambda (n)
             (incf c)
             (when (or (not max) (< c m2))
               (format t "  ~s~%" (sdf/base::enl n)))
             n))
    (when (and m2 (>= c m2))
      (let ((c2 (- c m2))
            (s2 start))
        (when (> c2 m2)
          (format t " ... skipped ~s ...~%" (- c2 m2)))
        (setf c2 (min m2 c2))
        (loop repeat c2 do (setf s2 (eprev s2)))
        (loop repeat c2
              do (format t "  ~s~%" (sdf/base::enl s2))
                 (setf s2 (enext s2))
              until (eql s2 start))))))

(defun %print-contours (contours)
  (loop for c in contours
        for i from 0
        do (format t " ~s = ~%" i)
           (%print-contour c)))

(defun %reverse-contour (contour)
  (loop for s = contour then n
        for n = (enext contour) then (enext n)
        do (rotatef (eprev s) (enext s))
        until (eql n contour))
  contour)
