;; original by mfiano
;; https://gist.github.com/mfiano/0b84ce94b66fe6739fdff2856811ae0a
;; https://gist.github.com/mfiano/3f1d272e022e3c9f20736667bed858f6
;; https://gist.github.com/mfiano/d414fed2ef95bae3d62156ef2cede011
;; MIT license
(defpackage #:sdf/rb
  (:use :cl)
  (:shadow #:find #:min #:max)
  (:export #:make-tree
           #:insert
           #:find
           #:delete-node
           #:previous
           #:next
           #:min
           #:max
           #:value)
  (:local-nicknames (#:a #:alexandria-2)))
(in-package #:sdf/rb)

(defmacro while (cond &body body)
  `(loop while ,cond do (progn ,@body)))

(defmacro fn-> (name (&rest args) ret)
  `(declaim (ftype (function (,@args) ,ret) ,name)))

(defstruct (tree
            (:constructor %make-tree)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (sentinel nil :type (or node null))
  (root nil :type (or node null))
  (key-func #'identity :type function)
  (sort-func #'< :type function))

#++
(u:define-printer (tree stream :type nil)
    (format stream "RED-BLACK-TREE"))
(defmethod print-object ((o tree) stream)
  (print-unreadable-object (o stream :identity t)
    (format stream "rb tree")))

(declaim (inline %make-node))
(defstruct (node
            (:constructor %make-node)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (tree nil :type (or tree symbol))
  (parent nil :type (or node null))
  (left nil :type (or node null))
  (right nil :type (or node null))
  (color :black :type (member :red :black))
  (key nil :type t)
  (value nil :type t))

#++
(u:define-printer (node stream :type nil)
    (format stream "RED-BLACK-TREE-NODE"))
(defmethod print-object ((o node) stream)
  (print-unreadable-object (o stream :identity t)
    (format stream "rb tree node")))

(fn-> make-node (tree t) node)
(defun make-node (tree item)
  (declare (optimize speed))
  (let ((node (%make-node :tree tree
                          :key (when item (funcall (key-func tree) item))
                          :value item)))
    (setf (parent node) node
          (left node) node
          (right node) node)
    node))

(fn-> make-tree (&key (:key-func function) (:sort-func function)) tree)
(defun make-tree (&key (key-func #'identity) (sort-func #'<))
  (declare (optimize speed))
  (let* ((tree (%make-tree :key-func key-func :sort-func sort-func))
         (sentinel (make-node tree nil)))
    (setf (sentinel tree) sentinel
          (root tree) sentinel)
    tree))

(fn-> node-p ((or node null)) (or node null))
(declaim (inline node-p))
(defun node-p (node)
  (declare (optimize speed))
  (unless (and node (eq node (sentinel (tree node))))
    node))


(fn-> walk (tree function) null)
(defun walk (tree func)
  (declare (optimize speed))
  (a:when-let ((node (node-p (root tree))))
    (loop :with current = node
          :with stack
          :do (cond
                ((node-p current)
                 (push current stack)
                 (setf current (left current)))
                (stack
                 (setf current (pop stack))
                 (funcall func (value current))
                 (setf current (right current)))
                (t (loop-finish))))))

(defun walkn (tree func)
  (declare (optimize speed))
  (a:when-let ((node (node-p (root tree))))
    (loop :with current = node
          :with stack
          :do (cond
                ((node-p current)
                 (push current stack)
                 (setf current (left current)))
                (stack
                 (setf current (pop stack))
                 (funcall func current)
                 (setf current (right current)))
                (t (loop-finish))))))

(defvar *n* nil)
(defun walk2 (tree func)
  (a:when-let ((node (node-p (root tree))))
    #++(loop :with current = node
             :with stack
             :do (when (node-p current)
                   (funcall func (value current) (length stack)))
                 (cond
                   ((node-p current)
                    (push current stack)
                    (setf current (left current)))
                   (stack
                    (setf current (pop stack))
                    (setf current (right current)))
                   (t (loop-finish))))
    (labels ((r (n d)
               (when (node-p n)
                 (let ((*n* n))
                   (funcall func (value n) d))
                 (r (left n) (1+ d))
                 (r (right n) (1+ d)))))
      (r node 0))))

(fn-> find (tree t) (values t &optional node))
(defun find (tree item)
  "search tree for item, if found, return value and node containing it"
  (declare (optimize speed))
  (labels ((%find (node key sort-func)
             (declare (function sort-func))
             (a:when-let ((result (and (node-p node) (key node))))
               (cond
                 ((funcall sort-func key result)
                  (%find (left node) key sort-func))
                 ((funcall sort-func result key)
                  (%find (right node) key sort-func))
                 (t node)))))
    (a:when-let ((node (%find (root tree)
                              (funcall (key-func tree) item)
                              (sort-func tree))))
      (values (value node)
              node))))

(defun find* (tree item)
  "search tree for item. If found, return (values node nil nil),
else (values nil prev next), where PREV,NEXT are nodes in tree
before/after ITEM."
  (declare (optimize speed))
  (labels ((ret (node left)
             (if left
                 (return-from find*
                   (values nil (previous node) node))
                 (return-from find*
                   (values nil node (next node)))))
           (%find (node key sort-func)
             (declare (function sort-func))
             (let ((result (and (node-p node) (key node))))
               (cond
                 ((not result)
                  (return-from find* (values nil nil nil)))
                 ((funcall sort-func key result)
                  (let ((l (left node)))
                    (if (and l (node-p l))
                        (%find l key sort-func)
                        (ret node t))))
                 ((funcall sort-func result key)
                  (let ((r (right node)))
                    (if (and r (node-p r))
                        (%find r key sort-func)
                        (ret node nil))))
                 (t node)))))
    (declare (dynamic-extent #'ret))
    (values (%find (root tree)
                   (funcall (key-func tree) item)
                   (sort-func tree))
            nil nil)))


(fn-> rotate-left (tree node) null)
(defun rotate-left (tree node)
  (declare (optimize speed))
  (let ((right (right node)))
    (setf (right node) (left right))
    (when (node-p (left right))
      (setf (parent (left right)) node))
    (setf (parent right) (parent node))
    (cond
      ((not (node-p (parent node)))
       (setf (root tree) right))
      ((eq node (left (parent node)))
       (setf (left (parent node)) right))
      (t
       (setf (right (parent node)) right)))
    (setf (left right) node
          (parent node) right)
    nil))

(fn-> rotate-right (tree node) null)
(defun rotate-right (tree node)
  (declare (optimize speed))
  (let ((left (left node)))
    (setf (left node) (right left))
    (when (node-p (right left))
      (setf (parent (right left)) node))
    (setf (parent left) (parent node))
    (cond
      ((not (node-p (parent node)))
       (setf (root tree) left))
      ((eq node (right (parent node)))
       (setf (right (parent node)) left))
      (t
       (setf (left (parent node)) left)))
    (setf (right left) node
          (parent node) left)
    nil))

(fn-> insert-fixup (tree node) null)
(defun insert-fixup (tree node)
  (declare (optimize speed))
  (let ((node node))
    (while (eq (color (parent node)) :red)
      (if (eq (parent node) (left (parent (parent node))))
          (let ((y (right (parent (parent node)))))
            (cond
              ((eq (color y) :red)
               (setf (color (parent node)) :black
                     (color y) :black
                     (color (parent (parent node))) :red
                     node (parent (parent node))))
              (t
               (when (eq node (right (parent node)))
                 (setf node (parent node))
                 (rotate-left tree node))
               (setf (color (parent node)) :black
                     (color (parent (parent node))) :red)
               (rotate-right tree (parent (parent node))))))
          (let ((y (left
                    (parent (parent node)))))
            (cond
              ((eq (color y) :red)
               (setf (color (parent node)) :black
                     (color y) :black
                     (color (parent (parent node))) :red
                     node (parent (parent node))))
              (t
               (when (eq node (left (parent node)))
                 (setf node (parent node))
                 (rotate-right tree node))
               (setf (color (parent node)) :black
                     (color (parent (parent node))) :red)
               (rotate-left tree (parent (parent node)))))))))
  (setf (color (root tree)) :black)
  nil)

(fn-> insert (tree t) node)
(defun insert (tree item)
  (declare (optimize speed))
  (flet ((%insert (tree node)
           (let ((sort-func (sort-func tree))
                 (x (root tree))
                 (y (sentinel tree)))
             (while (node-p x)
               (setf y x)
               (if (funcall sort-func (key node) (key x))
                   (setf x (left x))
                   (setf x (right x))))
             (setf (parent node) y)
             (cond
               ((not (node-p y))
                (setf (root tree) node))
               ((funcall sort-func (key node) (key y))
                (setf (left y) node))
               (t
                (setf (right y) node)))
             (setf (left node) (sentinel tree)
                   (right node) (sentinel tree)
                   (color node) :red)
             (insert-fixup tree node))))
    (let ((node (make-node tree item)))
      (%insert tree node)
      node)))

(fn-> transplant (tree node node) null)
(defun transplant (tree u v)
  (declare (optimize speed))
  (cond
    ((not (node-p (parent u)))
     (setf (root tree) v))
    ((eq u (left (parent u)))
     (setf (left (parent u)) v))
    (t
     (setf (right (parent u)) v)))
  (setf (parent v) (parent u))
  nil)

(fn-> delete-fixup (tree node) null)
(defun delete-fixup (tree node1)
  (declare (optimize speed))
  (let ((x node1))
    (while (and (not (eq x (root tree)))
                (eq (color x) :black))
      (if (eq x (left (parent x)))
          (let ((w (right (parent x))))
            (when (eq (color w) :red)
              (setf (color w) :black
                    (color (parent x)) :red)
              (rotate-left tree (parent x))
              (setf w (right (parent x))))
            (cond
              ((and (eq (color (left w)) :black)
                    (eq (color (right w)) :black))
               (setf (color w) :red
                     x (parent x)))
              (t
               (when (eq (color (right w)) :black)
                 (setf (color (left w)) :black
                       (color w) :red)
                 (rotate-right tree w)
                 (setf w (right (parent x))))
               (setf (color w) (color (parent x))
                     (color (parent x)) :black
                     (color (right w)) :black)
               (rotate-left tree (parent x))
               (setf x (root tree)))))
          (let ((w (left (parent x))))
            (when (eq (color w) :red)
              (setf (color w) :black
                    (color (parent x)) :red)
              (rotate-right tree (parent x))
              (setf w (left (parent x))))
            (cond
              ((and (eq (color (right w)) :black)
                    (eq (color (left w)) :black))
               (setf (color w) :red
                     x (parent x)))
              (t
               (when (eq (color (left w)) :black)
                 (setf (color (right w)) :black
                       (color w) :red)
                 (rotate-left tree w)
                 (setf w (left (parent x))))
               (setf (color w) (color (parent x))
                     (color (parent x)) :black
                     (color (left w)) :black)
               (rotate-right tree (parent x))
               (setf x (root tree)))))))
    (setf (color x) :black))
  nil)

(fn-> delete-node (tree node) null)
(defun delete-node (tree node)
  (declare (optimize speed))
  (flet ((minimum (x)
           (while (node-p (left x))
             (setf x (left x)))
           x))
    (let* ((x nil)
           (y node)
           (y-color (color y)))
      (cond
        ((not (node-p (left node)))
         (setf x (right node))
         (transplant tree node (right node)))
        ((not (node-p (right node)))
         (setf x (left node))
         (transplant tree node (left node)))
        (t
         (setf y (minimum (right node))
               y-color (color y)
               x (right y))
         (cond
           ((eq (parent y) node)
            (setf (parent x) y))
           (t
            (transplant tree y (right y))
            (setf (right y) (right node)
                  (parent (right y)) y)))
         (transplant tree node y)
         (setf (left y) (left node)
               (parent (left y)) y
               (color y) (color node))))
      (when (eq y-color :black)
        (delete-fixup tree x))
      (setf (tree node) :deleted)
      nil)))


;; needs updates
(defun min (node)
  (when (typep node 'tree)
    (setf node (root node)))
  (when (node-p node)
    (loop :for current = (left node)
          :while (node-p current)
          :do (setf node current)
          :finally (return node))))

(defun max (node)
  (when (typep node 'tree)
    (setf node (root node)))
  (when (node-p node)
    (loop :for current = (right node)
          :while (node-p current)
          :do (setf node current)
          :finally (return node))))

(defun previous (node)
  (when (node-p node)
    (a:if-let ((left (node-p (left node))))
      (max left)
      (loop :for current = node :then parent
            :for parent = (parent current)
            :while (and (node-p parent)
                        (eq current (left parent)))
            :finally (return (node-p parent))))))

(defun next (node)
  (when (node-p node)
    (a:if-let ((right (node-p (right node))))
      (min right)
      (loop :for current = node :then parent
            :for parent = (parent current)
            :while (and (node-p parent)
                        (eq current (right parent)))
            :finally (return (node-p parent))))))

#++
(let ((rb (make-tree))
      (l (a:shuffle (a:iota 32))))
  (flet ((w ()
           (let ((r nil))
             (walk rb (lambda (a) (push a r)))
             (nreverse r))))
    (loop for i in l
          do (insert rb i)
             (assert (equalp (print (w))
                             (loop for i = (min rb) then (next i)
                                   while i
                                   collect (value i)))))))

(defun %red-black-tree/check-invariants (tree)
  "Checks the red-black-tree conditions for the tree/subtree with root ROOT.
Returns the black depth of the tree on success.
TODO: Put in red-black tree tests."
  (labels ((recur (node)
             (if (node-p node)
                 (let ((a (recur (left node)))
                       (b (recur (right node)))
                       (parcheck t))
                   (when (node-p (left node))
                     (setf parcheck (eq (parent (left node)) node)))
                   (when (node-p (right node))
                     (setf parcheck (and parcheck
                                         (eq (parent (right node)) node))))
                   (when (and parcheck a b (= a b))
                     (if (eq :black (color node))
                         (1+ a)
                         (when (and (eq :black (color (left node)))
                                    (eq :black (color (right node))))
                           a)))))))
    (and (eq (color (root tree)) :black)
         (recur (root tree)))))

(defun to-list (tree)
  (let ((r nil))
    (walk tree (lambda (a) (push a r)))
    (nreverse r)))

(defun %map-range (tree func start-func end-func)
  "call FUNC with values of all nodes (in order) of TREE where
 (AND (START-FUNC (VALUE NODE))
      (END-FUNC (VALUE NODE)))"
  (declare (optimize speed))
  (let ((node (root tree))
        (stack nil)
        (func (coerce func 'function))
        (start-func (coerce start-func 'function))
        (end-func (coerce end-func 'function)))
    (when (node-p node)
      (loop :do (cond
                  ((and (node-p node) (funcall start-func (value node)))
                   (when (funcall end-func (value node))
                     (push node stack))
                   (setf node (left node)))
                  ((node-p node)
                   (setf node (right node)))
                  (stack
                   (setf node (pop stack))
                   (funcall func (value node))
                   (setf node (right node)))
                  (t (loop-finish)))))))


