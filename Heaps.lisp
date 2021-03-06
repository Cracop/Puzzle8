;Esto fue una implementacion de una binary heap obtenida de: https://github.com/TheRiver/CL-HEAP
(defclass heap ()
  ((sort-fun :initform #'<
	     :reader heap-sorting-function
	     :initarg :sort-fun
	     :documentation "The function used to apply an order to
   elements in the heap.")
   (key :initform #'identity
	:reader heap-key
	:initarg :key
	:documentation "A function used to obtain the elements which
	should be compared to give an order to the items in the
	heap."))
  (:documentation "The base type of the two HEAP implementations."))

;;;--------------------------------------------------------------------
;;; Exported functions.

(defgeneric heap-size (heap)
  (:documentation "Returns the number of elements in the heap."))

(defgeneric add-to-heap (heap item)
  (:documentation "Inserts an item into the given HEAP data
  structure. Returns two values: first, the item added, and then an
  index-key which can be used to identify the item for DECREASE-KEY
  and DELETE-FROM-HEAP operations."))


(defgeneric add-all-to-heap (heap items)
  (:documentation "Adds a list of items to a HEAP. This can typically
  be done more efficiently than by using repeated calls to
  ADD-TO-HEAP, since the heap order only needs to be reinstated after
  all of the items have been inserted, rather than maintained through
  each operation. Returns the heap object."))

(defgeneric peep-at-heap (heap)
  (:documentation "Returns the heap's root, without modifying the
  heap. Returns nil if the heap is empty."))

(defgeneric pop-heap (heap)
  (:documentation "Removes the top element from the heap and returns
  it."))

(defgeneric empty-heap (heap)
  (:documentation "Removes all contents from the given heap. Returns the heap."))

(defgeneric is-empty-heap-p (heap)
  (:documentation "Returns true iff the given heap is empty."))

(defgeneric merge-heaps (first second)
  (:documentation "Returns a new heap that is the merged copy of those
  given here. Can only merge heaps which use the same key and sorting
  function. The two arguments are nt modified by this function."))

(defgeneric nmerge-heaps (first second)
  (:documentation "Destructively updates the arguments to contain the
  merge of both heaps, which is then returned. Can only merge heaps
  which use the same key and sorting function."))

(defgeneric decrease-key (heap item-index value)
  (:documentation "Decreases the value of the item represented by
  ITEM-INDEX. ITEM-INDEX is the index returned by ADD-TO-HEAP for a
  particular item. VALUE is the item's new value, and this must be
  \"less\" than its old value. Returns the heap."))


(defgeneric delete-from-heap (heap item-index)
  (:documentation "Removes the item from the heap represented by the
  ITEM-INDEX. This index is returned as the second value of
  ADD-TO-HEAP. Returns the heap."))

;;;--------------------------------------------------------------------
;;; Unexported functions

(defgeneric compare-items (heap parent child)
  (:documentation "Compares two items, using the HEAP's SORT-FUN and
  KEY functions.")
  (:method ((heap heap) parent child)
    (with-slots (sort-fun key) heap
      (funcall sort-fun (funcall key parent) (funcall key child)))))

;;;--------------------------------------------------------------------
;;; Various implementation details

(defmethod print-object ((heap heap) stream)
  (print-unreadable-object (heap stream :type t :identity t)
    (format stream "Size: ~A" (heap-size heap))))


(defclass priority-queue ()
  ((heap))
  (:documentation "A simple priority queue implementaiton, based on a
  Fibonacci heap."))

(defmethod initialize-instance :after ((queue priority-queue) &key (sort-fun #'<))
  (with-slots (heap) queue
    (setf heap (make-instance 'fibonacci-heap :key #'first :sort-fun sort-fun))))

;;;----------------------------------------------------------------

(declaim (inline enqueue))
(defgeneric enqueue (queue item priority)
  (:documentation "Adds an item with a given priority on to the
  queue. Returns a list containing first the priority, then the
  item. This is a constant time operation.")
  (:method ((queue priority-queue) item priority)
    (with-slots (heap) queue
      (values (add-to-heap heap (list priority item))))))

(declaim (inline dequeue))
(defgeneric dequeue (queue)
  (:documentation "Removes an element from the front of the queue and
  returns it. This is an amortised O(log(n)) operation.")
  (:method (queue)
    (with-slots (heap) queue
      (second (pop-heap heap)))))

(declaim (inline peep-at-queue))
(defgeneric peep-at-queue (queue)
  (:documentation "Returns the element at the front of the queue,
  without modifying the queue. This is a constant time operation.")
  (:method ((queue priority-queue))
    (with-slots (heap) queue
      (second (peep-at-heap heap)))))

(declaim (inline empty-queue))
(defgeneric empty-queue (queue)
  (:documentation "Removes all items from the queue. This is a constant time operation.")
  (:method ((queue priority-queue))
    (with-slots (heap) queue
      (empty-heap heap))))

(declaim (inline queue-size))
(defgeneric queue-size (queue)
  (:documentation "Returns the number of elements in the queue.")
  (:method ((queue priority-queue))
    (with-slots (heap) queue
      (heap-size heap))))


(defclass fibonacci-heap (heap)
  ((root :initform nil
	 :documentation "The minimum element in the tree.")
   (count :initform 0
	  :documentation "The number of items in the heap."))
  (:documentation "A heap made up of item-disjoint, heap-ordered
  trees. Has some good time constraints on various heap operations."))

;;;----------------------------------------------------------------

(defclass node ()
  ((item :initform nil
	 :initarg :item
	 :accessor node-item)
   (parent :initform nil
	   :accessor node-parent)
   (child :initform nil
	  :accessor node-child)
   (rank :initform 0
	 :accessor node-rank
	 :documentation "The number of children the node has.")
   (marked :initform nil
	   :accessor node-marked-p
	   :documentation "Used to implement cascading cuts.")
   (next :initform nil
	 :accessor node-next)
   (last :initform nil
	 :accessor node-last))
  (:documentation "A class used for storing data in a FIBONACCI-HEAP."))

(defmethod initialize-instance :after ((node node) &key)
  (with-slots (next last) node
    (setf next node
	  last node)))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "Item: ~a" (slot-value node 'item))))
      
;;;----------------------------------------------------------------
;;; Unexported functions for handling nodes.

(defgeneric unmark-node (node)
  (:method ((node node))
    (setf (node-marked-p node) nil)))

(defgeneric mark-node (node)
  (:method ((node node))
    (setf (node-marked-p node) t)))

(defgeneric is-node-root-p (node)
  (:method ((node node))
    (null (node-parent node))))

(defgeneric concatenate-node-lists (lhs rhs)
  (:method ((lhs node) (rhs null))
    lhs)
  (:method ((lhs null) (rhs node))
    rhs)
  (:method ((lhs node) (rhs node))
    (psetf (node-next lhs) rhs
	   (node-last (node-next lhs)) (node-last rhs)
	   (node-last rhs) lhs
	   (node-next (node-last rhs)) (node-next lhs))
    lhs))


(defgeneric delete-node (node)
  (:documentation "Deletes this node from the linked list that it
  represents, and returns the new list. Nulls the node's parent, and
  resets its rank if appropriate.")
  (:method ((node null))
    nil)
  (:method ((node node))
    (with-slots (next last parent) node
      (let ((result (when (not (eq next node))
		      next)))
	(when result			; There was something to delete.
	  (psetf (node-last next) last
		 (node-next last) next
		 next node
		 last node))
	(when parent			; Remove the item from any parents.
	  (decf (node-rank parent))
	  (when (eq (node-child parent) node)
	    (setf (node-child parent) result))
	  (setf parent nil))
	result))))



(defmacro do-each-node ((symbol node) &body body)
  (let ((node node)
	(last (gensym))
	(next (gensym)))
      `(when ,node
	 (loop
	    with ,last = (node-last ,node)
	    for ,symbol = ,node then ,next
	    for ,next = (node-next ,node) then (node-next ,next)
	    while (not (eq ,symbol ,last))
	    do (progn
		 ,@body)
	    finally (progn
		      ,@body)))))


;;;--------------------
;;; Unexported functions

(defgeneric meld (one two)
  (:documentation "Joins together two fibonacci heaps."))

;; This should not increase the heap's count of its items, since it's
;; used in areas such as linking, where this must not occur.
(defmethod meld ((heap fibonacci-heap) (item node))
  "Adds a node to the heap."
  (with-slots (root) heap
    (cond
      ((null root)
       (setf root item))
      ((compare-items heap (node-item root) (node-item item))
       (setf root (concatenate-node-lists root item)))
      (t
       (setf root (concatenate-node-lists item root)))))
  heap)

;; This should adjust the heap's count of its children, since it's use
;; only makes sense in places where more items are added.
(defmethod meld ((heap1 fibonacci-heap) (heap2 fibonacci-heap))
  (with-slots ((heap1-root root)
	       (heap1-count count)) heap1
    (with-slots ((heap2-root root)
		 (heap2-count count)) heap2
      (setf heap1-root (concatenate-node-lists heap1-root heap2-root))
      (unless (compare-items heap1 (node-item heap1-root) (node-item heap2-root))
	(setf heap1-root heap2-root
	      heap1-count (+ heap1-count heap2-count))))))

(defgeneric link (heap node-one node-two)
  (:documentation "Places node-two as a child of node-one if
  node-one's item is smaller, or vice versa.")
  (:method ((heap fibonacci-heap) (node-one node) (node-two node))
    (with-slots ((one-child child)
		 (one-item item)
		 (one-rank rank)) node-one
      (with-slots ((two-child child)
		   (two-item item)
		   (two-rank rank)) node-two
	(cond
	  ((compare-items heap one-item two-item)
	   (delete-node node-two)
	   (unless (is-node-root-p node-two)
	     (unmark-node node-two))
	   (setf one-child (concatenate-node-lists one-child node-two)
		 (node-parent node-two) node-one)
	   (incf one-rank)
	   node-one)
	  (t
	   (delete-node node-one)
	   (setf two-child (concatenate-node-lists two-child node-one)
		 (node-parent node-one) node-two)
	   (incf two-rank)
	   node-two))))))

(defgeneric cut-node (heap node)
  (:documentation "Cuts a child from its parent and makes and places
  it in the root list.")
  (:method ((heap fibonacci-heap) (node node))
    (let ((parent (node-parent node)))
      (with-slots (root) heap
	(delete-node node)
	(concatenate-node-lists root node)
	(cond
	  ((and parent (not (is-node-root-p parent)) (node-marked-p parent))
	   (cut-node heap parent))
	  ((and parent (not (is-node-root-p parent)))
	   (mark-node parent)
	   heap))))))
      

;;;----------------------------------------------------------------
;;; Exported Functions

(defmethod empty-heap ((heap fibonacci-heap))
  "Clears all items from the heap. This is a constant time operation."
  (with-slots (root count) heap
    (setf root nil
	  count 0))
  heap)

(defmethod is-empty-heap-p ((heap fibonacci-heap))
  (unless (slot-value heap 'root)
    t))

(defmethod heap-size ((heap fibonacci-heap))
  (slot-value heap 'count))

(defmethod add-to-heap ((heap fibonacci-heap) item)
  "Adds an item to a Fibonacci-heap. This is a constant time
operation. Returns the item added to the heap."
  (let ((node (make-instance 'node :item item)))
  (meld heap node)
  (incf (slot-value heap 'count))
  (values item node)))

(defmethod add-all-to-heap ((heap fibonacci-heap) (items list))
  "Adds the following list of items into the heap. This is an O(n) operation."
  (with-slots (count) heap
    (loop for i in items
       do (progn
	    (meld heap (make-instance 'node :item i))
	    (incf count))))
  heap)

(defmethod peep-at-heap ((heap fibonacci-heap))
  "See the heap's minimum value without modifying the heap. This is a
constant time operation."
  (with-slots (root) heap
    (when root
      (node-item root))))

(defmethod pop-heap ((heap fibonacci-heap))
  "Remove the minimum element in the tree. This has an amortised
running time of O(log(n)), where n is the number of items in the
heap."
  (unless (is-empty-heap-p heap)
    (let ((item (peep-at-heap heap)))
      (with-slots (root count) heap
	;; Delete the minimum.
	(concatenate-node-lists root (node-child root))
	(setf root (delete-node root))
	(when root
	  (let ((ranks (make-array (1+ (ceiling (log count 2)))  :initial-element nil))
		(min nil))
	    ;; Merge all trees of the same rank.
	    (labels ((sort-node (node)
		       (let ((position (node-rank node)))
			 (cond
			   ((aref ranks position)
			    (let ((new (link heap node (aref ranks position))))
			      (setf (aref ranks position) nil)
			      (sort-node new)))
			   (t
			    (setf (aref ranks position) node))))))
	      (do-each-node (node root)
		;; The newly added nodes should not have a parent
		(setf (node-parent node) nil)
		(delete-node node)
		(sort-node node)))
	    (loop for tree across ranks
	       do (when (not (null tree))
		    (cond
		      ((null min)
		       (setf min tree))
		      ((compare-items heap
				      (node-item min)
				      (node-item tree))

		       (setf min (concatenate-node-lists min tree)))
		      (t
		       (setf min (concatenate-node-lists tree min))))))
	    (setf root min)))
	(decf (slot-value heap 'count))
	item))))


(defmethod nmerge-heaps ((first fibonacci-heap) (second fibonacci-heap))
  "Destructively marges the two heaps. This is a constant time
operation."
  (with-slots ((first-key key)
	       (first-fun sort-fun)) first
    (with-slots ((second-key key)
		 (second-fun sort-fun)) second
      (unless (and (eq first-key second-key)
		   (eq first-fun second-fun))
	(error 'heap-error :message "These two heaps were constructed using different
	access keys and sorting functions."))))
  (meld first second)
  first)

(defmethod merge-heaps ((first fibonacci-heap) (second fibonacci-heap))
  "Returns the merge of the two given heaps. This operation runs in
O(n + m), where n and m are the number of items in each heap."
  (with-slots ((first-root root)
	       (first-key key)
	       (first-fun sort-fun)) first
    (with-slots ((second-root root)
		 (second-key key)
		 (second-fun sort-fun)) second
      (unless (and (eq first-key second-key)
		   (eq first-fun second-fun))
	(error 'heap-error :message "These two heaps were constructed using different
	access keys and sorting functions."))
      (let ((result (make-instance 'fibonacci-heap
				   :sort-fun first-fun
				   :key first-key)))
	(labels ((add-from-level (node-list)
		   (when node-list
		     (do-each-node (node node-list)
		       (add-from-level (node-child node))
		       (add-to-heap result (node-item node))))))
	  (add-from-level first-root)
	  (add-from-level second-root))
	result))))

;;; This method decreases the node's key, removes the node from the
;;; tree and adds it to the root list (unless this is of course where
;;; the node originally was.
(defmethod decrease-key ((heap fibonacci-heap) (item-index node) value)
  "Changes the value of an item represented by the ITEM-INDEX to
  VALUE. This index is returned as the second argument to
  ADD-TO-HEAP. This is an amortised constant time operation."
  (with-slots (key sort-fun) heap
    (unless (funcall sort-fun value (funcall key (node-item item-index)))
      (error 'key-error :message
	     (format nil "The given value (~a) must be less than the current value (~a)."
	     value (funcall key (node-item item-index)))))
    (if (eq key #'identity)
	(setf (node-item item-index) value)
	(handler-case 
	    (funcall key (node-item item-index) value)
	  (error (e)
	    (declare (ignore e))
	    (error 'key-error))))
    (cond
      ;; A child of something. See if cascading cuts should occur.
      ((node-parent item-index)		
       (let ((parent (node-parent item-index)))
	 (delete-node item-index)
	 (meld heap item-index)
	 (when (not (is-node-root-p parent))
	   (if (node-marked-p parent)
	       (cut-node heap parent)
	       (mark-node parent)))))
      (t				; In the list with the root.
       (with-slots (root) heap
	 (unless (compare-items heap (node-item root) (node-item item-index))
	   (setf root item-index))))))
  heap)

(defmethod delete-from-heap ((heap fibonacci-heap) (item-index node))
  "Removes an item from the heap, as pointed to by item-index. This
  operation is amortised O(1), unless the item removed is the minimum item, in
  which case the operation is equivalent to a POP-HEAP."
  (with-slots (root count) heap
    (let ((parent (node-parent item-index)))
      (cond
	((eq root item-index)
	 (pop-heap heap))
	(t
	 (do-each-node (child (node-child item-index))
	   (setf (node-parent child) nil))
	 ;; Add children to root level.
	 (concatenate-node-lists root (node-child item-index))
	 (delete-node item-index)
	 (decf count)))
      (when (and parent (not (is-node-root-p parent)))
	(if (node-marked-p parent)
	    (cut-node heap parent)
	    (mark-node parent)))))
  heap)

  