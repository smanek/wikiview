(in-package :wikiview)

(defclass node ()
  ((real-name          :initarg :real-name
                       :reader real-name
                       :initform (error "Must supply a node's real-name"))
   (display-name       :accessor display-name)
   (links-to           :initarg :links-to
                       :initform nil
                       :accessor links-to)))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (real-name display-name) object
      (format stream "~s :display-name ~s" real-name display-name))))

(defun wiki-to-display (wiki-name)
  (cl-ppcre:regex-replace-all "_" wiki-name " "))

(defun display-to-wiki (wiki-name)
  (cl-ppcre:regex-replace-all " " wiki-name "_"))

(defmethod initialize-instance :after ((node node) &key)
  (setf (slot-value node 'display-name) (wiki-to-display (real-name node))))

(defun build-network-around (name depth max)
  ;;max*depth is a heuristic, that produces a reasonable maximum number of children per node
  (make-instance 'node :real-name name
                       :links-to (when (< 0 depth) 
                                   (loop for neighbour in (shuffle-list (mapcar #'page-title-to 
                                                                                (remove-if #'null 
                                                                                           (neighbours (get-cluster-by-name name)))))
                                      for j from 1 upto (* max depth)
                                      collect (build-network-around neighbour (1- depth) (1- max))))))