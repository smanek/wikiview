(in-package :wikiview)

(defun range (start finish)
	   (loop for i from start upto finish collecting i))

(defun first-n (list n)
  (loop for element in list
     for i from 1 upto n
     collect element))

(defmacro while (expression &body body)
  `(tagbody
    start (if (not ,expression) (go end))
      ,@body
      (go start)
    end))

(defun squash (statement)
  "returns all atoms in statement as an un-nested list"
  (cond 
   ((null statement) nil)
   ((atom statement) (list statement))
   ('t (append (squash (first statement))
               (squash (rest statement))))))
(defun sq (x)
  (expt x 2))

(defun shuffle-list (l)
  (loop for i below (length l) do
	(rotatef
	 (elt l i)
	 (elt l (random (length l)))))
  l)
