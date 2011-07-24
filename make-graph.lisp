(in-package :wikiview)

(defun generate-image (filetype node-name)
  "this is the function that should be called. Everything else just helps this one"
  (let ((filename (next-filename filetype)))
    (create-image filetype filename node-name)
    filename))

(defmethod node-link-string ((parent node) (child node))
  (format nil "~T\"ID=~A\" -> \"ID=~A\";~%" (real-name parent) (real-name child)))

(defmethod node-string ((node node) fontsize)
  (format nil "~Tnode[label=\"~A\", fontsize=\"~A\"]{\"ID=~A\"};~%" 
          (display-name node) fontsize (real-name node)))

(let ((image-count 0))
  (defun next-filename (filetype)
    "This function sets up image rotation (a 100 image cycle) so I don't fill up my HD"
    (if (> image-count 99)
        (setf image-count 0)
        (incf image-count))
    (format nil "~A.~A" image-count filetype)))

(defun create-image (filetype filename node-name)
  (clsql-sys:reconnect) ;;make sure the db connection is still up
  (sb-ext:run-program *twopi-binary*
                      (list (concatenate 'string  "-T" filetype) 
                            "-o" 
                            (concatenate 'string *image-directory* filename)) 
                      :input (make-string-input-stream (create-dot node-name))))

(let ((nodes-seen-cache (make-hash-table :test 'equal))) 
  (defun create-dot (node-name)
    (let ((root (build-network-around node-name *default-depth* *default-children*)))
      (setf nodes-seen-cache (make-hash-table :test 'equal))
      (if (null root)
          nil
          (concatenate 'string (make-dot-helper root) 
                       (format nil "~Troot=\"ID=~A\";~%}" 
                               (real-name root))))))
  
  (defun write-file (file-name dot-string)
    (with-open-file (stream (concatenate 'string  
                                         *image-directory*
                                         file-name) 
                            :direction :output 
                            :if-exists :supersede)
      (format stream dot-string))
    t)
  
  (defmethod make-dot-helper ((root node) &optional 
                              (depth 1) 
                              (res (format nil "digraph g {~%overlap=\"false\";~%~A" (node-string root 60))))
    (loop for child in (links-to root)
       do (progn (setf res 
                       (concatenate 'string 
                                    res 
                                    (when (null (gethash (real-name child) nodes-seen-cache))
                                      (setf (gethash (real-name child) nodes-seen-cache) t)
                                      ;;nodes get smaller further away from the root
                                      (node-string child (floor (* 7 (/ 5 depth))))) 
                                    (node-link-string root child)))
                 (setf res (make-dot-helper child (1+ depth) res))))
    res))