(in-package :wikiview)
;;Use "correct-spelling" to suggest one correction to a word.
;;Use "list-spelling-corrections" to get a list of possible corrections in order of likelyhood
;;Use "list-completions" to get a list of possible completions in order of likelyhood

;This is fairly easy to intuitively understand by looking at the code, but the theoretical foundation is Bayes' Theorem
;We are trying to find the correction, 'c', which maximizes the probability of c given a word 'w'.
;which is expressed as max(c) P(c|w) 
;The notation on the max(c) is a bit sloppy ... but you see what I mean
;by Bayes' Theorem this is equivalent to:
;max(c) P(w|c) * P(c) / P(w) 
;Since P(w) is the same for every possible c, we can ignore it, giving:
;maxc P(w|c) * P(c) 
;Which is what this program attempts to compute

(defclass trie-node ()
  ((children-nodes    :initarg :children-nodes
                      :accessor children-nodes
                      :initform nil
                      :documentation "A list of this nodes children")
   (node-score        :initform 0
                      :initarg :node-score
                      :accessor node-score
                      :documentation "How many words seen so far that terminate on this node.")
   (capitalization    :initarg :capitalization
                      :accessor capitalization
                      :initform nil
                      :documentation "If a word terminates on this node this is the correct cap.")
   (letter            :initform ""
                      :initarg :letter
                      :reader letter)))

(defmethod print-object ((object trie-node) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (node-score capitalization letter) object
      (format stream "~A~TScore: ~A~TCapitalization: \"~A\"" letter node-score capitalization))))

(defvar *root-node* (make-instance 'trie-node))

(let ((alphabet (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))

  (defmethod %add-child ((parent trie-node) letter)
    "Adds a new node to the trie, with a parent node of parent"
    (let ((child (make-instance 'trie-node :letter letter)))
      (push child (children-nodes parent))
      child))

  (defmethod %find-direct-child ((parent trie-node) letter)
    "Checks if 'parent' has a child with matching 'letter'"
    (loop for child in (children-nodes parent) 
       when (string-equal letter (letter child)) 
       do (return child) finally (return nil)))

  (defun add-word (word &key (weight 1))
    (loop for letter in (concatenate 'list word) 
       with current-node = *root-node*
       do
       (let ((isChild (%find-direct-child current-node letter)))
         (if isChild
             (setf current-node isChild)
             (setf current-node (%add-child current-node letter))))
       finally (progn  
                 (setf (node-score current-node) (+ weight (node-score current-node)))
                 (setf (capitalization current-node) word))))

  (defmethod %pre-order ((root trie-node))
    (let ((res (list root))) 
      (loop for child in (children-nodes root) do (setf res (append res (%pre-order child))))
      res))
  
  (defmethod %pre-order ((null null))
    nil)

  
  (defmethod %find-nodes (str)
    "Find all words, starting with str"
    (let ((top-nodes (%pre-order (%get-node-at str))))     
      (loop for node in top-nodes when (> (node-score node) 0) 
         collect (cons (node-score node) (capitalization node)))))
  
  (defun %get-node-at (word)
    (loop for letter in (concatenate 'list word) 
       with current-node = *root-node* do
         (let ((isChild (%find-direct-child current-node letter)))
           (if isChild
               (setf current-node isChild)
               (return-from %get-node-at nil)))
       finally (return current-node)))

  (defun list-completions (start)
    "List possible completions for the string starting with 'start'"
    (assert (typep start 'string))
    (assert (not (string= "" start)))
    (first-n (mapcar #'(lambda (x) (cdr x)) 
                     (sort (%find-nodes start) #'(lambda (x y) (> (car x) (car y)))))
             *num-autocompletions*))

  (defun clear-trie ()
    "clear the trie"
    (setf *root-node* (make-instance 'trie-node)))

  (defun get-root-node ()
    *root-node*)

  (defun %unique (lst)
    "Returns a list of unique elements in lst"
    (let ((uniq (make-hash-table :test #'equalp)))
      (loop for item in lst do (setf (gethash item uniq) t) 
         finally (return 
                   (loop for keys being the hash-keys of uniq 
                      collect keys into res finally (return res))))))

  (defun %edit-distance-one (x)
    "All the strings within an edit distance of 1 of x. ~80% of mis-spellings are within an edit distance of one of the target"
    (declare (optimize (speed 3) (safety 0) (debug 0)) (type (string) x))
    (setf x x)
    (let ((results nil)
          (len (1- (length x))))
                                        ;transposition
      (loop for i from 0 upto (1- len) do
           (push (concatenate 'string (subseq x 0 i) (string (schar x (1+ i))) (string (schar x i)) (subseq x (+ 2 i))) results))
    
      (loop for i from 0 upto len do
           (progn
             ;;deletion 
             (push (concatenate 'string (subseq x 0 i) (subseq x (1+ i))) results)
             (loop for letter in alphabet do
                  (progn
                    ;;alteration
                    (push (concatenate 'string (subseq x 0 i) letter (subseq x (1+ i))) results)
                    ;;insertion
                    (push (concatenate 'string (subseq x 0 i) letter (subseq x i)) results)))))
      results))

  (defun %edit-distance-two (x)
    "All the strings within an edit distance of 2 of x. ~99% of mis-spellings are within an edit distance of two of the target"
    (declare (optimize (speed 3) (safety 0) (debug 0)) (type (string) x))
    (mapcan #'%edit-distance-one (%edit-distance-one x)))

  (defun %known-filter (x)
    "Returns all the strings in list x that are in the dictionary"
    (loop for word in x 
       with results = nil
       when (and (%get-node-at word) 
                 (< 0 (node-score (%get-node-at word))))
       do (setf results (cons (cons (node-score (%get-node-at word)) word) results))
       finally (return results)))

  (defun correct-spelling (x)
    "Return our best guess at the correct spelling of the string x"
    (let ((possible (or (%known-filter (list x)) ;;I chose to have it always infinetly prefer a correction with a lower edit distance
                        (%known-filter (%edit-distance-one x))
                        (%known-filter (%edit-distance-two x))
                        (list (cons 1 x)))))
      (loop for word in possible with max = 0 with result
         when (> (car word) max) 
         do (progn 
              (setf max (car word)) 
              (setf result (cdr word)))
         finally (return result))))


  (defun list-spelling-corrections (x)
    "Provide a list of possible corrections of the string x, in order of liklihood"
    (mapcar #'(lambda (x) (cdr x))  
            (sort (%unique (or (%known-filter (list x)) 
                               (%known-filter (%edit-distance-one x)) 
                               (%known-filter (%edit-distance-two x))
                               (list (cons 1 x))))
                  #'(lambda (x y) 
                      (if (> (car x) (car y))
                          t
                          nil))))))