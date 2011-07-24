(in-package :wikiview)

  (defun read-csv-stream (stream &key (state 0) (working-result nil) (results nil) (line nil)) 
    "A finite state machine to parse a csv file (per RFC 4180). Technically, it's a bit
too leniant with nominally malformed files and will attempt to do the right thing instead of just 
failing"
    ;;State 0 is normal unquoted state, in which we're just naively reading non-special characters
    ;;State 1 is quoted state. We're reading all characters
    ;;State 2 is after a second quote has been encountered. A third quote will be interpreted as a literal quote
    (let ((cur-char (read-char stream nil)))
      (cond ((null cur-char) 
             (when (or line working-result) ;;Catch in case csv doesn't have a trailing newline
               (setf results (cons (reverse (cons working-result line)) results))) 
             (reverse results))
            ((and (= state 0) 
                  (equal cur-char #\")) (read-csv-stream stream :state 1 :results results :line line))
            ((and (= state 0) (equal cur-char #\,)) 
             (read-csv-stream stream :state 0 :results results :line (cons working-result line)))
            ((and (= state 0) (equal cur-char #\Newline)) 
             (read-csv-stream stream :state 0 :results (cons (reverse (cons working-result line)) results)))
            ((= state 0) 
             (read-csv-stream stream :state 0 :results results :working-result
                              (concatenate 'string working-result (string cur-char)) :line line))       
            ((and (= state 1) (equal cur-char #\")) 
             (read-csv-stream stream :state 2 :results results :line line :working-result working-result))
            ((= state 1) 
             (read-csv-stream stream :state 1 :results results :line line :working-result 
                              (concatenate 'string working-result (string cur-char))))        
            ((and (= state 2) (equal cur-char #\Newline)) 
             (read-csv-stream stream :state 0 :results (cons (reverse (cons working-result line)) results)))
            ((and (= state 2) (equal cur-char #\,)) 
             (read-csv-stream stream :state 0 :results results :line (cons working-result line)))
            ((and (= state 2) (equal cur-char #\")) 
             (read-csv-stream stream :state 1 :results results :line line :working-result 
                              (concatenate 'string working-result (string cur-char))))
            ((= state 2) 
             (read-csv-stream stream :state 2 :results results :line line))
            ('t 
             (format t "Slipped through the FSM. This should be impossible. Cur-char is ~A state is ~A~%" cur-char state)))))

  (defun read-csv-stream-with-headers (stream)
    "Function builds a list of alists of CSV Records. The first line of the CSV Stream must contain column headers"
    ;;In case I forget later, to query an alist use something like:
    ;;(cdr (assoc "HEADER-NAME" ALIST-NAME :test #'string=))
    (let* ((raw-csv (read-csv-stream stream))
           (csv-headers (car raw-csv))
           (csv-data (cdr raw-csv))
           (rec-size (length csv-headers)))
      ;;ensure all records have all required fields, even if they're nil
      (dolist (record csv-data) (unless (= rec-size (length record)) (return-from read-csv-stream-with-headers nil)))
      (mapcar #'(lambda (record) (map 'list #'cons csv-headers record)) csv-data)))
