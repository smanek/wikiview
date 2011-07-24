(in-package :wikiview)

(defun build-autocomplete-cache ()
  (loop for word in (with-open-file (stream "/home/lispserver/site/popularity.csv")
                      (read-csv-stream stream))
     do (add-word (car word) :weight (parse-integer (cadr word)))))

(defun start ()
  (setf *db-connection* (clsql-user:connect 
                         (list *db-host* *db-name* *db-user* *db-pass*) 
                         :database-type *db-type* 
                         :if-exists :old))
  (setf *server* (hunchentoot:start-server :port *server-port*))
  (build-autocomplete-cache))
