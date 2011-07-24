(in-package :wikiview)

;;enable hunchentoot to serve static files.
;;I wanted the static files to be served by apache, but I don't have enough RAM to run apache
(push 
 (hunchentoot:create-folder-dispatcher-and-handler "/static/" #P "/home/lispserver/site/document-root/static/")
 hunchentoot:*dispatch-table*)

;;enable the easy-handlers
(push #'hunchentoot:dispatch-easy-handlers hunchentoot:*dispatch-table*)

;;enable debugging output. I'd turn this off in a production environment
(setf hunchentoot:*show-lisp-errors-p* t
      hunchentoot:*show-lisp-backtraces-p* t)


(hunchentoot:define-easy-handler (home :uri "/"
                                       :default-request-type :get) 
    ()
  (with-output-to-string (stream)  
    (html-template:fill-and-print-template 
     #p"index.htmlf" 
     nil
     :stream stream)))

(hunchentoot:define-easy-handler (generate-dot :uri "/generate-dot"
                                               :default-request-type :get)
    ((query-name :parameter-type 'string))
  (format nil "<a href=\"/static/images/~A\">Result Graph</a><br/>I'd suggest you download this file and view it in <a href=\"http://www.inkscape.org/download/?lang=en\">Inkscape</a> - it can crash some (inferior) browsers"
          (generate-image "svg" (display-to-wiki query-name))))

(hunchentoot:define-easy-handler (autocomplete :uri "/autocomplete"
                                               :default-request-type :get)
    ((str :parameter-type 'string))
  (format nil "[~{~s, ~}]" (mapcar #'wiki-to-display (list-completions (display-to-wiki str)))))