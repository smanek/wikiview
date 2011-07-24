(in-package :cl-user)

(defpackage :wikiview
  (:use :cl :asdf))

(in-package :wikiview)

(defsystem :wikiview
  :depends-on (:hunchentoot :html-template :cl-json :clsql :cl-who :cl-ppcre)
  :version "0.1"
  :serial t
  :components
  ((:file "global") ;;global variables and settings
   (:file "misc") ;;misc useful functions
   (:file "pages");;serves up the pages to the webserver
   (:file "db-access") ;;low level database access functions. After this, data is abstracted into classes and gotten lazily
   (:file "digraph") ;;Builds a recursive directed graph datastructure around a central article
   (:file "make-graph") ;;Converts the directed graph into a graphviz dot file
   (:file "read-csv") ;;just a few utility functions I wrote to read CSVs into sexps
   (:file "autocomplete") ;;used to serve up the fuzzy autocompletion data
   (:file "control"))) ;;Starts the server (brings up hunchentoot and clsql)