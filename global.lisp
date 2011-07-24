(in-package :wikiview)

;;set the db settings
(defparameter *db-host* "mysql.maneks.net")
(defparameter *db-name* "wikidata")
(defparameter *db-user* "wikiview")
(defparameter *db-pass* "mwanza")
(defparameter *db-type* :mysql)
(defparameter *server-port* 4242)

(defvar *db-connection* nil)
(defvar *server* nil)

(defparameter *num-autocompletions* 15)
(defparameter *default-depth* 3)
(defparameter *default-children* 3)

;;set the template directory
(setf html-template:*default-template-pathname* #P"/home/lispserver/site/document-root/templates/")

(setf hunchentoot:*default-content-type* "application/xhtml+xml charset=UTF-8")

(defparameter *image-directory* "/home/lispserver/site/document-root/static/images/")
(defparameter *twopi-binary* "/usr/bin/twopi")