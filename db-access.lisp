(in-package :wikiview)

;;Classes
;;;DB Interaction
(clsql:def-view-class page ()
  ((page-id       :db-kind :key
                  :db-constraints :not-null
                  :reader page-id
                  :type integer
                  :column "page_id")
   (page-title    :db-kind :key
                  :db-constraints :not-null
                  :type string
                  :reader page-title
                  :column "page_title")
   (page-counter  :db-kind :base
                  :db-constraints :not-null
                  :type integer
                  :reader page-counter
                  :column "page_counter")
   (is-redirect   :db-kind :base
                  :db-constraints :not-null
                  :type integer
                  :reader is-redirect
                  :column "page_is_redirect"))
  (:base-table page))

(clsql:def-view-class pagelinks ()
  ((page-id-from    :db-kind :key
                    :db-constraints :not-null
                    :reader page-id-from
                    :type integer
                    :column "pl_from")
   (page-title-to   :db-kind :base
                    :db-constraints :not-null
                    :type string
                    :reader page-title-to
                    :column "pl_title")
   (namespace       :db-kind :base
                    :db-constraints :not-null
                    :type integer
                    :reader namespace
                    :column "pl_namespace"))
  (:base-table pagelinks))

(clsql:def-view-class redirect ()
  ((page-id-from    :db-kind :key
                    :db-constraints :not-null
                    :reader page-id-from
                    :type integer
                    :column "rd_from")
   (page-title-to   :db-kind :base
                    :db-constraints :not-null
                    :type string
                    :reader page-title-to
                    :column "rd_title")
   (namespace       :db-kind :base
                    :db-constraints :not-null
                    :type integer
                    :reader namespace
                    :column "rd_namespace"))
  (:base-table redirect))

(clsql:def-view-class cluster (page)
  ((neighbours      :db-kind :join
                    :reader neighbours
                    :db-info (:home-key page-id
                              :foreign-key page-id-from
                              :join-class pagelinks
                              :set t)))
  (:base-table page))



(clsql-user:locally-disable-sql-reader-syntax)
(clsql-user:locally-enable-sql-reader-syntax) ;;first enable some syntactic sugar

;;some convenience query functions
(defun get-redirect-by-id (id)
  (caar (clsql-user:select 'redirect :where [= [slot-value 'redirect 'page-id-from] id])))

(defmethod neighbours ((the-val null))
  nil)

(defmethod page-title-to ((null null))
  nil)

(defmethod get-redirect-name ((original cluster))
  (page-title-to (get-redirect-by-id (page-id original))))

(defmethod get-redirect ((original cluster))
  (if (= 0 (is-redirect original))
      original
      (get-cluster-by-name (get-redirect-name original))))

(defmethod get-redirect ((orig null))
  nil)

(defun get-cluster-by-id (id)
  (get-redirect (caar (clsql-user:select 'cluster :where [= [slot-value 'cluster 'page-id] id]))))

(defun get-cluster-by-name (name)
  (get-redirect (caar (clsql-user:select 'cluster :where [= [slot-value 'cluster 'page-title] name]))))