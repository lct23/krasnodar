(uiop:define-package #:app/models/document
  (:use #:cl)
  (:import-from #:mito
                #:select-dao)
  (:import-from #:app/models/department
                #:department)
  (:import-from #:sxql
                #:order-by))
(in-package #:app/models/document)


(defclass document ()
  ((title :col-type :text
          :initform ""
          :initarg :title
          :accessor document-title)
   (text :col-type :text
         :initform ""
         :initarg :text
         :accessor document-text)
   (department :col-type (or :null department)
               :initform nil
               :initarg :department
               :accessor document-department))
  (:metaclass mito:dao-table-class))


(defun create-document (&key (title "") department)
  (mito:create-dao 'document
                   :title title
                   :department department))


(defun get-documents ()
  (select-dao 'document
    (order-by :title)))


(defun get-document (id)
  (mito:find-dao 'document
                 :id id))


(defun get-documents-for-list-widget ()
  (mito:select-by-sql 'document
                      "SELECT doc.*
                         FROM document as doc
                         LEFT JOIN department as dep ON doc.department_id = dep.id
                        ORDER BY dep.title NULLS FIRST, doc.title
                          "
                      ))
