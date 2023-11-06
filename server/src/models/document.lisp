(uiop:define-package #:app/models/document
  (:use #:cl)
  (:import-from #:mito
                #:select-dao)
  (:import-from #:app/models/department
                #:department)
  (:import-from #:app/models/user
                #:user)
  (:import-from #:sxql
                #:order-by))
(in-package #:app/models/document)


(defclass document ()
  ((title :col-type :text
          :initform ""
          :initarg :title
          :reader document-title)
   (text :col-type :text
         :initform ""
         :initarg :text
         :reader document-text)
   (department :col-type (or :null department)
               :initform nil
               :initarg :department
               :reader document-department))
  (:metaclass mito:dao-table-class))


(defclass who-can-help ()
  ((document :col-type document
             :initform nil
             :initarg :document
             :reader document)
   (person :col-type user
           :initform nil
           :initarg :person
           :reader person))
  (:metaclass mito:dao-table-class)
  (:documentation "Описывает связь между документом и людьми, которые могут помочь разобраться в этой теме."))



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
