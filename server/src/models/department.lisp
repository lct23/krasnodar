(uiop:define-package #:app/models/department
  (:use #:cl)
  (:import-from #:mito
                #:find-dao
                #:create-dao
                #:select-dao)
  (:import-from #:sxql
                #:order-by))
(in-package #:app/models/department)


(defclass department ()
  ((title :col-type :text
          :initform ""
          :initarg :title
          :reader department-title))
  (:metaclass mito:dao-table-class))



(defun get-department (id)
  (find-dao 'department :id id))


(defun get-departments ()
  (select-dao 'department
    (order-by :title)))


(defun create-department (title)
  (create-dao 'department
              :title title))
