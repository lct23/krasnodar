(uiop:define-package #:app/models/department
  (:use #:cl)
  (:import-from #:mito))
(in-package #:app/models/department)


(defclass department ()
  ((title :col-type :text
          :initform ""
          :initarg :title
          :reader department-title))
  (:metaclass mito:dao-table-class))



(defun get-departments ()
  (mito:select-dao 'department
    (sxql:order-by :title)))


(defun create-department (title)
  (mito:create-dao 'department
                   :title title))
