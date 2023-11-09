(uiop:define-package #:app/models/who-can-help
  (:use #:cl)
  (:import-from #:mito
                #:select-dao)
  (:import-from #:app/models/department
                #:department)
  (:import-from #:app/models/user
                #:user)
  (:import-from #:sxql
                #:order-by)
  (:import-from #:app/models/document
                #:document))
(in-package #:app/models/who-can-help)


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
