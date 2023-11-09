(uiop:define-package #:app/models/knowledge
  (:use #:cl)
  (:import-from #:mito
                #:object-id
                #:select-dao)
  (:import-from #:app/models/department
                #:department)
  (:import-from #:sxql
                #:order-by)
  (:import-from #:app/models/document
                #:document-title
                #:create-document
                #:document)
  (:import-from #:app/models/questionnaire
                #:make-default-questionnaire
                #:questionnaire-document
                #:make-questionnaire
                #:questionnaire))
(in-package #:app/models/knowledge)


(defclass knowledge ()
  ((department :col-type (or :null department)
               :initform nil
               :initarg :department
               :reader knowledge-department)
   (document :col-type (or :null document)
             :initarg :document
             :reader knowledge-document)
   (questionnaire :col-type (or :null questionnaire)
                  :initarg :questionnaire
                  :reader knowledge-questionnaire))
  (:metaclass mito:dao-table-class))


(defun knownledge-title (obj)
  (check-type obj knowledge)
  (if (knowledge-document obj)
      (document-title (knowledge-document obj))
      ""))


(defun create-knowledge (&key document questionnaire department (title "Документ без названия"))
  (let* ((document (or document
                       (create-document :title title
                                        :department department)))
         (questionnaire (or (when questionnaire
                              (setf (questionnaire-document questionnaire)
                                    document)
                              (mito:save-dao questionnaire)
                              questionnaire)
                            (make-default-questionnaire document))))
    (mito:create-dao 'knowledge
                     :department department
                     :document document
                     :questionnaire questionnaire)))


(defun delete-knowledge (knowledge)
  (let ((knowledge-id (object-id knowledge)))
    (log:warn "Deleting knowledge" knowledge-id)
    (mito:delete-dao knowledge)))


(defun get-knowledge (id)
  (mito:find-dao 'knowledge
                 :id id))

(defun get-knowledges ()
  (mito:select-by-sql 'knowledge
                      "SELECT k.*
                         FROM knowledge as k
                         LEFT JOIN document as d ON k.document_id = d.id
                        ORDER BY d.title"))
