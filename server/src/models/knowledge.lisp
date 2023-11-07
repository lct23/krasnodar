(uiop:define-package #:models/app/knowledge
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
                #:create-document
                #:document)
  (:import-from #:app/models/questionnaire
                #:questionnaire-document
                #:make-questionnaire
                #:questionnaire))
(in-package #:models/app/knowledge)


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


(defun create-knowledge (&key document questionnaire department (title "Документ без названия"))
  (let* ((document (or document
                       (create-document :title title
                                        :department department)))
         (questionnaire (or (when questionnaire
                              (setf (questionnaire-document questionnaire)
                                    document)
                              (mito:save-dao questionnaire)
                              questionnaire)
                            (make-default-questionnaire))))
    (mito:create-dao 'knowledge
                     :department department
                     :document document
                     :questionnaire questionnaire)))


(defun get-knowledge (id)
  (mito:find-dao 'knowledge
                 :id id))
