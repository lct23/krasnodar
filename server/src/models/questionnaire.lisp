(uiop:define-package #:app/models/questionnaire
  (:use #:cl)
  (:import-from #:app/models/document
                #:document))
(in-package #:app/models/questionnaire)


(defclass questionnaire ()
  ((title :col-type :text
          :initform ""
          :initarg :title
          :accessor questionnaire-title)
   (document :col-type (or :null document)
             :initform nil
             :initarg :document
             :accessor questionnaire-document))
  (:documentation "Опросник, который надо пройти после изучения материала.")
  (:metaclass mito:dao-table-class))


(defclass question ()
  ((questionnaire :col-type questionnaire
                  :initarg :questionnaire
                  :accessor question-questionnaire)
   (question :col-type :text
             :initarg :question
             :accessor question))
  (:documentation "Один вопрос из опросника.")
  (:metaclass mito:dao-table-class))


(defclass possible-answer ()
  ((question :col-type question
             :initarg :question
             :accessor possible-anwer-question)
   (text :col-type :text
         :initarg :text
         :accessor possible-answer-text)
   (correct :col-type :boolean
            :initform nil
            :accessor possible-answer-correct-p))
  (:documentation "Возможный ответ.")
  (:metaclass mito:dao-table-class))


(defun make-questionnaire (title)
  (mito:create-dao 'questionnaire
                   :title title))


(defun get-questionnairies ()
  (mito:select-dao 'questionnaire))


(defun get-questionnaire-questions (questionnaire)
  (check-type questionnaire questionnaire)
  (let ((questions (mito:retrieve-dao 'question
                                      :questionnaire questionnaire)))
    (values questions)))


(defun get-question-possible-answers (question)
  (check-type question question)
  (mito:retrieve-dao 'possible-answer
                     :question question))


(defun add-question (questionnaire question)
  (check-type questionnaire questionnaire)
  (check-type question string)
  (mito:create-dao 'question
                   :questionnaire questionnaire
                   :question question))


(defun add-possible-answer (question text &key correct)
  (check-type question question)
  (check-type text string)
  (mito:create-dao 'possible-answer
                   :question question
                   :text text
                   :correct correct))

(defun delete-question (question)
  (check-type question question)
  (mito:delete-dao question))

(defun delete-possible-answer (answer)
  (check-type answer possible-answer)
  (mito:delete-dao answer))
