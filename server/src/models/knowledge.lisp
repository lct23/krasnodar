(uiop:define-package #:app/models/knowledge
  (:use #:cl)
  (:import-from #:mito
                #:select-by-sql
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


(defclass knowledge-with-deadline (knowledge)
  ((deadline-missed :col-type :boolean
                    :initarg :deadline-missed
                    :reader deadline-missed-p)
   (period-knowledge-progress-id :col-type :bigint
                                 :reader period-knowledge-progress-id))
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


(defun find-pending-knowledges (user)
  "Возвращает список знаний которые надо изучить следующими.

   При этом у знаний есть дополнительная колонка deadline_missed,
   которая говорит о том, что период, когда его надо было изучить
   уже закончился."
  ;; Creates a circular dependencies :(
  ;; (check-type user user)
  (select-by-sql 'knowledge-with-deadline
                 "
select k.*,
       pp.ends_at < now() as deadline_missed,
       pkp.id as period_knowledge_progress_id 
  from period_knowledge_progress as pkp
  join period_progress as pp on pkp.period_progress_id = pp.id
  join board_progress as bp on pp.board_progress_id = bp.id
  join question_response as qr on qr.questionnaire_results_id = pkp.questionnaire_results_id
  join period_knowledge as pk on pk.id = pkp.period_knowledge_id
  join knowledge as k on k.id = pk.knowledge_id
where bp.user_id = ?
  and qr.answered_at IS NULL
  and pp.starts_at < now()
order by pp.starts_at, pp.id
"
                 :binds (list (object-id user))))
