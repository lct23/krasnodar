(uiop:define-package #:app/models/board-progress
  (:use #:cl)
  (:import-from #:app/models/board
                #:knowledge
                #:period-knowledge
                #:period-knowledges
                #:period-to-day
                #:period-from-day
                #:period-title
                #:board-periods
                #:board-period
                #:board)
  (:import-from #:app/models/user
                #:user-board
                #:user)
  (:import-from #:app/models/knowledge
                #:knownledge-title
                #:knowledge-questionnaire
                #:knowledge)
  (:import-from #:app/models/questionnaire
                #:get-question-possible-answers
                #:get-questionnaire-questions
                #:possible-answer
                #:question
                #:questionnaire)
  (:import-from #:40ants-pg/transactions
                #:with-transaction)
  (:import-from #:mito
                #:select-dao
                #:object-id
                #:find-dao
                #:create-dao)
  (:import-from #:sxql
                #:where
                #:order-by)
  (:import-from #:local-time
                #:timestamp>=
                #:now))
(in-package #:app/models/board-progress)


(defclass board-progress ()
  ((user :col-type user
         :initarg :user
         :reader user)
   (board :col-type board
          :initarg :board
          :reader board))
  (:metaclass mito:dao-table-class))


(defclass period-progress ()
  ((board-progress :col-type board-progress
                   :initarg :board-progress
                   :reader board-progress)
   (title :col-type :text
          :initarg :title
          :reader period-title)
   (starts-at :col-type :timestamptz
              :initform ""
              :initarg :starts-at
              :reader starts-at
              :documentation "Конкретное время, начиная с которого стартует период.")
   (ends-at :col-type :timestamptz
            :initform ""
            :initarg :ends-at
            :reader ends-at
            :documentation "Конкретное время, до которого длится период, включительно.")
   (progress :col-type :integer
             :initform 0
             :accessor progress
             :documentation "Показывает сколько процентов задач из этого периода выполнено."))
  (:metaclass mito:dao-table-class))


(defun get-periods (board-progress)
  (check-type board-progress board-progress)
  (select-dao 'period-progress
    (where (:= :board_progress_id
               (object-id board-progress)))
    (order-by :id)))


(defclass questionnaire-results ()
  ((questionnaire :col-type questionnaire
                  :initarg :questionnaire
                  :reader questionnaire)
   (questionnaire-progress :col-type :integer
                           :initform 0
                           :accessor questionnaire-results-progress
                           :documentation "Процент правильных ответов в опроснике по знанию"))
  (:documentation "Ответы на вопросы")
  (:metaclass mito:dao-table-class))


(defclass question-response ()
  ((questionnaire-results :col-type questionnaire-results
                          :initarg :questionnaire-results
                          :reader questionnaire-results)
   (question :col-type question
             :initarg :question
             :reader question)
   (answer :col-type (or :null possible-answer)
           :initarg :answer
           :accessor answer)
   (answered-at :col-type (or :null :timestamptz)
                :initform nil
                :accessor answered-at)
   (answer-is-correct :col-type :boolean
                      :initform nil
                      :accessor answer-is-correct))
  (:documentation "Ответ на один вопрос из опросника, пока сотрудник не прошел опрос, answer будет NULL")
  (:metaclass mito:dao-table-class))


(defclass period-knowledge-progress ()
  ((period-progress :col-type period-progress
                    :initarg :period-progress
                    :reader period-progress)
   (period-knowledge :col-type period-knowledge
                     :initarg :period-knowledge
                     :reader period-knowledge)
   (questionnaire-results :col-type questionnaire-results
                          :initform :questionnaire-results
                          :accessor questionnaire-results
                          :documentation "Ответы сотрудника на вопросы опросника"))
  (:documentation "Связь между знанием и периодом онбординга")
  (:metaclass mito:dao-table-class))


(defun get-period-knowledge-progress (id)
  (find-dao 'period-knowledge-progress
            :id id))


(defun period-knowledge-progress-title (obj)
  (check-type obj period-knowledge-progress)
  (knownledge-title (knowledge (period-knowledge obj))))


(defun period-knowledge-progress-status (obj)
  (check-type obj period-knowledge-progress)
  (cond
    ((not (zerop (questionnaire-results-progress
                  (questionnaire-results obj))))
     "Пройдено")
    ((timestamp>= (now)
                  (ends-at (period-progress obj)))
     "Просрочено")
    (t
     "Ожидает")))

(defun period-knowledge-progress-percent (obj)
  (check-type obj period-knowledge-progress)
  (questionnaire-results-progress
   (questionnaire-results obj)))


(defun get-knowledge-progresses (period-progress)
  (check-type period-progress period-progress)
  (select-dao 'period-knowledge-progress
    (where (:= :period_progress_id
               (object-id period-progress)))
    (order-by :id)))


(defun relative-to-absolute (num-days &key end-of-the-day)
  "Высчитывает реальную дату и время относительно текущей даты. Чтобы мы понимали когда дедлайн."
  (let ((ts (local-time:now)))
    (local-time:adjust-timestamp ts
      (:set :hour (if end-of-the-day
                      23
                      0))
      (:set :minute (if end-of-the-day
                        59
                        0))
      (:set :sec (if end-of-the-day
                     59
                     0))
      (:set :nsec (if end-of-the-day
                      999999999
                      0))
      (:offset :day num-days))))


(defun make-questionnaire-results (period-knowledge)
  (check-type period-knowledge period-knowledge)
  (let* ((knowledge (app/models/board::knowledge period-knowledge))
         (questionnaire (knowledge-questionnaire knowledge))
         (qr (create-dao 'questionnaire-results
                         :questionnaire questionnaire)))
    (loop for question in (get-questionnaire-questions questionnaire)
          do (create-dao 'question-response
                         :questionnaire-results qr
                         :question question))
    qr))


(defun user-progress (user)
  (check-type user user)
  (find-dao 'board-progress
            :user user))


(defun assign-board (user board)
  (check-type user user)
  (check-type board board)

  (cond
    ((user-progress user)
     (user-progress user))
    (t
     (let ((user-id (object-id user))
           (board-id (object-id board)))
       (log:info "Assigning board" user-id board-id))
     
     (with-transaction
         (let* ((bp (create-dao 'board-progress
                                :user user
                                :board board)))
           (loop for period in (board-periods board)
                 for pp = (create-dao 'period-progress
                                      :board-progress bp
                                      :title (period-title period)
                                      :starts-at (relative-to-absolute
                                                  (period-from-day period))
                                      :ends-at (relative-to-absolute
                                                (period-to-day period)
                                                :end-of-the-day t))
                 do (loop for period-knowledge in (period-knowledges period)
                          for questionnaire-results = (make-questionnaire-results period-knowledge)
                          for pkp = (create-dao 'period-knowledge-progress
                                                :period-progress pp
                                                :period-knowledge period-knowledge
                                                :questionnaire-results questionnaire-results)))
           (values bp))))))


(defun get-question-responses (questionnaire-results)
  (check-type questionnaire-results questionnaire-results)
  (select-dao 'question-response
    (where (:= :questionnaire_results_id
               (object-id questionnaire-results)))
    (order-by :id)))


(defun update-questionnaire-results-progress (questionnaire-results)
  (check-type questionnaire-results questionnaire-results)
  (mito:execute-sql
   "UPDATE questionnaire_results as qr
       SET questionnaire_progress = (
         SELECT 100 * COUNT(*) FILTER (WHERE answer_is_correct) / COUNT(*)
           FROM question_response as r
          WHERE r.questionnaire_results_id = ?
       )
     WHERE qr.id = ?"
   (list (object-id questionnaire-results)
         (object-id questionnaire-results))))