(uiop:define-package #:app/analytics
  (:use #:cl)
  (:import-from #:mito))
(in-package #:app/analytics)


(defclass hr-stats ()
  ((period-title :col-type :text
                 :initarg :period-title
                 :reader period-title)
   (prohodit :col-type :integer
             :initarg :prohodit
             :reader prohodit
             :documentation "сколько ещё не прошли период онбординга")
   (proshlo :col-type :integer
            :initarg :proshlo
            :reader proshlo
            :documentation "сколько человек прошли период онбординга")
   (avg-progress :col-type :float
                 :initarg :avg-progress
                 :reader avg-progress
                 :documentation "средний процент правильных ответов на человека")
   (incorrect-answers :col-type :integer
                      :initarg :incorrect-answers
                      :reader incorrect-answers
                      :documentation "на сколько вопросов ответили неверно")
   (delayed-answers :col-type :integer
                    :initarg :delayed-answers
                    :reader delayed-answers
                    :documentation "на сколько вопросов ответили с опазданием")
   (overdue-answers :col-type :integer
                    :initarg :overdue-answers
                    :reader overdue-answers
                    :documentation "на сколько вопросов ответа нет, а срок прошёл"))
  (:metaclass mito:dao-table-class))


(defun get-stats-for-hr-dashboard ()
  (mito:select-by-sql
   'hr-stats
   "
with data_by_user as (
    select pp.id,
           pp.title,
           min(qr.questionnaire_progress) as min_progress,
           max(qr.questionnaire_progress) as max_progress,
           avg(qr.questionnaire_progress) as avg_progress
      from period_progress as pp
      join period_knowledge_progress as pkp on pp.id = pkp.period_progress_id
      join questionnaire_results as qr on qr.id = pkp.questionnaire_results_id
     group by pp.id
-- статистика по числу пользователей проходящих разные
-- этапы онбординга
), users_stats as (
    select title,
           count(*) filter (where min_progress = 0) as prohodit,
           count(*) filter (where max_progress = 100) as proshlo,
           avg(avg_progress) as  avg_progress
      from data_by_user
     group by title
-- провалы и задержки ответов
), answers_stats as (
    select pp.title,
           count(*) filter (
               WHERE qres.answered_at is not NULL
                 AND qres.answer_is_correct = False
           ) as incorrect_answers,
           count(*) filter (
               WHERE qres.answered_at is not NULL
                 AND qres.answered_at > pp.ends_at
           ) as delayed_answers,
           count(*) filter (
               WHERE qres.answered_at is NULL
                 AND pp.ends_at < now()
           ) as overdue_answers
      from question_response as qres
      join questionnaire_results as qr on qr.id = qres.questionnaire_results_id
      join period_knowledge_progress as pkp on pkp.questionnaire_results_id = qr.id
      join period_progress as pp on pp.id = pkp.period_progress_id
    group by pp.title
), period_titles as (
   select title,
          min(from_day) as from_day
     from board_period
    group by title
    order by from_day
)
select pt.title as period_title,
       prohodit,          -- сколько ещё не прошли период онбординга
       proshlo,           -- сколько человек прошли период онбординга
       avg_progress,      -- средний процент правильных ответов на человека
       incorrect_answers, -- на сколько вопросов ответили неверно
       delayed_answers,   -- на сколько вопросов ответили с опазданием
       overdue_answers    -- на сколько вопросов ответа нет, а срок прошёл
  from period_titles as pt
  left join users_stats using (title)
  left join answers_stats using (title)
"))
