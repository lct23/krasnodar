(uiop:define-package #:app/scripts/onboarding-filler
  (:use #:cl)
  (:import-from #:40ants-pg/transactions
                #:with-transaction)
  (:import-from #:app/models/board-progress
                #:game-results
                #:get-question-possible-answers
                #:question
                #:get-question-responses
                #:questionnaire-results
                #:get-knowledge-progresses
                #:ends-at
                #:starts-at
                #:get-periods
                #:questionnaire-results-progress
                #:answer-is-correct
                #:answered-at
                #:answer
                #:assign-board
                #:find-board-for-user
                #:user-progress)
  (:import-from #:app/models/user
                #:get-all-users
                #:user-start-work-at
                #:start-work-at
                #:get-user)
  (:import-from #:humanize-duration
                #:humanize-duration)
  (:import-from #:local-time
                #:timestamp-minimum
                #:adjust-timestamp
                #:timestamp<
                #:now
                #:adjust-timestamp!)
  (:import-from #:random-sample
                #:random-sample)
  (:import-from #:app/utils
                #:random-timestamp-between)
  (:import-from #:mito
                #:save-dao)
  (:import-from #:app/models/questionnaire
                #:possible-answer-correct-p))
(in-package #:app/scripts/onboarding-filler)


(defun random-time-in-past (max-days)
  (let ((num-days (random max-days)))
    (adjust-timestamp! (now)
      (:offset :day (- num-days)))))


(defun answer-random-questions (board-progress)
  (loop with now = (now)
        for period in (get-periods board-progress)
        for starts-at = (starts-at period)
        for ends-at = (ends-at period)
        when (timestamp< starts-at now)
        do (loop for knowledge-progress in (get-knowledge-progresses period)
                 for results = (questionnaire-results knowledge-progress)
                 for game-results = (game-results knowledge-progress)
                 for question-responses = (get-question-responses results)
                 ;; for game-score = (when game-finished
                 ;;                    (random 100))
                 for correct-responses = 0
                 ;; Если есть результаты игры, то не пытаемся заполнять результаты
                 ;; иначе дашборд может показать что пункт выполнен, хотя мы хотим чтобы
                 ;; игру всё же прошли
                 unless game-results
                 do (loop for response in question-responses
                          for question = (question response)
                          for possible-answers = (get-question-possible-answers question)
                          for correct-answer = (first (remove-if-not #'possible-answer-correct-p
                                                                     possible-answers))
                          for incorrect-answers = (remove-if #'possible-answer-correct-p
                                                             possible-answers)
                          for incorrect-answer = (first (random-sample incorrect-answers 1))
                          ;; Вероятность того, что ответ вообще был дан
                          for answered = (< (random 100)
                                            90)
                          ;; Вероятность того, что был дан правильный ответ
                          for is-correct = (< (random 100)
                                              90)
                          for answer = (if is-correct
                                           correct-answer
                                           incorrect-answer)
                          for answered-at = (random-timestamp-between starts-at
                                                                      ;; Добавим недельку на возможную просрочку
                                                                      (timestamp-minimum
                                                                       (adjust-timestamp ends-at
                                                                         (:offset :day 7))
                                                                       now))
                          when answered
                          do (when is-correct
                               (incf correct-responses))
                             (setf (answer response) answer
                                   (answered-at response) answered-at
                                   (answer-is-correct response) is-correct)
                             (save-dao response))
                    ;; Сохраним результаты по этому знанию и перейдём к следующему
                    (setf (questionnaire-results-progress results)
                          (/ (* 100 correct-responses)
                             (length question-responses)))
                    (mito:save-dao results))))


(defun fill-onboarding-for-user (user &key starts-at
                                           (max-days
                                            (* 1 365)))
  "Заполняет базу тестовыми данными."
  (let ((starts-at (or starts-at
                       (random-time-in-past max-days))))
    (with-transaction
      (setf (user-start-work-at user)
            starts-at)
      (mito:save-dao user)

      (let ((existing-progress (user-progress user))
            (new-board (find-board-for-user user)))
        (when existing-progress
          (mito:delete-dao existing-progress))

        (let ((board-progress (assign-board user new-board)))
          (answer-random-questions board-progress)
          (values board-progress
                  starts-at))))))


(defun fill-onboarding-for-all-users ()
  (loop for user in (get-all-users)
        do (fill-onboarding-for-user user)))
