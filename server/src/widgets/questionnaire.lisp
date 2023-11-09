(uiop:define-package #:app/widgets/questionnaire
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:update
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/models/board-progress
                #:update-questionnaire-results-progress
                #:answer-is-correct
                #:answered-at
                #:answer
                #:question-response
                #:get-question-responses
                #:questionnaire-results)
  (:import-from #:app/models/questionnaire
                #:possible-answer-correct-p
                #:get-possible-answer
                #:possible-answer-text
                #:get-question-possible-answers)
  (:import-from #:reblocks-ui/form
                #:with-html-form)
  (:import-from #:app/widgets/utils
                #:submit-button)
  (:import-from #:local-time
                #:now)
  (:import-from #:mito
                #:save-dao)
  (:import-from #:event-emitter
                #:event-emitter)
  (:import-from #:reblocks-ui2/widget
                #:ui-widget))
(in-package #:app/widgets/questionnaire)


(defwidget questionnaire-widget (event-emitter ui-widget)
  ((questionnaire-results :type questionnaire-results
                          :initarg :questionnaire-results
                          :reader questionnaire-results))
  (:documentation "Показывает пользователю вопросы и фиксирует ответы."))


(defwidget question-widget (event-emitter ui-widget)
  ((question-response :type question-response
                      :initarg :question-response
                      :reader question-response)
   (responded :type boolean
              :initform nil
              :accessor responded)
   (response-is-correct :type boolean
                        :initform nil
                        :accessor response-is-correct))
  (:documentation "Показывает вопрос и варианты ответов, фиксирует что выбрал пользователь."))


(defun make-questionnaire-widget (questionnaire-results)
  (make-instance 'questionnaire-widget
                 :questionnaire-results questionnaire-results))

(defun make-question-widget (question-response)
  (make-instance 'question-widget
                 :question-response question-response))


(defmethod render ((widget questionnaire-widget))
  (with-html
    (loop with questionnaire-results = (questionnaire-results widget)
          for question in (get-question-responses questionnaire-results)
          for subwidget = (make-question-widget question)
          do (event-emitter:on :answered subwidget
                               (lambda (result-is-correct)
                                 (declare (ignore result-is-correct))
                                 (update-questionnaire-results-progress questionnaire-results)))
             (render subwidget))))


(defmethod render ((widget question-widget))
  (let* ((response (question-response widget))
         (question (app/models/board-progress::question response)))
    (flet ((save-results (&key answer-id &allow-other-keys)
             (when answer-id
               (log:error "User answered" answer-id)
               ;; Проверим правильный ли ответ и зафиксируем его:
               (let ((user-answer (get-possible-answer answer-id)))
                 (when user-answer
                   (let ((is-correct-p (possible-answer-correct-p user-answer)))
                     (setf (answer response)
                           user-answer)
                     (setf (answered-at response)
                           (now))
                     (setf (answer-is-correct response)
                           is-correct-p)
                     (save-dao response)

                     ;; Обновим состояние виджета, чтобы показать пользователю верно ли он ответил
                     (setf (responded widget)
                           t
                           (response-is-correct widget)
                           is-correct-p)
                     (event-emitter:emit :answered widget
                                         is-correct-p))))
               (update widget))))
      (with-html-form (:post #'save-results
                       :class "border-2 p-2 mb-4 flex flex-col")
        (:h1 (app/models/questionnaire::question question))
                
        (cond
          ((responded widget)
           (if (response-is-correct widget)
               (:p "Ответ верный.")
               (:p "Ответ неверный.")))
          (t
           (loop for answer in (get-question-possible-answers question)
                 for input-id = (symbol-name (gensym "input"))
                 do (:div (:input :id input-id
                                  :type "radio"
                                  :name "answer-id"
                                  :value (mito:object-id answer))
                          (:label :for input-id
                                  (possible-answer-text answer))))
           (:div
            (submit-button :text "Ответить"))))))))
