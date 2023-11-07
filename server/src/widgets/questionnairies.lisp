(uiop:define-package #:app/widgets/questionnairies
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:get-css-classes
                #:update
                #:render
                #:defwidget)
  (:import-from #:reblocks/widget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:event-emitter
                #:event-emitter)
  (:import-from #:serapeum
                #:fmt
                #:soft-list-of
                #:push-end)
  (:import-from #:alexandria
                #:last-elt)
  (:import-from #:reblocks-ui2/widget
                #:ui-widget)
  (:import-from #:app/widgets/utils
                #:*small-button-classes*
                #:text-input
                #:submit-button
                #:*button-classes*)
  (:import-from #:reblocks-ui2/buttons/button
                #:button)
  (:import-from #:app/models/questionnaire
                #:delete-question
                #:delete-possible-answer
                #:possible-answer-correct-p
                #:possible-answer-text
                #:get-question-possible-answers
                #:get-questionnaire-questions
                #:add-possible-answer
                #:possible-answer
                #:question
                #:questionnaire
                #:add-question
                #:make-questionnare)
  (:import-from #:str
                #:replace-all)
  (:import-from #:reblocks-ui/form
                #:with-html-form)
  (:import-from #:app/widgets/small-button
                #:small-button))
(in-package #:app/widgets/questionnairies)


(defwidget add-questionnaire-widget (event-emitter ui-widget)
  ())


(defwidget possible-answer-widget (event-emitter ui-widget)
  ((possible-answer :initarg :possible-answer
                    :type possible-answer
                    :reader possible-answer)))


(defwidget possible-answer-form-widget (event-emitter ui-widget)
  ((question :initarg :question
             :type question
             :reader question)))


(defwidget question-widget (event-emitter ui-widget)
  ((question :initarg :question
             :type question
             :reader question)
   (possible-answers :initform nil
                     :initarg :possible-answers
                     :type (soft-list-of possible-answer-widget)
                     :accessor possible-answers)))


(defwidget questionnaire-widget (event-emitter ui-widget)
  ((questionnare :initarg :questionnare
                 :type questionnaire
                 :reader questionnare)
   (questions :initform nil
              :initarg :questions
              :type (soft-list-of question-widget)
              :accessor questions)))


(defwidget questionnairies-widget ()
  ((add-widget :initarg :add-widget
               :reader add-widget)
   (questionnairies :initform nil
                    :initarg :questionnairies
                    :accessor questionnairies)))

(defmacro add-deletion-callbacks (list-form)
  `(flet ((remove-widget (w)
            (setf ,list-form
                  (remove w ,list-form))
            (update w :remove t)))
     (loop for widget in ,list-form
           do (event-emitter:on :delete widget
                                #'remove-widget))))


(defmacro add-to-the-end (parent-widget list-form new-widget-form)
  `(flet ((remove-widget (w)
            (setf ,list-form
                  (remove w ,list-form))
            (update w :remove t)))
     (let ((last-widget
             (when ,list-form
               (last-elt
                ,list-form)))
           (new-widget ,new-widget-form))
       
       (event-emitter:on :delete new-widget
                         #'remove-widget)
       
       (push-end new-widget
                 ,list-form)
       (if last-widget
           (update new-widget :inserted-after last-widget)
           (update ,parent-widget)))))


(defun make-possible-answer-widget (possible-answer)
  (make-instance 'possible-answer-widget
                 :possible-answer possible-answer))


(defun make-question-widget (question)
  (let ((widget (make-instance 'question-widget
                               :question question
                               :possible-answers (mapcar #'make-possible-answer-widget
                                                         (get-question-possible-answers question)))))
    (add-deletion-callbacks (possible-answers widget))
    widget))


(defun make-add-questionnaire-widget ()
  (make-instance 'add-questionnaire-widget))


(defun make-questionnaire-widget (questionnare)
  (let ((widget (make-instance 'questionnaire-widget
                               :questionnare questionnare
                               :questions (mapcar #'make-question-widget
                                                  (get-questionnaire-questions questionnare)))))
    (add-deletion-callbacks (questions widget))
    (values widget)))


(defun make-questionnairies-widget ()
  (let* ((add-widget (make-add-questionnaire-widget))
         (widget (make-instance 'questionnairies-widget
                                :add-widget add-widget
                                :questionnairies
                                (mapcar #'make-questionnaire-widget
                                        (app/models/questionnaire::get-questionnairies)))))
    (event-emitter:on :object-created add-widget
                      (lambda (questionnaire)
                        (let ((last-widget
                                (when (questionnairies widget)
                                  (last-elt
                                   (questionnairies widget))))
                              (new-widget
                                (make-questionnaire-widget questionnaire)))
                          (push-end new-widget
                                    (questionnairies widget))
                          (if last-widget
                              (update new-widget :inserted-after last-widget)
                              (update widget)))))
    widget))


(defmethod render ((widget questionnairies-widget))
  (with-html
    (loop for widget in (questionnairies widget)
          do (render widget))
    (render (add-widget widget))))


(defmethod render ((widget add-questionnaire-widget))
  (render
   (button "Добавить опрос"
           :on-click (lambda (&rest rest)
                       (declare (ignore rest))
                       (event-emitter:emit :object-created widget
                                           (make-questionnare)))
           :class *button-classes*)))


(defmethod render ((widget questionnaire-widget))
  (flet ((add-question (&key question &allow-other-keys)
           (let ((new-question (add-question (questionnare widget) question)))
             (add-to-the-end widget
                             (questions widget)
                             (make-question-widget new-question)))))
    (with-html
      (cond
        ((questions widget)
         (loop for widget in (questions widget)
               do (render widget)))
        (t
         (:div :class "mb-4"
               "В этом опроснике пока нет ни одного вопроса."))))

    ;; TODO: Надо переделать на виджет, чтобы сбрасывать форму отдельно от поля
    (with-html-form (:post #'add-question
                     :class "w-full mb-8 flex items-center")
      (text-input "question"
                  :label "Добавить вопрос")
      (submit-button :text "Добавить"))))


(defmethod get-css-classes ((widget questionnaire-widget))
  (list "questionnaire-widget"
        "border border-green-500 p-2 mb-6 rounded"))


(defmethod render ((widget possible-answer-form-widget))
  (flet ((add-new-item (&key text correct &allow-other-keys)
           (let ((new-item (add-possible-answer (question widget) text
                                                :correct (when correct
                                                           t))))
             (event-emitter:emit :object-created widget
                                 new-item))))
    (with-html-form (:post #'add-new-item
                     :class "w-full mb-8 flex items-center")
      (text-input "text"
                  :label "Возможный ответ")
      ;; TODO: add check-box
      ;; (text-input "text"
      ;;             :label "Возможный ответ")
      (submit-button :text "Добавить"))))


(defmethod render ((widget question-widget))
  (let ((form (make-instance 'possible-answer-form-widget
                             :question (question widget))))
    (flet ((add-new-item (possible-answer)
             (add-to-the-end widget
                             (possible-answers widget)
                             ;; TODO: apply delete handler here
                             (make-possible-answer-widget possible-answer))
             (update form))
           ;; Обработчик удаления:
           (remove-item (&rest rest)
             (declare (ignore rest))
             (delete-question (question widget))
             (event-emitter:emit :delete widget
                                 widget)))
      (event-emitter:on :object-created form
                        #'add-new-item)
      
      (with-html
        (:div
         (fmt "Вопрос: ~A"
              (app/models/questionnaire::question
               (question widget)))
         (render
          (small-button "X"
                        :on-click #'remove-item)))

        (:ul
         (loop for widget in (possible-answers widget)
               do (render widget))))
      (render form))))


(defmethod render ((widget possible-answer-widget))
  (flet ((remove-item (&rest rest)
           (declare (ignore rest))
           (delete-possible-answer (possible-answer widget))
           (event-emitter:emit :delete widget
                               widget)))
    (with-html
      (let* ((answer (possible-answer widget))
             (text (possible-answer-text answer))
             (correct (possible-answer-correct-p answer))
             (classes (when correct
                        "font-bold")))
        (:span :class classes
               text)
        (render
         (small-button "X"
                       :on-click #'remove-item))))))

(defmethod reblocks/widget:get-html-tag ((widget possible-answer-widget))
  :li)

(defmethod reblocks/widget:get-css-classes ((widget possible-answer-widget))
  (list "list-inside"
        "list-decimal"))
