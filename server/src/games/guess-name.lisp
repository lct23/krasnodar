(uiop:define-package #:app/games/guess-name
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:update
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks-auth/models
                #:get-current-user
                #:get-all-users)
  (:import-from #:random-sample
                #:random-sample)
  (:import-from #:mito
                #:object-id)
  (:import-from #:app/models/user
                #:user-position
                #:user-avatar-url
                #:user-name)
  (:import-from #:serapeum
                #:take)
  (:import-from #:alexandria
                #:shuffle)
  (:import-from #:event-emitter
                #:event-emitter)
  (:import-from #:reblocks-ui2/widget
                #:ui-widget)
  (:import-from #:reblocks-ui/form
                #:with-html-form)
  (:import-from #:app/widgets/utils
                #:*green-button-classes*
                #:submit-button))
(in-package #:app/games/guess-name)


(defwidget card-widget (event-emitter ui-widget)
  ((user :initarg :user
         :reader user)
   (names :initarg :names
          :reader names)
   (correct-name :initarg :correct-name
                 :reader correct-name)
   (answered :initform nil
             :accessor answered)
   (num-attempts-left :initform 5
                      :accessor num-attempts-left)
   (failed :initform nil
           :accessor failed
           :documentation "Проставляется в True, когда заканчиваются попытки."))
  (:documentation "Карточка с фоткой сотрудника и несколькими именами на выбор."))


(defwidget guess-name-widget ()
  ((cards :initarg :cards
          :accessor cards))
  (:documentation "Игра в которой по фотографии надо угадывать имя коллеги."))


(defun make-guess-name-widget (&key (num-cards 5))
  (let* ((current-user (get-current-user))
         (all (remove current-user
                      (get-all-users)
                      ;; За исключением текущего сотрудника
                      :key #'object-id))
         (all-names (mapcar #'user-name
                            all))
         (users (random-sample all num-cards))
         (main-widget (make-instance 'guess-name-widget))
         (cards (loop for user in users
                      for correct-name = (user-name user)
                      for names = (shuffle
                                   (list* correct-name
                                          (take 4
                                                (remove correct-name
                                                        (remove-duplicates
                                                         (random-sample
                                                         ;; Возьмем имен с запасом
                                                          all-names
                                                          10)
                                                         :test 'string-equal)
                                                        :test 'string-equal))))
                      for card = (make-instance 'card-widget
                                                :user user
                                                :names names
                                                :correct-name correct-name)
                      do (event-emitter:on :next-button card
                                           (lambda (card)
                                             ;; Если карточка отвечена неверно, то сбросим ответ
                                             ;; и если закончились попытки, то пометим как непройденную.
                                             (unless (answer-correct-p card)
                                               (decf (num-attempts-left card))
                                               (cond
                                                 ((zerop (num-attempts-left card))
                                                  (setf (failed card)
                                                        t))
                                                 (t
                                                  (reset-answer card))))

                                             ;; Переставим карточку в конец колоды
                                             (setf (cards main-widget)
                                                   (append (cdr (cards main-widget))
                                                           (list card)))
                                             (update main-widget)))
                      collect card)))
    (setf (cards main-widget)
          cards)
    main-widget))


(defmethod render ((widget guess-name-widget))
  (let* ((all-cards (cards widget))
         (card
           (first
            (remove-if (lambda (card)
                         (or (failed card)
                             (answer-correct-p card)))
                       all-cards))))
    (with-html
      (:div :class "flex justify-center"
            (cond
              (card
               (render card))
              (t
               (:div :class "flex flex-col gap-8"
                     (:div :class "text-2xl font-bold text-center"
                           "Поздравляем!")
                     (:div :class "text-xl font-bold text-center"
                           "Вы успешно прошли игру!")
                     (:div :class "text-center"
                           :style "font-size: 200px"
                           "🎉"))))))))


(defun answer-correct-p (widget)
  (check-type widget card-widget)
  (and (answered widget)
       (string-equal (answered widget)
                     (correct-name widget))))

(defun reset-answer (widget)
  (check-type widget card-widget)
  (setf (answered widget)
        nil))


(defmethod render ((widget card-widget))
  (flet ((on-submit (&key answer &allow-other-keys)
           (log:error "ANSWER" answer)
           (cond
             ;; Если ответ уже дан и нажали кнопку Далее
             ((answered widget)
              (event-emitter:emit :next-button widget
                                  widget))
             ;; Пользователь дал ответ впервые
             (t
              (when answer
                (setf (answered widget)
                      answer))))
           (update widget)))
    
    (with-html-form (:post #'on-submit)
      (:div :class "w-60 h-60 flex flex-col gap-8"
            (:div :class "flex flex-col gap-2"
                  (:img :class "w-full h-full rounded-full border-2 shadow-xl"
                        :src (user-avatar-url (user widget)))
                  (:div :class "font-bold text-center"
                        (user-position (user widget))))
            (:div :class "flex flex-col gap-4"
                  (:div :class "flex flex-col gap-2 pl-8"
                        (loop for name in (names widget)
                              for class = (cond
                                            ((answered widget)
                                             (cond
                                               ;; Ответ выбран правильно
                                               ((and (string-equal (answered widget)
                                                                   name)
                                                     (string-equal (correct-name widget)
                                                                   name))
                                                "font-bold text-green-400")
                                               ;; Ответ выбран неверно
                                               ((string-equal (answered widget)
                                                              name)
                                                "text-red-400")
                                               ((string-equal (correct-name widget)
                                                              name)
                                                "text-green-400")
                                               (t
                                                "text-gray-400")))
                                            (t
                                             ""))
                              do (:div :class "flex gap-2"
                                       (:input :type "radio"
                                               :name "answer"
                                               :id name
                                               :value name
                                               :disabled (answered widget))
                                       (:label :for name
                                               :class class
                                               name))))
                  (:div :class "text-center"
                   (if (answered widget)
                       (submit-button :text "Следующая карточка"
                                      :classes *green-button-classes*)
                       (submit-button :text "Проверить"))))))))


(defmethod get-dependencies ((widget guess-name-widget))
  (call-next-method))
