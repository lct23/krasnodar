(uiop:define-package #:app/games/guess-position
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
                #:push-end
                #:fmt
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
                #:submit-button)
  (:import-from #:reblocks-ui2/buttons/button
                #:button))
(in-package #:app/games/guess-position)


(defwidget card-widget (event-emitter ui-widget)
  ((user :initarg :user
         :reader user)
   (answers :initarg :answers ;; names
            :reader answers)
   (correct-answer :initarg :correct-answer ;; correct-name
                   :reader correct-answer)
   (answered :initform nil
             :accessor answered)
   (num-attempts-left :initform 5
                      :accessor num-attempts-left)
   (suggest-getter :initarg :suggest-getter
                   :reader suggest-getter
                   :documentation "Функция, принимающая сотрудника и отдающая подсказку под фото, например должность или фамилию.")
   (failed :initform nil
           :accessor failed
           :documentation "Проставляется в True, когда заканчиваются попытки."))
  (:documentation "Карточка с фоткой сотрудника и несколькими именами на выбор."))


(defmethod print-object ((obj card-widget) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A attempts: ~A, correct: ~A"
            (correct-answer obj)
            (num-attempts-left obj)
            (answer-correct-p obj))))


(defwidget guess-position-widget (event-emitter ui-widget)
  ((cards :initarg :cards
          :accessor cards)
   (failed-cards :initform nil
                 :accessor failed-cards)
   (successful-cards :initform nil
                     :accessor successful-cards)
   (congratulation-getter :initarg :congratulation-getter
                          :reader congratulation-getter))
  (:documentation "Игра в которой по фотографии надо угадывать имя коллеги."))


(defun make-guess-position-widget (&key (num-cards 5)
                                        (answer-getter #'user-position)
                                        (suggest-getter #'user-name)
                                        (congratulation-getter
                                         (lambda (&key num-successes &allow-other-keys)
                                           (fmt "Вы успешно прошли игру и запомнили должности ~A коллег!"
                                                num-successes))))
  (let* ((current-user (get-current-user))
         (all (remove current-user
                      (get-all-users)
                      ;; За исключением текущего сотрудника
                      :key #'object-id))
         (all-answers (mapcar answer-getter
                              all))
         (users (random-sample all num-cards))
         (main-widget (make-instance 'guess-position-widget
                                     :congratulation-getter congratulation-getter))
         (cards
           (flet ((on-next-button (card)
                    ;; Если карточка отвечена неверно, то сбросим ответ
                    ;; и если закончились попытки, то пометим как непройденную.

                    ;; Вынем карточку из общей колоды
                    (setf (cards main-widget)
                          (rest (cards main-widget)))
                                             
                    (cond
                      ((answer-correct-p card)
                       ;; И переложим в успешные
                       (push card
                             (successful-cards main-widget)))
                      (t
                       (decf (num-attempts-left card))
                       (cond
                         ;; Если попытки истекли, то переложим в неуспешные
                         ((zerop (num-attempts-left card))
                          (setf (failed card)
                                t)
                          (push card
                                (failed-cards main-widget)))
                         ;; Если есть ещё попытки, то засунем
                         ;; карточку в конец колоды
                         (t
                          (reset-answer card)
                          (push-end card
                                    (cards main-widget))))))
                    (update main-widget)))
             (loop for user in users
                   for correct-answer = (funcall answer-getter user)
                   for answers = (shuffle
                                  (list* correct-answer
                                         (take 4
                                               (remove correct-answer
                                                       (remove-duplicates
                                                        (random-sample
                                                       ;; Возьмем имен с запасом
                                                         all-answers
                                                         10)
                                                        :test 'string-equal)
                                                       :test 'string-equal))))
                   for card = (make-instance 'card-widget
                                             :user user
                                             :answers answers
                                             :suggest-getter suggest-getter
                                             :correct-answer correct-answer)
                   do (event-emitter:on :next-button card
                                        #'on-next-button)
                   collect card))))
    (setf (cards main-widget)
          cards)
    main-widget))


(defmethod render ((widget guess-position-widget))
  (let* ((card (first (cards widget))))
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
                           (funcall (congratulation-getter widget)
                                    :num-successes (length (successful-cards widget))))
                     (:div :class "text-center"
                           :style "font-size: 200px"
                           "🎉")

                     (let ((game-score (coerce
                                        (ceiling
                                         (float
                                          (/ (* 100 (length
                                                     (successful-cards widget)))
                                             (total-cards widget))))
                                        'integer)))
                       (:div :class "text-center"
                             (render (button "Продолжить обучение!"
                                             :class *green-button-classes*
                                             :on-click (lambda (&rest rest)
                                                         (declare (ignore rest))
                                                         (event-emitter:emit :continue widget
                                                                             game-score)))))))))))))


(defun total-cards (widget)
  (check-type widget guess-position-widget)
  (+ (length (successful-cards widget))
     (length (failed-cards widget))
     (length (cards widget))))


(defun answer-correct-p (widget)
  (check-type widget card-widget)
  (and (answered widget)
       (string-equal (answered widget)
                     (correct-answer widget))))

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
                        (funcall (suggest-getter widget)
                                 (user widget))))
            (:div :class "flex flex-col gap-4"
                  (:div :class "flex flex-col gap-2 pl-8"
                        (loop for answer in (answers widget)
                              for class = (cond
                                            ((answered widget)
                                             (cond
                                               ;; Ответ выбран правильно
                                               ((and (string-equal (answered widget)
                                                                   answer)
                                                     (string-equal (correct-answer widget)
                                                                   answer))
                                                "font-bold text-green-400")
                                               ;; Ответ выбран неверно
                                               ((string-equal (answered widget)
                                                              answer)
                                                "text-red-400")
                                               ((string-equal (correct-answer widget)
                                                              answer)
                                                "text-green-400")
                                               (t
                                                "text-gray-400")))
                                            (t
                                             ""))
                              do (:div :class "flex gap-2"
                                       (:input :type "radio"
                                               :name "answer"
                                               :id answer
                                               :value answer
                                               :disabled (answered widget))
                                       (:label :for answer
                                               :class class
                                               answer))))
                  (:div :class "text-center flex flex-col gap-2"
                        (let ((num-attempts (num-attempts-left widget)))
                          (when (and (not (answer-correct-p widget))
                                     (<= num-attempts (if (answered widget)
                                                          5
                                                          4)))
                            (:div :class "text-gray-500"
                                  (if (answered widget)
                                      (case num-attempts
                                        (5 "Осталось четыре попытки")
                                        (4 "Осталось три попытки")
                                        (3 "Осталось две попытки")
                                        (2 "Осталась последняя попытки")
                                        (1 "В следующий раз стоит поднапрячься!")
                                        (0 "Всё пропало!"))
                                      (case num-attempts
                                        (5 "Осталось пять попыток")
                                        (4 "Осталось четыре попытки")
                                        (3 "Осталось три попытки")
                                        (2 "Осталось две попытки")
                                        (1 "Это последняя попытка")
                                        (0 "Всё пропало!"))))))
                        (if (answered widget)
                            (submit-button :text "Следующая карточка"
                                           :classes *green-button-classes*)
                            (submit-button :text "Проверить"))))))))


(defmethod get-dependencies ((widget guess-position-widget))
  (call-next-method))
