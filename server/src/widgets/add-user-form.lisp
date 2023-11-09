(uiop:define-package #:app/widgets/add-user-form
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks-ui/form
                #:get-field-errors-count
                #:form-error-placeholder
                #:form-error
                #:field-error
                #:with-html-form)
  (:import-from #:reblocks-ui2/widget
                #:ui-widget)
  (:import-from #:event-emitter
                #:event-emitter)
  (:import-from #:app/models/department
                #:get-department
                #:department-title
                #:get-departments)
  (:import-from #:app/models/user
                #:user-start-work-at
                #:user
                #:user-mentor
                #:get-user
                #:user-position
                #:user-is-mentor-p
                #:user-is-boss-p
                #:user-department
                #:user-avatar-url
                #:user-name)
  (:import-from #:reblocks-auth/providers/email/resend)
  (:import-from #:app/widgets/utils
                #:*button-classes*
                #:board-select-box
                #:mentor-select-box
                #:checkbox
                #:text-input
                #:*select-box-classes*
                #:label
                #:department-select-box)
  (:import-from #:reblocks-auth/providers/email/models
                #:send-code)
  (:import-from #:reblocks-auth/models
                #:get-email
                #:create-social-user)
  (:import-from #:mito
                #:object-id)
  (:import-from #:app/models/board
                #:get-board
                #:board-title)
  (:import-from #:app/models/board-progress
                #:assign-board
                #:user-progress)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:app/models/board-progress
                #:board)
  (:import-from #:str
                #:emptyp)
  (:import-from #:local-time
                #:+iso-8601-date-format+
                #:format-timestring))
(in-package #:app/widgets/add-user-form)


(defparameter *welcome-message*
  (flet ((send-welcome-message (email url)
         (resend:send ("HR Сервис <noreply@lct-krasnodar.dev.40ants.com>"
                       email
                       "Адаптация в компании, ссылка для входа")
           (:p
            ("Чтобы начать процесс адаптации, пройдите по [этой ссылке](~A)."
             url url))
           (:p "Ссылка действительна в течении часа."))))
    (reblocks-auth/providers/email/resend::make-code-sender #'send-welcome-message)))


(defwidget add-user-form-widget (event-emitter ui-widget)
  ((user :type (or null user)
         :initform nil
         :initarg :user
         :accessor user)))


(defun make-add-user-form-widget (&key user)
  (make-instance 'add-user-form-widget
                 :user user))


(defmethod reblocks-ui2/widget:render ((widget add-user-form-widget) theme)
  (flet ((add-user (&key email name department-id start-work-at mentor-id position avatar-url is-mentor-p is-boss-p board-id &allow-other-keys)
           ;; Валидация
           (when (emptyp email)
             (field-error "email"
                          "Емейл должен быть заполнен"))
           (when (emptyp name)
             (field-error "name"
                          "Имя должно быть заполнено"))
           (when (emptyp position)
             (field-error "position"
                          "Должность должна быть заполнена"))
           (cond
             ((emptyp start-work-at)
              (field-error "name"
                           "Имя должно быть заполнено"))
             (t
              (handler-case (local-time:parse-timestring start-work-at)
                (local-time:invalid-timestring ()
                  (field-error "start-work-at"
                               "Дата начала работы должна быть в формате: 2023-11-30")))))
           
           (unless (zerop (get-field-errors-count))
             (form-error "Некоторые поля заполнены неверно"))
           
           (let ((user (user widget))
                 (user-created nil))
             (cond
               (user
                (log:info "Editing user with" email))
               (t
                ;; Заранее зарегистрируем новую учётку
                (log:info "Adding user with" email)
                (setf user
                      (create-social-user :email
                                          email
                                          :email email)
                      user-created t)))

             (unless (str:emptyp board-id)
               (assign-board user
                             (get-board board-id)))
             
             ;; Проставим дополнительные поля
             (setf (user-name user) name
                   (user-avatar-url user) avatar-url
                   (user-department user) (get-department department-id)
                   (user-start-work-at user) start-work-at
                   (user-mentor user) (get-user mentor-id)
                   (user-position user) position
                   (user-is-mentor-p user) (string-equal is-mentor-p "true")
                   (user-is-boss-p user) (string-equal is-boss-p "true"))

             (mito:save-dao user)
             
             (when user-created
               ;; и отправим сотруднику email, если это новый сотрудник
               (send-code email
                          :send-callback *welcome-message*))
             (event-emitter:emit (if user-created
                                     :object-created
                                     :object-saved)
                                 widget
                                 user)

             ;; Обновим себя, чтобы сбросить форму.
             (reblocks/widget:update widget))))
    
    (with-html-form (:post #'add-user
                     :class "w-full my-8 flex flex-col gap-2")
      (let ((user (user widget)))
        (:div :class "flex gap-8"
              (:div :class "flex flex-col gap-4"
                    (if user
                        (:img :style "width: 200px"
                              :src (user-avatar-url user))
                        (:div :style "width: 200px;height: 200px" 
                              :class "bg-gray-200"))

                    (text-input "avatar-url"
                                :placeholder "Путь до аватарки (позже надо сделать загрузку)"
                                :value (user-avatar-url user)))
             
              (:div :class "w-full flex flex-col"
                    (text-input "email" :type "email"
                                        :placeholder "Email сотрудника"
                                        :label "Email"
                                        :value (when user
                                                 (get-email user)))
                    (text-input "name" :placeholder "Имя сотрудника"
                                       :label "ФИО"
                                       :value (when user
                                                (user-name user)))

                    (department-select-box "department-id"
                                           :label "Отдел"
                                           :selected-department-id (when user
                                                                     (object-id
                                                                      (user-department user))))

                    (text-input "start-work-at"
                                :placeholder "В формате: 2023-11-30"
                                :label "Дата выхода на работу"
                                :value (when user
                                         (format-timestring
                                          nil
                                          (user-start-work-at user)
                                          :format +iso-8601-date-format+)))

                    (mentor-select-box "mentor-id"
                                       :label "Ментор"
                                       :selected-mentor-id (when (and user
                                                                      (user-mentor user))
                                                             (object-id
                                                              (user-mentor user))))

                    (text-input "position" :placeholder "Должность"
                                           :label "Должность"
                                           :value (when user
                                                    (user-position user)))

                    (:div :class "flex"
                          (checkbox "is-boss-p"
                                    :label "Начальник"
                                    :checked (when user
                                               (user-is-boss-p user)))

                          (checkbox "is-mentor-p"
                                    :label "Может быть ментором"
                                    :checked (when user
                                               (user-is-mentor-p user))))
                    ;; Назначение онбординга
                    (cond
                      ((user-progress user)
                       (:p (fmt "Сотрудник уже проходит онбординг: ~A"
                                (board-title (board (user-progress user))))))
                      (t
                       (:span "")
                       ;; Ручное назначение онбординга пока отключено,
                       ;; решили сделать автоматическое назначение по крону:
                       ;; (board-select-box "board-id"
                       ;;                   :label "Начать онбординг")
                       ))

                    (form-error-placeholder)))
        (:div :class "flex justify-end mt-8"
              (:button :name "submit"
                       :class *button-classes*
                       (if user
                           "Сохранить"
                           "Добавить")))))))
