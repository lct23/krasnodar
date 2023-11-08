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
                #:board))
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
  (flet ((add-user (&key email name department-id mentor-id position avatar-url is-mentor-p is-boss-p board-id &allow-other-keys)
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
                    (:img :src "https://placekitten.com/200/300")
                    (:input :name "avatar-url"
                            :placeholder "Путь до аватарки (позже надо сделать загрузку)"))
             
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
                       (board-select-box "board-id"
                                         :label "Начать онбординг")))))
        (:div :class "flex justify-end mt-8"
              (:button :name "submit"
                       :class "border border-indigo-500 bg-indigo-500 text-white rounded-md px-4 py-2 m-2 transition duration-500 ease select-none hover:bg-indigo-600 focus:outline-none focus:shadow-outline"
                       (if user
                           "Сохранить"
                           "Добавить")))))))
