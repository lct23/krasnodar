(uiop:define-package #:app/widgets/user
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
                #:delete-user
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
                #:inline-label
                #:*dangerous-button-classes*
                #:submit-button
                #:redirect-button
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
                #:get-current-user
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
                #:format-timestring)
  (:import-from #:app/models/roles
                #:hr-p)
  (:import-from #:reblocks/response
                #:redirect))
(in-package #:app/widgets/user)


(defwidget user-widget ()
  ((user :type user
         :initarg :user
         :reader user)))


(defun make-user-widget (user)
  (make-instance 'user-widget
                 :user user))


(defmethod render ((widget user-widget))
  (with-html
    (let ((user (user widget))
          (field-group-classes "flex gap-2"))
      (:div :class "flex gap-8"
            (:div :class "flex flex-col gap-4"
                  (:img :style "width: 200px"
                        :src (user-avatar-url user)))
            
            (:div :class "w-full flex flex-col gap-4"
                  (:div :class field-group-classes
                        (inline-label "Email")
                        (get-email user))
                  (:div :class field-group-classes
                        (inline-label "Имя")
                        (user-name user))
                  (:div :class field-group-classes
                        (inline-label "Отдел")
                        (department-title
                         (user-department user)))

                  (:div :class field-group-classes
                        (inline-label "Должность")
                        (user-position user))

                  (when (user-start-work-at user)
                    (:div :class field-group-classes
                          (inline-label "Дата выхода на работу")
                          (format-timestring
                           nil
                           (user-start-work-at user)
                           :format +iso-8601-date-format+)))

                  (when (and user
                             (user-mentor user))
                    (:div :class field-group-classes
                          (inline-label "Имя ментора")
                          (user-name
                           (user-mentor user))))

                  (:div :class "flex"
                        :style "margin-left: -0.9rem"
                        (checkbox "is-boss-p"
                                  :label "Начальник"
                                  :checked (user-is-boss-p user)
                                  :disabled t)

                        (checkbox "is-mentor-p"
                                  :label "Может быть ментором"
                                  :checked (user-is-mentor-p user)
                                  :disabled t))

                  (when (user-progress user)
                    (:p (fmt "Сотрудник уже проходит онбординг: ~A"
                             (board-title (board (user-progress user))))))))
      ;; Редактировать может только HR
      (when (hr-p (get-current-user))
        (:div :class "flex justify-end mt-8"
              (redirect-button "Редактировать"
                               (fmt "/personal/~A/edit"
                                    (object-id user)))
              (flet ((on-delete (&rest rest)
                       (declare (ignore rest))
                       (delete-user user)
                       (redirect "/personal")))
                ;; Пока удаляем без подтверждения.
                (with-html-form (:post #'on-delete)
                  (submit-button :text "Удалить"
                                 :classes *dangerous-button-classes*))))))))
