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
                #:user-department
                #:user-avatar-url
                #:user-name)
  (:import-from #:reblocks-auth/providers/email/resend))
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
  ())


(defun make-add-user-form-widget ()
  (make-instance 'add-user-form-widget))


(defmethod reblocks-ui2/widget:render ((widget add-user-form-widget) theme)
  (flet ((add-user (&key email role name department-id avatar-url &allow-other-keys)
           (log:info "Adding user with" email)

           ;; Заранее зарегистрируем учётку
           (let ((user (reblocks-auth/models:create-social-user :email
                                                                email
                                                                :email email)))
             ;; Пропишем ему роль сотрудника
             ;; (app/models/roles::give-a-role user role)

             ;; Проставим дополнительные поля
             (setf (app/models/user::user-roles user)
                   (make-array 1 :initial-element role)
                   (user-name user) name
                   (user-avatar-url user) avatar-url
                   (user-department user) (get-department department-id))
             (mito:save-dao user)
             
             ;; и отправим сотруднику email
             ;; TODO: надо придумать как сделать другой шаблон емейла
             (reblocks-auth/providers/email/models::send-code email
                                                              :send-callback *welcome-message*)
             (event-emitter:emit :object-created widget
                                 user)

             ;; Обновим себя, чтобы сбросить форму.
             (reblocks/widget:update widget))))
    
    (with-html-form (:post #'add-user
                     :class "w-full my-8")
      (:input :name "email"
              :placeholder "Email сотрудника")
      (:input :name "name"
              :placeholder "Имя")
      (:input :name "avatar-url"
              :placeholder "Путь до аватарки (позже надо сделать загрузку)")
      
      (:select :name "department-id"
        (loop for dep in (get-departments)
              do (:option :value (princ-to-string
                                  (mito:object-id dep))
                          (department-title dep))))
      
      (:select :name "role"
        (:option :value "employee"
                 :selected t
                 "Новый сотрудник")
        (:option :value "boss"
                 "Руководитель")
        (:option :value "hr"
                 "HR")
        (:option :value "mentor"
                 "Ментор"))
      (:button :name "submit"
               :class "border border-indigo-500 bg-indigo-500 text-white rounded-md px-4 py-2 m-2 transition duration-500 ease select-none hover:bg-indigo-600 focus:outline-none focus:shadow-outline"
               "Добавить"))))
