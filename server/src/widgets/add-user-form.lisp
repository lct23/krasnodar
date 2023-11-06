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
                #:event-emitter))
(in-package #:app/widgets/add-user-form)


(defwidget add-user-form-widget (event-emitter ui-widget)
  ;; TODO: remove on-add
  ((on-add :initform nil
           :initarg :on-add
           :reader on-add)))


(defun make-add-user-form-widget (&key on-add)
  (make-instance 'add-user-form-widget
                 :on-add on-add))


;; TODO: remove
;; (defmethod render ((widget add-user-form-widget))
;;   (flet ((add-user (&key email role &allow-other-keys)
;;            (log:info "Adding user with" email)

;;            ;; Заранее зарегистрируем учётку
;;            (let ((user (reblocks-auth/models:create-social-user :email
;;                                                                 email
;;                                                                 :email email)))
;;              ;; Пропишем ему роль сотрудника
;;              (app/models/roles::give-a-role user role)
;;              ;; и отправим сотруднику email
;;              ;; TODO: надо придумать как сделать другой шаблон емейла
;;              (reblocks-auth/providers/email/models::send-code email)
;;              (when (on-add widget)
;;                (funcall (on-add widget)))

;;              (event-emitter:emit :object-created widget
;;                                  user)

;;              ;; Обновим себя, чтобы сбросить форму.
;;              (reblocks/widget:update widget))))
    
;;     (with-html-form (:post #'add-user
;;                      :class "w-full my-8")
;;       (:input :name "email"
;;               :placeholder "Email сотрудника")
;;       (:select :name "role"
;;         (:option :value "employee"
;;                  :selected t
;;                  "Новый сотрудник")
;;         (:option :value "boss"
;;                  "Руководитель")
;;         (:option :value "hr"
;;                  "HR")
;;         (:option :value "mentor"
;;                  "Ментор"))
;;       (:button :name "submit"
;;                :class "border border-indigo-500 bg-indigo-500 text-white rounded-md px-4 py-2 m-2 transition duration-500 ease select-none hover:bg-indigo-600 focus:outline-none focus:shadow-outline"
;;                "Добавить"))))


(defmethod reblocks-ui2/widget:render ((widget add-user-form-widget) theme)
  (flet ((add-user (&key email role &allow-other-keys)
           (log:info "Adding user with" email)

           ;; Заранее зарегистрируем учётку
           (let ((user (reblocks-auth/models:create-social-user :email
                                                                email
                                                                :email email)))
             ;; Пропишем ему роль сотрудника
             (app/models/roles::give-a-role user role)
             ;; и отправим сотруднику email
             ;; TODO: надо придумать как сделать другой шаблон емейла
             (reblocks-auth/providers/email/models::send-code email)
             (when (on-add widget)
               (funcall (on-add widget)))

             (event-emitter:emit :object-created widget
                                 user)

             ;; Обновим себя, чтобы сбросить форму.
             (reblocks/widget:update widget))))
    
    (with-html-form (:post #'add-user
                     :class "w-full my-8")
      (:input :name "email"
              :placeholder "Email сотрудника")
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


;; (defmethod get-dependencies ((widget add-user-form-widget))
;;   (list*
;;    (reblocks-lass:make-dependency
;;      `(.add-user-form-widget
;;        :color red))
;;    (call-next-method)))
