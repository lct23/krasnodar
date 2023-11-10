(uiop:define-package #:app/pages/user-switch
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:update
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/models/user
                #:get-all-users
                #:user-avatar-url
                #:get-user
                #:user-name
                #:user)
  (:import-from #:reblocks-auth/models
                #:get-current-user)
  (:import-from #:reblocks/response
                #:redirect)
  (:import-from #:app/widgets/utils
                #:submit-button)
  (:import-from #:reblocks-ui/form
                #:with-html-form)
  (:import-from #:app/pages/utils
                #:title)
  (:import-from #:mito
                #:object-id)
  (:import-from #:reblocks-ui2/widget
                #:ui-widget)
  (:import-from #:event-emitter
                #:event-emitter))
(in-package #:app/pages/user-switch)


(defwidget user-switch-page ()
  ())


(defwidget user-switch-widget (event-emitter ui-widget)
  ((user :type user
         :initarg :user
         :reader user)
   (description :type string
                :initarg :description
                :reader description)))


(defun make-user-switch-widget (user description &key on-switch)
  (let ((widget (make-instance 'user-switch-widget
                               :user user
                               :description description)))
    (when on-switch
      (event-emitter:on :switched widget
                        on-switch))
    (values widget)))


(defun make-user-switch-page ()
  (make-instance 'user-switch-page))


(defmethod render ((widget user-switch-page))
  (title "Переключалка учёток")

  ;; TODO: для прода это надо выставить в NIL
  (let ((allow-for-anonymous t)
        (users-to-show '((32 "Сотрудник HR")
                         (22 "Ментор")
                         (23 "Новый сотрудник"))))
    (flet ((on-switch (subwidget)
             (declare (ignore subwidget))
             ;; (update widget)
             (redirect "/")))
      (with-html
        (cond
          ((or (get-current-user)
               allow-for-anonymous)
           (:div :class "flex flex-col gap-8"
                 (:div
                  (:p "Эта страница сделана для удобства тестирования. Тут можно переключаться на пользователей с разными ролями, чтобы попробовать разный функционал сайта.")
                  (:p "Тут можно переключаться на пользователей с разными ролями, чтобы попробовать разный функционал сайта.")
                  (:p "После выбора пользователя вас \"залогинит\" под его учёткой и перекинет на дашборд."))

                 (:h1 :class "text-xl font-bold text-center"
                      "Сотрудники для просмотра жюри")
                 (:div :class "flex flex-col gap-4"
                       (loop for (user-id title) in users-to-show
                             for user = (get-user user-id)
                             do (render (make-user-switch-widget user title
                                                                 :on-switch #'on-switch))))
                 (:h1 :class "text-xl font-bold text-center"
                      "Остальные все сотрудники (их завтра уберём)")
                
                 (:div :class "flex flex-col gap-4"
                       (loop for user in (get-all-users)
                             for user-id = (mito:object-id user)
                             for name = (user-name user)
                             unless (member user-id users-to-show
                                            :key #'first)
                             do (render (make-user-switch-widget user name
                                                                 :on-switch #'on-switch))))))
          (t
           (:p "Сорян, но тестовая переключалка учёток доступна только залогиновым пользователям.")))))))


(defmethod render ((widget user-switch-widget))
  (flet ((switch (&rest rest)
           (declare (ignore rest))
           (let ((user (user widget)))
             (log:warn "Switching to" user)
             (reblocks/session:reset)
             (setf (get-current-user)
                   user)
             (event-emitter:emit :switched widget
                                 widget)
             ;; (update widget)
             ;; (redirect "/")
             )))
    (let* ((user (user widget))
           (current-p (when (get-current-user)
                        (eql (object-id user)
                             (object-id (get-current-user))))))
      (with-html-form (:post #'switch
                       :class "flex gap-4")
        (:img :style "width: 40px; height: 40px"
              :class (if current-p
                         "border-4 border-red-500")
              :src (user-avatar-url user))
        (:div :class "flex flex-col"
              (:div (user-name user))
              (:div (description widget)))
        (submit-button :text "Переключиться"
                       :disabled current-p)))))
