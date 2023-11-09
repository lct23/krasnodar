(uiop:define-package #:app/pages/user-switch
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/models/user
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
                #:title))
(in-package #:app/pages/user-switch)


(defwidget user-switch-page ()
  ())


(defwidget user-switch-widget ()
  ((user :type user
         :initarg :user
         :reader user)
   (description :type string
                :initarg :description
                :reader description)))


(defun make-user-switch-widget (user description)
  (make-instance 'user-switch-widget
                 :user user
                 :description description))


(defun make-user-switch-page ()
  (make-instance 'user-switch-page))


(defmethod render ((widget user-switch-page))
  (title "Переключалка учёток")
  
  (with-html
    (:div :class "flex flex-col gap-8"
          (:div
           (:p "Эта страница сделана для удобства тестирования. Тут можно переключаться на пользователей с разными ролями, чтобы попробовать разный функционал сайта.")
           (:p "Тут можно переключаться на пользователей с разными ролями, чтобы попробовать разный функционал сайта.")
           (:p "После выбора пользователя вас \"залогинит\" под его учёткой и перекинет на дашборд."))

          (:div :class "flex flex-col gap-4"
                (render (make-user-switch-widget
                         (get-user 32) "Сотрудник HR"))
                (render (make-user-switch-widget
                         (get-user 22) "Ментор"))
                (render (make-user-switch-widget
                         (get-user 23) "Новый сотрудник"))))))


(defmethod render ((widget user-switch-widget))
  (flet ((switch (&rest rest)
           (declare (ignore rest))
           (let ((user (user widget)))
             (log:warn "Switching to" user)
             (setf (get-current-user)
                   user)
             (redirect "/"))))
    (with-html-form (:post #'switch
                     :class "flex gap-4")
      (:img :style "width: 40px; height: 40px"
            :src (user-avatar-url (user widget)))
      (:div :class "flex flex-col"
            (:div (user-name (user widget)))
            (:div (description widget)))
      (submit-button :text "Переключиться"))))
