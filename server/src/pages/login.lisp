(uiop:define-package #:app/pages/login
  (:use #:cl)
  (:import-from #:reblocks-auth/providers/email/resend
                #:define-code-sender)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks-ui/form
                #:render-form-and-button
                #:with-html-form)
  (:import-from #:reblocks-ui/popup
                #:show-popup)
  (:import-from #:reblocks-auth/providers/email/processing
                #:sent
                #:render-sent-message
                #:render-email-input
                #:form-css-classes
                #:render-submit-button
                #:request-code-form)
  (:import-from #:app/widgets/utils
                #:text-input
                #:*button-classes*)
  (:import-from #:app/pages/utils
                #:title))
(in-package #:app/pages/login)


(defwidget login-widget ()
  ())


(defun make-login-page ()
  (make-instance 'login-widget))


(defmethod reblocks-auth/button:render ((service (eql :email))
                                        &key retpath)
  (let ((popup (make-instance 'reblocks-auth/providers/email/processing::email-query-widget
                              :retpath retpath)))
    (with-html
      (reblocks/widget:render popup)

      (render-form-and-button
       :email (lambda (&rest args)
                (declare (ignore args))
                (show-popup popup))
       :button-class *button-classes*))))


(defmethod render ((widget login-widget))
  (with-html-form (:post #'request-link)
    
    (:p "Fill me with code")))


(defwidget custom-login-form (request-code-form)
  ())


(defmethod reblocks-auth:render-login-page ((app app/app::app) &key retpath)
  (title "Вход в систему")
  (with-html
    (:div :class "w-1/2 mx-auto mt-20 border rounded shadow-xl p-8 flex flex-col gap-8"
          (:p :class "text-xl font-bold"
              "Вход доступен только для сотрудников.")
          (:div :class "flex flex-col gap-4"
                (:p "Введите свой email:")
                (reblocks/widget:render
                 (make-instance 'custom-login-form
                                :retpath retpath))))))

(defmethod render-email-input ((widget custom-login-form))
  (with-html
    (:div :class "flex flex-col gap-4 w-full"
          (text-input "email"
                      :type "email"
                      :placeholder "Ваш email")
          (unless (sent widget)
            (:p :class "text-gray-500"
                :style "margin-top: -2rem"
                "Мы вышлем вам ссылку для входа.")))))


(defmethod render-submit-button ((widget custom-login-form))
  (with-html
    (app/widgets/utils::submit-button :text "Войти")))


(defmethod form-css-classes ((widget custom-login-form))
  (list "flex"))


(defmethod render-sent-message ((widget custom-login-form))
  (with-html
    (:p :style "margin-bottom: 2rem"
        "Ссылка для входа была отправлена на ваш email.")))


(define-code-sender send-code ("HR Сервис <noreply@hrzero.ru>" url :subject "Ссылка для входа на сайт")
    (:p ("Чтобы войти на [hrzero.ru](~A), пройдите по [этой ссылке](~A)."
       url
       url))
  (:p "Ссылка действительна в течении часа."))
