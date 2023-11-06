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
                #:render-sent-message
                #:render-email-input
                #:form-css-classes
                #:render-submit-button
                #:request-code-form))
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
       :button-class "border border-indigo-500 bg-indigo-500 text-white rounded-md px-4 py-2 m-2 transition duration-500 ease select-none hover:bg-indigo-600 focus:outline-none focus:shadow-outline"))))


(defmethod render ((widget login-widget))
  (with-html-form (:post #'request-link)
    
    (:p "Fill me with code")))


;; (defmethod get-dependencies ((widget login-widget))
;;   (list*
;;    (reblocks-lass:make-dependency
;;      `(.login-widget
;;        :color red))
;;    (call-next-method)))


(defwidget custom-login-form (request-code-form)
  ())


(defmethod reblocks-auth:render-login-page ((app app/app::app) &key retpath)
  (with-html
    (:div :class "p-10"
          (reblocks/widget:render
           (make-instance 'custom-login-form
                          :retpath retpath)))))

(defmethod render-email-input ((widget custom-login-form))
  (with-html
    (:input :name "email"
            :type "email"
            :class "border px-2"
            :placeholder "Ваш email")))


(defmethod render-submit-button ((widget custom-login-form))
  (with-html
    (:input :type "submit"
            :class "border border-indigo-500 bg-indigo-500 text-white rounded-md px-4 py-2 m-2 transition duration-500 ease select-none hover:bg-indigo-600 focus:outline-none focus:shadow-outline"
            :value "Войти")))


(defmethod form-css-classes ((widget custom-login-form))
  (list "flex"))


(defmethod render-sent-message ((widget custom-login-form))
  (with-html
    (:p "Ссылка для входа была отправлена на ваш email.")))


(define-code-sender send-code ("HR Сервис <noreply@lct-krasnodar.dev.40ants.com>" url :subject "Ссылка для входа на сайт")
    (:p ("Чтобы войти на [lct-krasnodar.dev.40ants.com](~A), пройдите по [этой ссылке](~A)."
       url
       url))
  (:p "Ссылка действительна в течении часа."))
