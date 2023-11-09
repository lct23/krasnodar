(uiop:define-package #:app/emails/welcome
  (:use #:cl)
  (:import-from #:reblocks-auth/providers/email/resend
                #:make-code-sender)
  (:import-from #:reblocks-auth/providers/email/models
                #:send-code)
  (:import-from #:resend
                #:send)
  (:import-from #:reblocks-auth/models
                #:get-email)
  (:import-from #:app/models/user
                #:user)
  (:import-from #:str
                #:ends-with-p))
(in-package #:app/emails/welcome)


(defparameter *welcome-message*
  (flet ((send-welcome-message (email url)
           (send ("HR Сервис <noreply@hrzero.ru>"
                  email
                  "Адаптация в компании, ссылка для входа")
             (:p
              ("Чтобы начать процесс адаптации, пройдите по [этой ссылке](~A)."
               url url))
             (:p "Ссылка действительна в течении часа."))))
    (make-code-sender #'send-welcome-message
                      :base-uri "https://hrzero.ru/")))


(defun send-welcome-message (user)
  (check-type user user)
  (let* ((email (get-email user))
         (allowed-email (ends-with-p "@svetlyak.ru"
                                     email)))
    (cond
      (allowed-email
       (log:info "Sending welcome message to" email)
       (send-code email
                  :send-callback *welcome-message*))
      (t
       (log:info "Not sending email because address is not allowed" email)))))
