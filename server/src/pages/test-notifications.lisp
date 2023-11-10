(uiop:define-package #:app/pages/test-notifications
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:update
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/html)
  (:import-from #:app/notifications/creator
                #:create-notifications-for-all-users)
  (:import-from #:app/models/user
                #:user-name)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:app/pages/utils
                #:title)
  (:import-from #:reblocks-ui2/buttons/button
                #:button)
  (:import-from #:app/widgets/utils
                #:*dangerous-button-classes*)
  (:import-from #:app/utils
                #:markdown-to-html))
(in-package #:app/pages/test-notifications)


(defwidget test-notifications-page ()
  ((sent :initform nil
         :accessor sent)))


(defun make-test-notifications-page ()
  (make-instance 'test-notifications-page))


(defmethod render ((widget test-notifications-page))
  (let ((all-notifications (unless (sent widget)
                             (remove-if #'null (create-notifications-for-all-users :dry-run t)))))

    (title "Тест отправки уведомлений")

    (with-html
      (:div :class "flex flex-col gap-8"
            (:div :class "text-xl"
                  (:p "Примечание: в реальной системе рассылка уведомлений будет осуществляться событийно или по расписанию.")
                  (:p "Эта страница нужна лишь чтобы облегчить демонстрацию фичи.")
                  (:p "В MVP рассылка на емейл отключена, чтобы не забанили за спам.")
                  (:p "Сообщения появляются только в разделе \"Уведомления\"!"))
            (cond
              ((sent widget)
               (:p "Класс! Уведомления разосланы! Переключитесь на другого сотрудника через тестовую переключалку, чтобы посмотреть раздел \"Уведомления\":")
               (:p (:a :class "text-blue-400"
                       :href "/switch"
                       "Тестовая переключка пользователей")))
              ;; отображение уведомлений
              (all-notifications
               (:h1 (fmt "В настоящий момент готово к отправке ~A уведомлений."
                         (length all-notifications))
                    (render (button "Отправить"
                                    :on-click (lambda (&rest rest)
                                                (declare (ignore rest))
                                                (create-notifications-for-all-users)
                                                (setf (sent widget)
                                                      t)
                                                (update widget))
                                    :class *dangerous-button-classes*)))
               (:style "
.document-text a {
color: blue;
}
")
               (:div :class "flex flex-col gap-8"
                     (loop for (user title text critical) in all-notifications
                           do (:div :class "flex flex-col gap-2"
                                    (:h2 (:span :class "text-gray-400"
                                                "Сотрудник: ")
                                         (user-name user))
                                    (:h3 (:span :class "text-gray-400"
                                                "Заголовок: ")
                                         (:span :class (when critical
                                                         "text-red-400")
                                                title))
                                    (:div :class "document-text"
                                          (markdown-to-html text))))))
              (t
               (:h1 "Пока нет уведомлений для отправки.")))))))
