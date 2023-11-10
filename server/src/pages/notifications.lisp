(uiop:define-package #:app/pages/notifications
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
  (:import-from #:3bmd)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:app/pages/utils
                #:title)
  (:import-from #:reblocks-ui2/buttons/button
                #:button)
  (:import-from #:app/widgets/utils
                #:*dangerous-button-classes*)
  (:import-from #:app/models/notification
                #:notification-title
                #:notification-text
                #:notification-seen-at
                #:critical-notification-p
                #:mark-as-seen
                #:get-user-notifications)
  (:import-from #:reblocks-auth/models
                #:get-current-user)
  (:import-from #:mito
                #:object-created-at)
  (:import-from #:local-time
                #:format-timestring)
  (:import-from #:app/utils
                #:+human-datetime-format+))
(in-package #:app/pages/notifications)


(defwidget notifications-page ()
  ((sent :initform nil
         :accessor sent)))


(defun make-notifications-page ()
  (make-instance 'notifications-page))


(defmethod render ((widget notifications-page))
  (let* ((user (get-current-user))
         (notifications (when user
                          (get-user-notifications (get-current-user))))
         ;; Если есть непрочитанные, то будем показывать кнопку пометки "как прочитанные"
         (has-unread-p
           (remove-if #'notification-seen-at
                      notifications)))
    
    (title "Уведомления")

    (with-html
      (:div :class "flex flex-col gap-8"
            (cond
              ;; отображение уведомлений
              (notifications
               (when has-unread-p
                 (:div
                  (render (button "Пометить прочитанными"
                                  :on-click (lambda (&rest rest)
                                              (declare (ignore rest))
                                              (mark-as-seen user)
                                              (update widget))
                                  :class *dangerous-button-classes*))))
               (:style "
.document-text a {
color: blue;
}
")
               (:div :class "flex flex-col gap-8"
                     (loop for notification in notifications
                           for date = (object-created-at notification)
                           for title = (notification-title notification)
                           for text = (notification-text notification)
                           for seen = (notification-seen-at notification)
                           for critical = (and (not seen)
                                               (critical-notification-p notification))
                           do (:div :class "flex flex-col gap-2"
                                    (:div :class "flex gap-2"
                                          (:span :class "bg-gray-700 text-gray-200 rounded-full px-2"
                                                 (format-timestring nil date
                                                                    :format +human-datetime-format+))
                                          (:span :class (cond
                                                          (critical
                                                           "text-red-400 font-bold")
                                                          (seen
                                                           "text-gray-400")
                                                          (t
                                                           "text-gray-800 font-bold"))
                                                 title))
                                    (:div :class "document-text"
                                          (3bmd:parse-string-and-print-to-stream text
                                                                                 reblocks/html:*stream*))))))
              (t
               (:h1 "Пока нет уведомлений")))))))
