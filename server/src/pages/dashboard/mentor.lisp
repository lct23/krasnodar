(uiop:define-package #:app/pages/dashboard/mentor
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/models/board-progress
                #:user-progress)
  (:import-from #:app/models/user
                #:get-mentee
                #:user-start-work-at
                #:user-name
                #:get-user)
  (:import-from #:app/widgets/board-progress
                #:make-board-progress-widget)
  (:import-from #:app/pages/utils
                #:title)
  (:import-from #:app/widgets/user
                #:make-user-widget)
  (:import-from #:reblocks-auth/models
                #:get-current-user)
  (:import-from #:local-time
                #:+iso-8601-date-format+
                #:format-timestring
                #:timestamp>
                #:now)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:app/pages/dashboard/user
                #:render-dashboard-content
                #:user-dashboard-page)
  (:import-from #:mito
                #:object-id))
(in-package #:app/pages/dashboard/mentor)


(defwidget mentor-dashboard-page (user-dashboard-page)
  ()
  (:default-initargs :page-title "Дашборд Ментора"))


(defun make-mentor-dashboard-page ()
  (make-instance 'mentor-dashboard-page))


(defmethod render-dashboard-content ((widget mentor-dashboard-page))
  (let ((mentee (get-mentee (get-current-user))))
    (with-html
      (:div :class "flex flex-col gap-8 my-8 mx-20 p-8 shadow-xl border rounded"
            (cond
              (mentee
               (:div :class "text-xl font-bold"
                     "Ваши подопечные:")
          
               (:ul :class "list-disc list-inside"
                    (loop for person in mentee
                          for link = (fmt "/personal/~A"
                                          (object-id person))
                          do (:li
                              (:a :class "text-blue-500"
                                  :href link
                                  (user-name person)))))
               (:div "Проверьте как у них дела с прохождением онбординга."))
              (t
               (:div :class "text-xl font-bold"
                     "У вас пока нет подопечных.")))))))


(defmethod render ((widget mentor-dashboard-page))
  
  (call-next-method))
