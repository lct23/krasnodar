(uiop:define-package #:app/pages/user
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/pages/utils
                #:title)
  (:import-from #:reblocks/response
                #:redirect)
  (:import-from #:event-emitter
                #:on)
  (:import-from #:app/widgets/add-user-form
                #:make-add-user-form-widget)
  (:import-from #:reblocks/request
                #:get-path)
  (:import-from #:app/models/user
                #:user-is-mentor-p
                #:user-name
                #:get-user)
  (:import-from #:app/widgets/user
                #:make-user-widget)
  (:import-from #:app/models/roles
                #:hr-p)
  (:import-from #:app/widgets/board-progress-for-hr
                #:make-board-progress-widget-for-hr)
  (:import-from #:app/models/board-progress
                #:user-progress))
(in-package #:app/pages/user)


(defwidget user-page ()
  ())


(defun make-user-page ()
  (make-instance 'user-page))


(defmethod render ((widget user-page))
  (cl-ppcre:register-groups-bind (user-id)
      ("/personal/(\\d+)" (get-path))
    (let* ((user (get-user user-id)))
      (cond
        (user
         (let ((widget (make-user-widget user)))
      
           (title (user-name user))
           (with-html
             (:div :class "flex flex-col gap-8"
                   (render widget)
                   (when (and (get-current-user)
                              (or (hr-p (get-current-user))
                                  (user-is-mentor-p (get-current-user))))
                     (cond
                       ;; У текущего сотрудника начат онбординг
                       ((user-progress user)
                        (with-html
                          (:div :class "flex flex-col gap-4"
                                (:h1 :class "text-xl font-bold text-center"
                                     "Прогресс по онбордингу")
                                (render (make-board-progress-widget-for-hr user)))))
                       (t
                        (:div :class "text-xl font-bold text-center"
                              "Онбординг пока не стартовал, потому что сотрудник пока не вышел на работу."))))))))
        (t
         (title "Сотрудник не найден."))))))
