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
                #:user-name
                #:get-user)
  (:import-from #:app/widgets/user
                #:make-user-widget)
  (:import-from #:app/models/roles
                #:hr-p)
  (:import-from #:app/widgets/board-progress-for-hr
                #:make-board-progress-widget-for-hr))
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
                   (when (hr-p (get-current-user))
                     (with-html
                       (:div :class "flex flex-col gap-4"
                             (:h1 :class "text-xl font-bold text-center"
                                  "Прогресс по онбордингу")
                             (render (make-board-progress-widget-for-hr user)))))))))
        (t
         (title "Сотрудник не найден."))))))
