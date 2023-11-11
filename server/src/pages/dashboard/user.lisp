(uiop:define-package #:app/pages/dashboard/user
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
                #:fmt))
(in-package #:app/pages/dashboard/user)


(defwidget user-dashboard-page ()
  ((page-title :initform  "Дашборд сотрудника"
               :initarg :page-title
               :reader page-title)))


(defun make-user-dashboard-page ()
  (make-instance 'user-dashboard-page))


(defgeneric render-dashboard-content (widget)
  (:method ((widget user-dashboard-page))
      (let ((user ;; (get-user 1)
          (get-current-user)))
    (with-html
      (cond
        (user
         (let ((progress (user-progress user))
               (start-at (user-start-work-at user)))
           (cond
             (progress (render (make-board-progress-widget progress)))
             (t
              (cond
                ((and start-at
                      (timestamp> start-at (now)))
                 (:p (fmt "Онбординг начнётся ~A, свяжитесь с HR."
                          (format-timestring nil start-at
                                             :format +iso-8601-date-format+))))
                (t
                 (:p "Онбординг должен был начаться, но что-то пошло не так, свяжитесь с HR.")))))))
        (t
         (:p "Чтобы работать с системой, надо залогиниться.")))))))


(defmethod render ((widget user-dashboard-page))
  (title (page-title widget))
  
  (render-dashboard-content widget))
