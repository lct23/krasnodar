(uiop:define-package #:app/pages/user
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
                #:get-user)
  (:import-from #:app/widgets/board-progress
                #:make-board-progress-widget)
  (:import-from #:app/pages/utils
                #:title))
(in-package #:app/pages/user)


(defwidget user-dashboard-page ()
  ())


(defun make-user-dashboard-page ()
  (make-instance 'user-dashboard-page))


(defmethod render ((widget user-dashboard-page))
  (title "Дашборд")
  
  (let ((user (get-user 1)
              ;; (reblocks-auth/models:get-current-user)
              ))
    (with-html
      (cond
        (user
         (let ((progress (user-progress user)))
           (render (make-board-progress-widget progress))))
        (t
         (:p "План онбординга пока не назначен, свяжитесь с HR."))))))
