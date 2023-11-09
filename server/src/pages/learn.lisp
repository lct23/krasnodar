(uiop:define-package #:app/pages/learn
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
  (:import-from #:app/widgets/learn
                #:make-learn-widget)
  (:import-from #:reblocks/request
                #:get-path)
  (:import-from #:cl-ppcre
                #:register-groups-bind)
  (:import-from #:app/models/board-progress
                #:get-period-knowledge-progress))
(in-package #:app/pages/learn)


(defwidget learn-page ()
  ())


(defun make-learn-page ()
  (make-instance 'learn-page))


(defmethod render ((widget learn-page))
  (title "Изучение материала")
  
  (register-groups-bind (object-id)
      ("/learn/(\\d+)" (get-path))
    (with-html
      (let* ((period-knowledge-progress (get-period-knowledge-progress object-id)))
        (cond
          (period-knowledge-progress
           ;; TODO: тут надо бы проверить, что у текущего пользователя
           ;; есть права на изучение этого материала и прохождение опроса
           (render (make-learn-widget period-knowledge-progress)))
          (t
           (:p "Знание не найдено")))))
     
    ;; (let* ((user (get-user user-id))
    ;;        (form (make-add-user-form-widget :user user)))
    ;;   (on :object-saved form
    ;;       (lambda (user)
    ;;         (declare (ignore user))
    ;;         (redirect "/personal")))
    ;;   (render form))
    ))
