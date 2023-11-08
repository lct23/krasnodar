(uiop:define-package #:app/pages/edit-user
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
                #:get-user))
(in-package #:app/pages/edit-user)


(defwidget edit-user-page ()
  ())


(defun make-edit-user-page ()
  (make-instance 'edit-user-page))


(defmethod render ((widget edit-user-page))
  (title "Редактирование сотрудника")

  (cl-ppcre:register-groups-bind (user-id)
      ("/personal/(\\d+)/edit" (get-path))
    (let* ((user (get-user user-id))
           (form (make-add-user-form-widget :user user)))
      (on :object-saved form
          (lambda (user)
            (declare (ignore user))
            (redirect "/personal")))
      (render form))))
