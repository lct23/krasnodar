(uiop:define-package #:app/pages/add-user
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
  (:import-from #:app/models/roles
                #:hr-p)
  (:import-from #:reblocks-auth/models
                #:get-current-user))
(in-package #:app/pages/add-user)


(defwidget add-user-page ()
  ())


(defun make-add-user-page ()
  (make-instance 'add-user-page))


(defmethod render ((widget add-user-page))
  (title "Добавить сотрудника")

  ;; Редактировать может только HR
  (cond
    ((hr-p (get-current-user))
     (let ((form (make-add-user-form-widget)))
       (on :object-created form
           (lambda (user)
             (declare (ignore user))
             (redirect "/personal")))
       (render form)))
    (t
     (with-html
       (:p "Добавлять сотрудников может только HR.")))))
