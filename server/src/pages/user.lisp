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
                #:make-user-widget))
(in-package #:app/pages/user)


(defwidget user-page ()
  ())


(defun make-user-page ()
  (make-instance 'user-page))


(defmethod render ((widget user-page))
  (cl-ppcre:register-groups-bind (user-id)
      ("/personal/(\\d+)" (get-path))
    (let* ((user (get-user user-id))
           (widget (make-user-widget user)))
      
      (title (user-name user))
      (render widget))))
