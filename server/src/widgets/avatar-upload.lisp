(uiop:define-package #:app/widgets/avatar-upload
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies))
(in-package #:app/widgets/avatar-upload)

;; https://tailwindcomponents.com/component/upload-user-photo

(defwidget avatar-upload-widget ()
  ())


(defun make-avatar-upload-widget ()
  (make-instance 'avatar-upload-widget))


(defmethod render ((widget avatar-upload-widget))
  (with-html
    (:p "Fill me with code")))
