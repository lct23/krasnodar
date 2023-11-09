(uiop:define-package #:app/pages/user-switch
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html))
(in-package #:app/pages/user-switch)


(defwidget user-switch-page ()
  ())


(defun make-user-switch-page ()
  (make-instance 'user-switch-page))


(defmethod render ((widget user-switch-page))
  (with-html
    (:p "Fill me with code")))
