(uiop:define-package #:app/pages/chats
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/pages/utils
                #:title))
(in-package #:app/pages/chats)


(defwidget chats-widget ()
  ())


(defun make-chats-widget ()
  (make-instance 'chats-widget))


(defmethod render ((widget chats-widget))
  (title "Чаты")
  
  (with-html
    (:p "Чаты пока не реализованы.")))

