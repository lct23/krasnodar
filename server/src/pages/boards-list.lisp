(uiop:define-package #:app/pages/boards-list
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/pages/utils
                #:title)
  (:import-from #:app/widgets/boards-list
                #:make-boards-list-widget)
  (:import-from #:app/widgets/add-board-form
                #:make-add-board-form-widget))
(in-package #:app/pages/boards-list)


(defwidget boards-list-page ()
  ())


(defun make-boards-list-page ()
  (make-instance 'boards-list-page))


(defmethod render ((widget boards-list-page))
  (title "Онбординги")
  
  (with-html
    (:div :class "flex flex-col gap-8"
          (render (make-boards-list-widget))
          (:div :class "flex justify-end"
                (render (make-add-board-form-widget))))))
