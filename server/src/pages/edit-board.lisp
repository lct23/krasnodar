(uiop:define-package #:app/pages/edit-board
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/widgets/edit-board-form
                #:make-edit-board-form-widget)
  (:import-from #:app/models/board
                #:board-title
                #:get-board)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:app/pages/utils
                #:title)
  (:import-from #:reblocks/request
                #:get-path))
(in-package #:app/pages/edit-board)


(defwidget edit-board-page ()
  ())


(defun make-edit-board-page ()
  (make-instance 'edit-board-page))


(defmethod render ((widget edit-board-page))
  (cl-ppcre:register-groups-bind (board-id)
      ("/boards/(\\d+)" (get-path))
    (let* ((board (get-board board-id))
           (form (make-edit-board-form-widget board)))
      
      (title (fmt "Редактирование онбординга \"~A\""
                  (board-title board)))
      (render form))))
