(uiop:define-package #:app/pages/del-board
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/widgets/del-board
                #:make-del-board-widget)
  (:import-from #:app/pages/utils
                #:title)
  (:import-from #:app/models/board
                #:board-title
                #:get-board)
  (:import-from #:app/widgets/edit-board-form
                #:make-edit-board-form-widget)
  (:import-from #:reblocks/request
                #:get-path)
  (:import-from #:serapeum
                #:fmt))
(in-package #:app/pages/del-board)


(defwidget del-board-page ()
  ())


(defun make-del-board-page ()
  (make-instance 'del-board-page))


(defmethod render ((widget del-board-page))
  (cl-ppcre:register-groups-bind (board-id)
      ("/boards/(\\d+)/del" (get-path))
    (let* ((board (get-board board-id)))
      (cond
        (board
         (let ((form (make-del-board-widget board)))
           (title (fmt "Удаление онбординга \"~A\""
                       (board-title board)))
           (render form)))
        (t
         (title "Онбординг не найден"))))))
