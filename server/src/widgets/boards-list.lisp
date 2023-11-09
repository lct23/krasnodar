(uiop:define-package #:app/widgets/boards-list
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks-ui2/tables/table
                #:column
                #:make-table)
  (:import-from #:app/models/board
                #:board-department
                #:board-title
                #:get-boards)
  (:import-from #:app/widgets/redirect-button
                #:redirect-button)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:mito
                #:object-id)
  (:import-from #:app/models/department
                #:department-title))
(in-package #:app/widgets/boards-list)


(defwidget boards-list-widget ()
  ())


(defun make-boards-list-widget ()
  (make-instance 'boards-list-widget))


(defmethod render ((widget boards-list-widget))
  (render
   (make-table
    (list (column "Название"
                  :getter #'board-title)
          (column "Отдел"
                  :getter (lambda (board)
                            (let ((department (board-department board)))
                              (if department
                                  (department-title department)
                                  "Для всех отделов"))))
          (column "Действия"
                  :getter (lambda (board)
                            (redirect-button "Редактировать"
                                             (fmt "/boards/~A/edit"
                                                  (object-id board))))))
    (get-boards))))

