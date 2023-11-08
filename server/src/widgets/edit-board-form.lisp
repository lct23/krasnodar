(uiop:define-package #:app/widgets/edit-board-form
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/models/board
                #:board-department
                #:board-title
                #:period-title
                #:board-periods
                #:board)
  (:import-from #:reblocks-ui/form
                #:with-html-form)
  (:import-from #:app/widgets/utils
                #:submit-button
                #:text-input
                #:department-select-box)
  (:import-from #:mito
                #:object-id)
  (:import-from #:app/models/department
                #:get-department)
  (:import-from #:str
                #:emptyp)
  (:import-from #:app/widgets/period-edit-form
                #:make-period-edit-form-widget))
(in-package #:app/widgets/edit-board-form)


(defwidget edit-board-form-widget ()
  ((board :type (or null board)
          :initform nil
          :initarg :board
          :accessor board)))


(defun make-edit-board-form-widget (board)
  (make-instance 'edit-board-form-widget
                 :board board))


(defmethod render ((widget edit-board-form-widget))
  (let* ((board (board widget))
         (department (board-department board))
         (periods (board-periods board)))

    (flet ((edit-board (&key title department-id &allow-other-keys)
             (log:info "Editing board, new" title)
             (setf (board-title board) title
                   (board-department board) (unless (emptyp department-id)
                                              (get-department department-id)))
             (mito:save-dao board)))
      
      (with-html-form (:post #'edit-board)
        (text-input "title"
                    :label "Название онбординга"
                    :value (board-title board))
       
        (department-select-box "department-id"
                               :label "Отдел"
                               :allow-empty t
                               :selected-department-id (when department
                                                         (object-id department)))
        (submit-button :text "Сохранить")))
      
    (with-html
      (loop for period in periods
            do (render (make-period-edit-form-widget period))))))

