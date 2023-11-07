(uiop:define-package #:app/widgets/add-board-form
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks-ui2/buttons/button
                #:button)
  (:import-from #:mito
                #:object-id)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:reblocks/response
                #:redirect)
  (:import-from #:app/models/board
                #:create-board)
  (:import-from #:app/widgets/utils
                #:*button-classes*))
(in-package #:app/widgets/add-board-form)


(defwidget add-board-form-widget ()
  ()
  (:documentation "Создаёт новый флоу онбординга и перекидывает на страницу редактирования."))


(defun make-add-board-form-widget ()
  (make-instance 'add-board-form-widget))


(defmethod render ((widget add-board-form-widget))
  (render
   (button "Добавить онбординг"
           :on-click (lambda (&rest rest)
                       (declare (ignore rest))
                       (let ((board (create-board)))
                         (redirect (fmt "/boards/~A"
                                        (object-id board)))))
           :class *button-classes*)))

