(uiop:define-package #:app/widgets/add-knowledge-to-period-form
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/models/board
                #:board-period)
  (:import-from #:reblocks-ui/form
                #:with-html-form)
  (:import-from #:app/widgets/utils
                #:submit-button
                #:knowledge-select-box)
  (:import-from #:event-emitter
                #:event-emitter)
  (:import-from #:reblocks-ui2/widget
                #:ui-widget))
(in-package #:app/widgets/add-knowledge-to-period-form)


(defwidget add-knowledge-to-period-form-widget (event-emitter ui-widget)
  ((period :type board-period
           :initarg :period
           :reader period)))


(defun make-add-knowledge-to-period-form-widget (period)
  (make-instance 'add-knowledge-to-period-form-widget
                 :period period))


(defmethod render ((widget add-knowledge-to-period-form-widget))
  (flet ((add-knowledge (&key knowledge-id &allow-other-keys)
           (log:error "Adding knowledge" knowledge-id)
           (let ((binding (app/models/board::bind-knowledge-to-period (period widget)
                                                                      (models/app/knowledge::get-knowledge knowledge-id))))
             (event-emitter:emit :object-created widget
                                 binding))))
    (with-html-form (:post #'add-knowledge)
      (knowledge-select-box "knowledge-id"
                            :label "Знание")
      (submit-button :text "Добавить знание"))))

