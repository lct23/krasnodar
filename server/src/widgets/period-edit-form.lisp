(uiop:define-package #:app/widgets/period-edit-form
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/models/board
                #:remove-knowledge-from-period
                #:period-knowledges
                #:period-knowledge
                #:period-title
                #:board-period)
  (:import-from #:app/widgets/add-knowledge-to-period-form
                #:make-add-knowledge-to-period-form-widget)
  (:import-from #:serapeum
                #:soft-list-of)
  (:import-from #:app/models/knowledge
                #:knownledge-title)
  (:import-from #:event-emitter
                #:event-emitter)
  (:import-from #:reblocks-ui2/widget
                #:ui-widget)
  (:import-from #:app/widgets/utils
                #:add-deletion-callbacks
                #:add-to-the-end)
  (:import-from #:app/widgets/small-button
                #:small-and-round-delete-button
                #:small-button))
(in-package #:app/widgets/period-edit-form)


(defwidget period-edit-form-widget (event-emitter ui-widget)
  ((period :initarg :period
           :type board-period
           :accessor period)))


(defwidget editable-period-knowledge-widget (event-emitter ui-widget)
  ((period-knowledge :initarg :period-knowledge
                     :type period-knowledge
                     :accessor period-knowledge)))


(defwidget editable-period-knowledges-list-widget (event-emitter ui-widget)
  ((period :initarg :period
           :type board-period
           :accessor period)
   (knowledges :initarg :knowledges
               :type (soft-list-of editable-period-knowledge-widget)
               :accessor knowledges)))


(defun make-editable-period-knowledge-widget (period-knowledge)
  (make-instance 'editable-period-knowledge-widget
                 :period-knowledge period-knowledge))


(defun make-editable-period-knowledges-list-widget (period)
  (let ((widget (make-instance 'editable-period-knowledges-list-widget
                               :period period
                               :knowledges (mapcar #'make-editable-period-knowledge-widget
                                                   (period-knowledges period)))))
    (add-deletion-callbacks (knowledges widget))
    widget))


(defun make-period-edit-form-widget (period)
  (make-instance 'period-edit-form-widget
                 :period period))


(defmethod render ((widget period-edit-form-widget))
  (with-html
    (let ((period (period widget)))
      (:h1 :class "text-xl font-bold"
           (period-title period))
      (let ((list (make-editable-period-knowledges-list-widget period))
            (form (make-add-knowledge-to-period-form-widget (period widget))))
        (flet ((add-list-item (period-knowledge)
                 (add-to-the-end widget
                                 (knowledges list)
                                 (make-editable-period-knowledge-widget period-knowledge))))
          (event-emitter:on :object-created form
                            #'add-list-item))
        (render list)
        (render form)))))


(defmethod render ((widget editable-period-knowledges-list-widget))
  (mapc #'render (knowledges widget)))


(defmethod render ((widget editable-period-knowledge-widget))
  (with-html
    (flet ((remove-item (&rest rest)
             (declare (ignore rest))
             (remove-knowledge-from-period (period-knowledge widget))
             (event-emitter:emit :delete widget
                                 widget)))
      (let ((knowledge (app/models/board::knowledge (period-knowledge widget))))
        (:div :class "inline-flex"
              (knownledge-title knowledge)
              (render
               (small-and-round-delete-button :on-click #'remove-item)))))))


(defmethod reblocks/widget:get-html-tag ((widget editable-period-knowledges-list-widget))
  :ul)

(defmethod reblocks/widget:get-css-classes ((widget editable-period-knowledges-list-widget))
  (list* "list-disc list-inside"
         (call-next-method)))

(defmethod reblocks/widget:get-html-tag ((widget editable-period-knowledge-widget))
  :li)

(defmethod reblocks/widget:get-css-classes ((widget editable-period-knowledge-widget))
  (call-next-method))

(defmethod reblocks/widget:get-css-classes ((widget period-edit-form-widget))
  (list* "w-2/3 bg-white text-gray-400 hover:text-gray-900 shadow-lg rounded-lg p-8 mb-10 lg:mb-0 flex flex-col hover:shadow-xl hover:scale-105 max-w-6xl mx-auto gap-4"
         (call-next-method)))
