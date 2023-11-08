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
                #:period-knowledges
                #:period-knowledge
                #:period-title
                #:board-period)
  (:import-from #:app/widgets/add-knowledge-to-period-form
                #:make-add-knowledge-to-period-form-widget)
  (:import-from #:serapeum
                #:soft-list-of)
  (:import-from #:models/app/knowledge
                #:knownledge-title))
(in-package #:app/widgets/period-edit-form)


(defwidget period-edit-form-widget ()
  ((period :initarg :period
           :type board-period
           :accessor period)))


(defwidget editable-period-knowledge-widget ()
  ((period-knowledge :initarg :period-knowledge
                     :type period-knowledge
                     :accessor period-knowledge)))


(defwidget editable-period-knowledges-list-widget ()
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
  (make-instance 'editable-period-knowledges-list-widget
                 :period period
                 :knowledges (mapcar #'make-editable-period-knowledge-widget
                                     (period-knowledges period))))


(defun make-period-edit-form-widget (period)
  (make-instance 'period-edit-form-widget
                 :period period))


(defmethod render ((widget period-edit-form-widget))
  (with-html
    (let ((period (period widget)))
      (:h1 (period-title period))
      (let ((list (make-editable-period-knowledges-list-widget period))
            (form (make-add-knowledge-to-period-form-widget (period widget))))
        (render list)
        (render form)))))


(defmethod render ((widget editable-period-knowledges-list-widget))
  (mapc #'render (knowledges widget)))


(defmethod render ((widget editable-period-knowledge-widget))
  (with-html
    (let ((knowledge (app/models/board::knowledge (period-knowledge widget))))
      (:p (knownledge-title knowledge)))))

