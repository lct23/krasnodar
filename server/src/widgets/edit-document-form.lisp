(uiop:define-package #:app/widgets/edit-document-form
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:update
                #:widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks-ui/form
                #:with-html-form)
  (:import-from #:app/widgets/utils
                #:text-area
                #:department-select-box
                #:text-input
                #:submit-button)
  (:import-from #:app/models/document
                #:document-department
                #:document-text
                #:document-title
                #:document)
  (:import-from #:app/models/department
                #:get-department)
  (:import-from #:event-emitter
                #:event-emitter)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:mito
                #:create-dao
                #:object-id))
(in-package #:app/widgets/edit-document-form)


(defwidget edit-document-form-widget (event-emitter widget)
  ((document :type document
             :initarg :document
             :accessor form-document)))


(defun make-edit-document-form-widget (document)
  (make-instance 'edit-document-form-widget
                 :document document))


(defmethod render ((widget edit-document-form-widget))
  (flet ((save-document (&rest rest &key title text department-id &allow-other-keys)
           (log:info "Editing document with" title rest)
           (let* ((department (when (and department-id
                                         (not (string= department-id "")))
                                (get-department department-id)))
                  (document (form-document widget)))
             (setf (document-title document)
                   title
                   (document-text document)
                   text
                   (document-department document)
                   department)
             (mito:save-dao document)
             (event-emitter:emit :object-updated widget
                                 document)
             (update widget))))
    (let ((document (form-document widget))
          (change-handler '(:|x-on:keyup| "$refs.button.disabled = false")))
      (with-html-form (:post #'save-document
                       :class "my-6")
        (department-select-box "department-id"
                               :allow-empty t
                               :label "Отдел"
                               :selected-department-id (object-id document))
        (:div :attrs '(:x-data "{}")
              (text-input "title"
                          :placeholder "Название документа"
                          :value (document-title document)
                          :attrs change-handler)
              (text-area "text" :placeholder "Текст документа в формате Markdown"
                                :label "Текст документа"
                                :value (document-text document)
                                :attrs change-handler)
              (submit-button :text "Сохранить"
                             :disabled t
                             :attrs '(:x-ref "button")))))))
