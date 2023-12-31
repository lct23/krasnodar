(uiop:define-package #:app/widgets/new-document-form
  (:use #:cl)
  (:import-from #:reblocks/widget
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
(in-package #:app/widgets/new-document-form)


(defwidget new-document-form-widget (event-emitter widget)
  ((selected-department-id :initform nil
                           :type (or null integer)
                           :accessor selected-department-id)))


(defun make-new-document-form-widget ()
  (make-instance 'new-document-form-widget))


;; How to convert a document
;; pandoc -f docx -t markdown -o FAQ.md FAQ.docx

(defmethod render ((widget new-document-form-widget))
  (flet ((add-document (&rest rest &key title text department-id &allow-other-keys)
           (log:info "Adding document with" title rest)
           (let* ((department (when (and department-id
                                         (not (string= department-id "")))
                                (get-department department-id)))
                  (document
                    (create-dao 'document
                                :title title
                                :text text
                                :department department)))
             (setf (selected-department-id widget)
                   (when department
                     (object-id department)))
             (event-emitter:emit :object-created widget
                                 document))))
    (with-html-form (:post #'add-document
                     :class "my-6")
      (department-select-box "department-id"
                             :allow-empty t
                             :label "Отдел"
                             :selected-department-id (selected-department-id widget))
      (text-input "title" :placeholder "Название документа")
      (text-area "text" :placeholder "Текст документа.")
      (submit-button))))
