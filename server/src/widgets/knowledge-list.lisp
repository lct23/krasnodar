(uiop:define-package #:app/widgets/knowledge-list
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
  (:import-from #:app/models/document
                #:get-documents-for-list-widget
                #:document-department
                #:document-title)
  (:import-from #:app/models/department
                #:department-title)
  (:import-from #:app/models/knowledge
                #:knowledge-document
                #:get-knowledges
                #:knowledge-department)
  (:import-from #:serapeum
                #:push-end
                #:fmt)
  (:import-from #:mito
                #:object-id)
  (:import-from #:app/widgets/redirect-button
                #:redirect-button)
  (:import-from #:app/widgets/utils
                #:*dangerous-button-classes*)
  (:import-from #:reblocks-ui2/containers/row
                #:make-row-widget)
  (:import-from #:reblocks-auth/models
                #:get-current-user)
  (:import-from #:app/models/roles
                #:hr-p))
(in-package #:app/widgets/knowledge-list)


(defun make-knowledge-list-widget ()
  (let* ((user (get-current-user))
         (is-hr (hr-p user))
         (columns
           (list
            (column "Отдел" :getter (lambda (obj)
                                      (let ((dep (knowledge-department obj)))
                                        (if dep
                                            (department-title dep)
                                            "Для всех"))))
            (column "Название" :getter (lambda (obj)
                                         (let ((title (document-title
                                                       (knowledge-document obj))))
                                           (if (str:emptyp title)
                                               "Без названия"
                                               title)))))))

    ;; Действия доступны только HR
    (when is-hr
      (push-end
       (column "Действия" :getter (lambda (obj)
                                    (make-row-widget
                                     (list
                                      (redirect-button "Редактировать"
                                                       (fmt "/kb/~A/edit"
                                                            (object-id obj)))
                                      (redirect-button "Удалить"
                                                       (fmt "/kb/~A/del"
                                                            (object-id obj))
                                                       :class *dangerous-button-classes*))
                                     :classes "gap-4")))
       columns))
    (make-table
     columns
     (get-knowledges))))
