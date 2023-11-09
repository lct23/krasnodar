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
                #:fmt)
  (:import-from #:mito
                #:object-id)
  (:import-from #:app/widgets/redirect-button
                #:redirect-button))
(in-package #:app/widgets/knowledge-list)


(defun make-knowledge-list-widget ()
  (make-table
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
                                       title))))
    (column "Действия" :getter (lambda (obj)
                                 (redirect-button "Редактировать"
                                                  (fmt "/kb/~A/edit"
                                                       (object-id obj))))))
   (get-knowledges)))
