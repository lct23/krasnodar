(uiop:define-package #:app/widgets/document-list
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
                #:get-documents
                #:document-department
                #:document-title)
  (:import-from #:app/models/department
                #:department-title))
(in-package #:app/widgets/document-list)


(defun make-document-list-widget ()
  (make-table
   (list
    (column "Отдел" :getter (lambda (obj)
                              (let ((dep (document-department obj)))
                                (if dep
                                    (department-title dep)
                                    ""))))
    (column "Название" :getter #'document-title))
   (get-documents-for-list-widget)))
