(uiop:define-package #:app/widgets/document
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/models/document
                #:document-text
                #:document-title
                #:get-document)
  (:import-from #:3bmd))
(in-package #:app/widgets/document)


(defwidget document-widget ()
  ((id :initarg :id
       :type integer
       :reader document-id)))


(defun make-document-widget (document-id)
  (make-instance 'document-widget
                 :id document-id))


(defmethod render ((widget document-widget))
  (with-html
    (let* ((document (get-document (document-id widget)))
           (markdown (document-text document)))
      (:h1 (document-title document))
      (:section
       ;; TODO: Здесь надо встроить проверку документа по white-list,
       ;; чтобы предотвратить XSS уязвимость в markdown документах.
       (3bmd:parse-string-and-print-to-stream markdown
                                              reblocks/html:*stream*)))))


(defmethod get-dependencies ((widget document-widget))
  (list*
   (reblocks-lass:make-dependency
     `(.document-widget
       (h1 :font-size 2rem
           :margin-top 2rem)
       (h2 :font-size 1.5rem
           :margin-top 1.5rem)
       (h3 :font-size 1.2rem
           :margin-top 1.2rem)))
   (call-next-method)))
