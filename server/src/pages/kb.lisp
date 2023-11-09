(uiop:define-package #:app/pages/kb
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks-ui2/tables/table
                #:append-data)
  (:import-from #:app/pages/utils
                #:title)
  (:import-from #:app/models/roles
                #:hr-p)
  (:import-from #:reblocks-auth/models
                #:get-current-user)
  (:import-from #:app/widgets/knowledge-list
                #:make-knowledge-list-widget)
  (:import-from #:app/widgets/add-knowledge-form
                #:make-add-knowledge-form-widget))
(in-package #:app/pages/kb)


(defwidget kb-widget ()
  ())


(defun make-kb-widget ()
  (make-instance 'kb-widget))


;; (defmethod render ((widget kb-widget))
;;   (title "База знаний")
  
;;   (let ((form (app/widgets/new-document-form::make-new-document-form-widget))
;;         (list (app/widgets/document-list::make-document-list-widget)))
;;     (event-emitter:on :object-created form
;;                       (lambda (obj)
;;                         (append-data list (list obj))
;;                         (reblocks/widget:update list)
;;                         (reblocks/widget:update form)))
;;     (render list)
;;     (when (hr-p (get-current-user))
;;       (render form))))

(defmethod render ((widget kb-widget))
  (title "База знаний")
  
  (let ((form (make-add-knowledge-form-widget))
        (list (make-knowledge-list-widget)))
    (event-emitter:on :object-created form
                      (lambda (obj)
                        (append-data list (list obj))
                        (reblocks/widget:update list)
                        (reblocks/widget:update form)))
    (with-html
      (:div :class "flex flex-col gap-8"
            (render list)
            (when (hr-p (get-current-user))
              (:div :class "flex justify-center"
                    (render form)))))))

