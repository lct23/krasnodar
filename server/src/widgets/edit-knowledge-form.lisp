(uiop:define-package #:app/widgets/edit-knowledge-form
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:models/app/knowledge
                #:knowledge-questionnaire
                #:knowledge-document
                #:knowledge))
(in-package #:app/widgets/edit-knowledge-form)


(defwidget edit-knowledge-form-widget ()
  ((knowledge :type knowledge
              :initarg :knowledge
              :reader knowledge)))


(defun make-edit-knowledge-form-widget (knowledge)
  (make-instance 'edit-knowledge-form-widget
                 :knowledge knowledge))


(defmethod render ((widget edit-knowledge-form-widget))
  (let* ((knowledge (knowledge widget))
         (document (knowledge-document knowledge))
         (questionnaire (knowledge-questionnaire knowledge)))
    (with-html
      (when document
        (:h1 :class "font-xl"
             "Документ")
        (render (app/widgets/edit-document-form::make-edit-document-form-widget document)))

      (when questionnaire
        (:h1 :class "font-xl"
             "Опрос")
        (render (app/widgets/questionnairies::make-questionnaire-widget questionnaire))))))

