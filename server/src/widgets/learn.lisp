(uiop:define-package #:app/widgets/learn
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/models/board
                #:period-knowledge)
  (:import-from #:app/widgets/document
                #:make-document-widget)
  (:import-from #:app/models/knowledge
                #:knowledge-document)
  (:import-from #:mito
                #:object-id)
  (:import-from #:app/models/board-progress
                #:questionnaire-results
                #:period-knowledge-progress)
  (:import-from #:app/widgets/questionnaire
                #:make-questionnaire-widget))
(in-package #:app/widgets/learn)


(defwidget learn-widget ()
  ((period-knowledge-progress :initarg :period-knowledge-progress
                              :type period-knowledge-progress
                              :reader period-knowledge-progress)))


(defun make-learn-widget (period-knowledge-progress)
  (make-instance 'learn-widget
                 :period-knowledge-progress period-knowledge-progress))


(defmethod render ((widget learn-widget))
  (with-html
    (let* ((progress (period-knowledge-progress widget))
           (period-knowledge (period-knowledge progress))
           (questionnaire-results (questionnaire-results progress))
           (knowledge (app/models/board::knowledge period-knowledge))
           (document (knowledge-document knowledge)))
      (render (make-document-widget (object-id document)))

      (:h1 :class "mt-8 mb-4 font-bold text-xl"
           "Проверка знаний")

      (render (make-questionnaire-widget questionnaire-results)))))

