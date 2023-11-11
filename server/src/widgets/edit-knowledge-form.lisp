(uiop:define-package #:app/widgets/edit-knowledge-form
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/models/knowledge
                #:knowledge-game
                #:knowledge-questionnaire
                #:knowledge-document
                #:knowledge)
  (:import-from #:app/widgets/edit-document-form
                #:make-edit-document-form-widget)
  (:import-from #:app/widgets/questionnairies
                #:make-questionnaire-widget)
  (:import-from #:app/models/roles
                #:hr-p)
  (:import-from #:reblocks-auth/models
                #:get-current-user)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:app/models/game
                #:game-title))
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
         (questionnaire (knowledge-questionnaire knowledge))
         (game (knowledge-game knowledge))
         (user (get-current-user))
         (is-hr (hr-p user)))
    
    (unless is-hr
      (reblocks/response:redirect "/"))
    
    (with-html
      (when document
        (:h1 :class "text-3xl"
             "Документ")
        (render (make-edit-document-form-widget document)))

      (cond
        (game
          (:div :class "my-8"
                (:p :class "text-lg font-bold text-gray-700"
                    (fmt "К знанию привязана игра: \"~A\""
                         (game-title game)))
                (:p "Настройка игр пока не поддержана в редакторе.")))
        (questionnaire
          (:div :class "flex flex-col gap-4"
                (:h1 :class "text-3xl"
                     "Опрос")
                (render (make-questionnaire-widget questionnaire))))))))

