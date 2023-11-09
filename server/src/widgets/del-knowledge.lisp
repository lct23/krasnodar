(uiop:define-package #:app/widgets/del-knowledge
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks-ui/form
                #:with-html-form)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:app/models/knowledge
                #:knowledge
                #:delete-knowledge
                #:knownledge-title)
  (:import-from #:app/widgets/utils
                #:*dangerous-button-classes*
                #:submit-button)
  (:import-from #:reblocks/response
                #:redirect))
(in-package #:app/widgets/del-knowledge)


(defwidget del-knowledge-widget ()
  ((knowledge :initarg :knowledge
              :type knowledge
              :reader knowledge)))


(defun make-del-knowledge-widget (knowledge)
  (make-instance 'del-knowledge-widget
                 :knowledge knowledge))


(defmethod render ((widget del-knowledge-widget))
  (flet ((on-delete (&rest rest)
           (declare (ignore rest))
           (delete-knowledge (knowledge widget))
           (redirect "/kb"))
         (on-cancel (&rest rest)
           (declare (ignore rest))
           (redirect "/kb")))
    
    (with-html
      (:div :class "flex flex-col items-center gap-4"
            (:div :class "text-xl"
                  "Вы точно хотите удалить это знание?")
            (:div :class "flex gap-4"
                  (with-html-form (:post #'on-delete)
                    (submit-button :text "Да"
                                   :classes *dangerous-button-classes*))
                  (with-html-form (:post #'on-cancel)
                    (submit-button :text "Нет")))))))
