(uiop:define-package #:app/widgets/del-board
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
  (:import-from #:app/models/board
                #:board
                #:delete-board
                #:knownledge-title)
  (:import-from #:app/widgets/utils
                #:*dangerous-button-classes*
                #:submit-button)
  (:import-from #:reblocks/response
                #:redirect))
(in-package #:app/widgets/del-board)


(defwidget del-board-widget ()
  ((board :initarg :board
              :type board
              :reader board)))


(defun make-del-board-widget (board)
  (make-instance 'del-board-widget
                 :board board))


(defmethod render ((widget del-board-widget))
  (flet ((on-delete (&rest rest)
           (declare (ignore rest))
           (delete-board (board widget))
           (redirect "/boards"))
         (on-cancel (&rest rest)
           (declare (ignore rest))
           (redirect "/boards")))
    
    (with-html
      (:div :class "flex flex-col items-center gap-4"
            (:div :class "text-xl"
                  "Вы точно хотите удалить это этот онбординг?")
            (:div :class "flex gap-4"
                  (with-html-form (:post #'on-delete)
                    (submit-button :text "Да"
                                   :classes *dangerous-button-classes*))
                  (with-html-form (:post #'on-cancel)
                    (submit-button :text "Нет")))))))
