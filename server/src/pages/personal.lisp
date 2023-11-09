(uiop:define-package #:app/pages/personal
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/widgets/user-list
                #:make-user-list-widget)
  (:import-from #:app/widgets/utils
                #:redirect-button)
  (:import-from #:app/pages/utils
                #:title)
  (:import-from #:app/models/roles
                #:hr-p)
  (:import-from #:reblocks-auth/models
                #:get-current-user))
(in-package #:app/pages/personal)


(defwidget personal-widget ()
  ())


(defun make-personal-widget ()
  (make-instance 'personal-widget))


(defmethod render ((widget personal-widget))
  (title "Сотрудники")
  
  (with-html
    (:div :class "flex flex-col gap-8"
          (render (make-user-list-widget))
         
          (when (hr-p (get-current-user))
            (:div :class "flex justify-end"
                  (redirect-button "Добавить сотрудника"
                                   "/personal/add"))))))

