(uiop:define-package #:app/pages/dashboard/common
  (:use #:cl)
  (:import-from #:reblocks-auth/models
                #:get-current-user)
  (:import-from #:app/models/roles
                #:hr-p)
  (:import-from #:app/pages/dashboard/hr
                #:make-hr-dashboard-widget)
  (:import-from #:app/pages/landing
                #:make-landing-page)
  (:import-from #:app/pages/user
                #:make-user-dashboard-page)
  (:import-from #:app/widgets/frame
                #:make-page-frame))
(in-package #:app/pages/dashboard/common)


(defun make-dashboard-page ()
  "Строит страницу дашборда в зависимости от роли сотрудника."
  (let ((user (get-current-user)))
    (cond
      ((hr-p user)
       (make-page-frame
        (make-hr-dashboard-widget)))
      (user
       (make-page-frame
        (make-user-dashboard-page)))
      (t
       (make-landing-page)))))
