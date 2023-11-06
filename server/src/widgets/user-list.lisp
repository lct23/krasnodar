(uiop:define-package #:app/widgets/user-list
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/models/user
                #:user-roles)
  (:import-from #:reblocks-auth/models
                #:get-all-users))
(in-package #:app/widgets/user-list)


(defwidget user-list-widget2 (reblocks-ui2/tables/editable-table:editable-table-widget)
  ())



(defun make-user-list-widget2 ()
  (reblocks-ui2/tables/editable-table:make-editable-table
   (list (reblocks-ui2/tables/table:column "Email"
                                           :getter #'reblocks-auth/models::get-email)
         (reblocks-ui2/tables/table:column "Роли"
                                           :getter (lambda (user)
                                                     (format nil "~{~A~^ ~}"
                                                             (coerce (user-roles user)
                                                                     'list))))
         (reblocks-ui2/tables/table:column "Действия"
                                           :getter (lambda (user)
                                                     (declare (ignore user))
                                                     "")
                                           :align :right))
   (reblocks-auth/models:get-all-users)
   :object-creator (app/widgets/add-user-form::make-add-user-form-widget)
   ;; (lambda (table-widget)
   ;;   (declare (ignore table-widget))
   ;;   (let ((email "foo@svetlyak.ru"))
   ;;     (reblocks-auth/models:create-social-user :email
   ;;                                              email
   ;;                                              :email email)))
   ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; OLD CODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwidget user-list-widget ()
  ())


(defun make-user-list-widget ()
  (make-instance 'user-list-widget))


(defun render-user (user)
  (let ((cell-classes "w-full lg:w-auto p-3 text-gray-800 text-center border border-b block lg:table-cell relative lg:static"))
    (with-html
      (:tr :class "bg-white lg:hover:bg-gray-100 flex lg:table-row flex-row lg:flex-row flex-wrap lg:flex-no-wrap mb-10 lg:mb-0"
           (:td :class cell-classes
                (:span :class "lg:hidden absolute top-0 left-0 bg-blue-200 px-2 py-1 text-xs font-bold uppercase"
                       "Email")
                (reblocks-auth/models:get-email user))
           (:td :class cell-classes
                (:span :class "lg:hidden absolute top-0 left-0 bg-blue-200 px-2 py-1 text-xs font-bold uppercase" "Роли")
                (with-output-to-string (s)
                  (loop for role across (app/models/user::user-roles user)
                        do (format s "~A " role))))
           (:td :class cell-classes
                (:span :class "lg:hidden absolute top-0 left-0 bg-blue-200 px-2 py-1 text-xs font-bold uppercase" "Действия")
                "")))))


(defmethod render ((widget user-list-widget))
  (let ((header-classes
          "p-3 font-bold uppercase bg-gray-200 text-gray-600 border border-gray-300 hidden lg:table-cell"))
    (with-html
      (:table :class "border-collapse w-full"
              (:thead
               (:tr (:th :class header-classes "Email")
                    (:th :class header-classes "Роли")
                    (:th :class header-classes "Действия")))
              (:tbody
               (loop for user in (get-all-users)
                     do (render-user user)))))))


;; (defmethod get-dependencies ((widget user-list-widget))
;;   (list*
;;    (reblocks-lass:make-dependency
;;      `(.user-list-widget
;;        :color red))
;;    (call-next-method)))

