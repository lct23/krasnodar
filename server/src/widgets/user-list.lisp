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
                #:user-mentor
                #:user-is-mentor-p
                #:user-is-boss-p
                #:user-department
                #:user-name
                #:user-roles)
  (:import-from #:reblocks-auth/models
                #:get-current-user
                #:get-all-users)
  (:import-from #:app/models/department
                #:department-title
                #:deparment-title)
  (:import-from #:reblocks-ui2/tables/editable-table)
  (:import-from #:app/widgets/add-user-form
                #:make-add-user-form-widget)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:app/widgets/utils
                #:redirect-button)
  (:import-from #:mito
                #:object-id)
  (:import-from #:app/models/roles
                #:hr-p))
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
   :object-creator (make-add-user-form-widget)
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


(defun render-user (user &key show-controls)
  (let ((cell-classes "w-full lg:w-auto p-3 text-gray-800 text-center border border-b block lg:table-cell relative lg:static")
        (span-classes "lg:hidden absolute top-0 left-0 bg-blue-200 px-2 py-1 text-xs font-bold uppercase"))
    (with-html
      (:tr :class "bg-white lg:hover:bg-gray-100 flex lg:table-row flex-row lg:flex-row flex-wrap lg:flex-no-wrap mb-10 lg:mb-0"
           (:td :class cell-classes
                (:span :class span-classes
                       "Фото")
                (let ((url (app/models/user::user-avatar-url user))
                      (classes "w-20 h-20"))
                  (if (or (null url)
                          (string= url ""))
                      (:img :class classes
                            :src "https://placekitten.com/300/300")
                      (:img :class classes
                            :src url))))
           (:td :class cell-classes
                (:span :class span-classes
                       "Имя")
                (user-name user))
           (:td :class cell-classes
                (:span :class span-classes
                       "Отдел")
                (let ((is-boss (user-is-boss-p user))
                      (title (department-title
                              (user-department user))))
                  (if is-boss
                      (fmt "~A (начальник)" title)
                      title)))
           (:td :class cell-classes
                (:span :class span-classes
                       "Должность")
                (app/models/user::user-position user))
           (:td :class cell-classes
                (:span :class span-classes
                       "Ментор")
                (cond
                  ((user-is-mentor-p user)
                   "Да")
                  ;; Выведем имя ментора
                  ((user-mentor user)
                   (user-name
                    (user-mentor user)))
                  (t
                   "")))
           (when show-controls
             (:td :class cell-classes
                  (:span :class span-classes)
                  (redirect-button "Открыть"
                                   (fmt "/personal/~A"
                                        (object-id user)))
                  (redirect-button "Редактировать"
                                   (fmt "/personal/~A/edit"
                                        (object-id user)))))))))


(defmethod render ((widget user-list-widget))
  (let ((header-classes
          "p-3 font-bold uppercase bg-gray-200 text-gray-600 border border-gray-300 hidden lg:table-cell")
        (show-controls
          (and (get-current-user)
               (hr-p (get-current-user)))))
    (with-html
      (:table :class "border-collapse w-full"
              (:thead
               (:tr (:th :class header-classes "Фото")
                    (:th :class header-classes "Имя")
                    (:th :class header-classes "Отдел")
                    (:th :class header-classes "Должность")
                    (:th :class header-classes "Ментор")
                    (when show-controls
                      (:th :class header-classes "Действия"))))
              (:tbody
               (loop for user in (get-all-users)
                     do (render-user user :show-controls show-controls)))))))
