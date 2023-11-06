(uiop:define-package #:app/models/user
  (:use #:cl)
  (:import-from #:app/models/department
                #:department))
(in-package #:app/models/user)


(progn
  (defclass user (reblocks-auth/models:user)
    ;; Тут может быть одна из 4 ролей:
    ;; - hr
    ;; - employee
    ;; - boss
    ;; - mentor
    ((name :col-type :text
           :initarg :name
           :accessor user-name
           :documentation "Фамилия и Имя сотрудника")
     (avatar-url :col-type (or :null :text)
                 :initarg :avatar-url
                 :accessor user-avatar-url)
     (roles :col-type "text[]"
            :initform "{}"
            :initarg :roles
            :accessor user-roles)
     (department :col-type department
                 :initarg :department
                 :accessor user-department)
     (position :col-type :text
               :initform "employee"
               :initarg :position
               :accessor user-position
               :documentation "Должность: boss для руководителя отдела или employee для обычного работника."))
    (:metaclass mito:dao-table-class))

  (setf reblocks-auth/models::*user-class*
        'user)

  ;; (eval-when (:compile-toplevel :load-toplevel :execute))
  (mito.dao.mixin::make-relational-reader-method
   'reblocks-auth/models:profile-user
   (find-class 'reblocks-auth/models:social-profile)
   'reblocks-auth/models:user
   (find-class 'user)))
