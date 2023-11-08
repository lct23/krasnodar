(uiop:define-package #:app/models/user
  (:use #:cl)
  (:import-from #:app/models/department
                #:department)
  (:import-from #:mito
                #:select-by-sql))
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
     (can-be-mentor :col-type :boolean
                    :initform nil
                    :initarg :can-be-mentor
                    :accessor user-is-mentor-p
                    :documentation "Если True, значит чел руководитель может быть назначен в качестве ментора.")
     (mentor :col-type user
             :initform nil
             :initarg :mentor
             :accessor user-mentor
             :documentation "Наставник, который менторит данного сотрудника.")
     (boss :col-type :boolean
           :initform nil
           :initarg :boss
           :accessor user-is-boss-p
           :documentation "Если True, значит чел руководитель в своём отделе. В реале, надо у отдела поле boss иметь, но тогда один человек может быть руководителем нескольких отделов. Пока для простоты сделаем это признаком на записи сотрудника.")
     (department :col-type department
                 :initarg :department
                 :accessor user-department)
     (position :col-type :text
               :initform ""
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


(defun get-all-mentors ()
  (select-by-sql 'user
                 "SELECT *
                    FROM \"user\"
                   WHERE can_be_mentor"))

(defun get-user (id)
  (when (and id
             (not (string= id "")))
    (mito:find-dao 'user
                   :id id)))
