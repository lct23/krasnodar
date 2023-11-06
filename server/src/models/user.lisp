(uiop:define-package #:app/models/user
  (:use #:cl))
(in-package #:app/models/user)


(progn
  (defclass user (reblocks-auth/models:user)
    ;; Тут может быть одна из 4 ролей:
    ;; - hr
    ;; - employee
    ;; - boss
    ;; - mentor
    ((roles :col-type "text[]"
            :initform "{}"
            :initarg :roles
            :reader user-roles))
    (:metaclass mito:dao-table-class))

  (setf reblocks-auth/models::*user-class*
        'user)

  ;; (eval-when (:compile-toplevel :load-toplevel :execute))
  (mito.dao.mixin::make-relational-reader-method
   'reblocks-auth/models:profile-user
   (find-class 'reblocks-auth/models:social-profile)
   'reblocks-auth/models:user
   (find-class 'user)))
