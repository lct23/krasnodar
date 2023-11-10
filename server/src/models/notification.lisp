(uiop:define-package #:app/models/notification
  (:use #:cl)
  (:import-from #:app/models/user
                #:user)
  (:import-from #:mito
                #:object-id
                #:create-dao
                #:select-dao)
  (:import-from #:sxql
                #:where
                #:order-by))
(in-package #:app/models/notification)


(defclass notification ()
  ((user :col-type user
         :initarg :user
         :reader notification-user)
   (title :col-type :text
          :initform ""
          :initarg :title
          :reader notification-title)
   (text :col-type :text
         :initform ""
         :initarg :text
         :reader notification-text
         :documentation "Текст уведомления в Markdown формате.")
   (critical :col-type :boolean
             :initform nil
             :initarg :critical
             :reader critical-notification-p
             :documentation "Показывает что уведомление говорит о просроченных заданиях.")
   (seen-at :col-type (or :null :timestamptz)
            :initform nil
            :accessor notification-seen-at))
  (:metaclass mito:dao-table-class))


(defun notification-is-new-p (obj)
  (check-type obj notification)
  (null (notification-seen-at obj)))


(defun make-notification (user title text &key critical)
  (create-dao 'notification
              :user user
              :title title
              :text text
              :critical (when critical
                          t)))


(defun get-user-notifications (user)
  "Самые свежие уведомления идут первыми."
  (select-dao 'notification
    (where (:= :user_id (object-id user)))
    (order-by (:desc :created-at))))


(defun mark-as-seen (user)
  (mito:execute-sql "UPDATE notification
                        SET seen_at = NOW()
                     WHERE user_id = ?
                       AND seen_at IS NULL"
                    (list (object-id user))))
