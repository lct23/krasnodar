(uiop:define-package #:app/notifications/creator
  (:use #:cl)
  (:import-from #:40ants-pg/transactions
                #:with-transaction)
  (:import-from #:reblocks-auth/models
                #:get-all-users)
  (:import-from #:app/models/notification
                #:make-notification)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:app/models/knowledge
                #:period-knowledge-progress-id
                #:deadline-missed-p
                #:find-pending-knowledges
                #:knownledge-title)
  (:import-from #:mito
                #:object-id)
  (:import-from #:alexandria
                #:length=))
(in-package #:app/notifications/creator)


(defun create-notification-for-user (user &key dry-run)
  ;; TODO: по хорошему, надо в нотификации запоминать что
  ;; мы уже слали уведомление про данное знание, чтобы не было дублей 
  (let ((next-knowledges (find-pending-knowledges user)))
    (when next-knowledges
      (let* ((next-knowledge (first next-knowledges))
             (missed-deadlines (remove-if-not #'deadline-missed-p
                                              next-knowledges))
             (critical (when missed-deadlines
                         t))
             (title (fmt "Изучите \"~A\"~@[ ~A~]"
                         (knownledge-title next-knowledge)
                         (when missed-deadlines
                           (fmt "(~A просрочено!)"
                                (length missed-deadlines)))))
             (text
               (cond
                 ((length= 1 next-knowledges)
                  (fmt "Пройдите шаг \"[~A](https://hrzero.ru/learn/~A)\""
                       (knownledge-title next-knowledge)
                       (period-knowledge-progress-id next-knowledge)))
                 (t
                  (fmt "Всего надо изучить ~A материалов. Рекомендуем начать с документа \"[~A](https://hrzero.ru/learn/~A)\""
                       (length next-knowledges)
                       (knownledge-title next-knowledge)
                       (period-knowledge-progress-id next-knowledge))))))
        (if dry-run
            (list title
                  text
                  critical)
            (make-notification user title text :critical critical))))))


(defun create-notifications-for-all-users (&key dry-run)
  ;; TODO: тут надо будет придумать более масштабируемую схему
  ;; какой нибуд job-processing с расписанием подойдёт, если пользователей
  ;; будут тысячи и выше:
  (with-transaction
      (loop for user in (get-all-users)
            for notification = (create-notification-for-user user
                                                             :dry-run dry-run)
            when (and dry-run notification)
            collect (list* user notification))))
