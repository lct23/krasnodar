(uiop:define-package #:app/widgets/board-progress
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/models/board-progress
                #:starts-at
                #:ends-at
                #:period-knowledge-progress-percent
                #:period-knowledge-progress-status
                #:period-knowledge-progress-title
                #:get-knowledge-progresses
                #:period-title
                #:get-periods)
  (:import-from #:reblocks-ui2/tables/table
                #:column
                #:make-table)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:app/widgets/redirect-button
                #:redirect-button)
  (:import-from #:app/utils
                #:time-to)
  (:import-from #:local-time
                #:timestamp<
                #:now))
(in-package #:app/widgets/board-progress)


(defwidget board-progress-widget ()
  ((board-progress :initarg :board-progress
                   :reader board-progress)))


(defun make-board-progress-widget (board-progress)
  (make-instance 'board-progress-widget
                 :board-progress board-progress))


(defmethod render ((widget board-progress-widget))
  (with-html
    (flet ((columns (not-started-yet)
             (list (column "Знание"
                           :getter #'period-knowledge-progress-title
                           :classes (list "w-full"))
                   (column "Статус"
                           :getter #'period-knowledge-progress-status)
                   (column "Успех"
                           :getter (lambda (obj)
                                     (let ((value (period-knowledge-progress-percent obj)))
                                       (if (zerop value)
                                           "-"
                                           (fmt "~A%" value)))))
                   (column "Действия"
                           :getter (lambda (obj)
                                     (let ((value (period-knowledge-progress-percent obj)))
                                       (if (zerop value)
                                           (redirect-button "Изучить"
                                                            (fmt "/learn/~A"
                                                                 (mito:object-id obj))
                                                            :disabled not-started-yet)
                                           "")))))))
      (:div :class "flex flex-col gap-8"
            (loop with has-progresses = nil
                  and now = (now)
                  for period in (get-periods (board-progress widget))
                  for title = (app/models/board-progress::period-title period)
                  for progresses = (get-knowledge-progresses period)
                  for ends-at = (ends-at period)
                  for starts-at = (starts-at period)
                  for not-started-yet = (timestamp< now starts-at)
                  when progresses
                  do (setf has-progresses t)
                     (:div :class "flex flex-col gap-4"
                           (:h1 :class "inline-flex gap-2"
                                ;; (:span (period-title period))
                                (cond
                                  (not-started-yet
                                   (:span (fmt "До начала ~A"
                                               (time-to starts-at))))
                                  ((timestamp< now ends-at)
                                   (:span (fmt "Осталось ~A"
                                               (time-to ends-at))))
                                  (t
                                   (:span :class "text-red-700"
                                          (fmt "~A: Дедлайн истёк ~A назад"
                                               title
                                               (time-to now :base-ts ends-at))))))
                           (render (make-table (columns not-started-yet)
                                               progresses)))
                  finally (unless has-progresses
                            (:div :class "text-xl font-bold text-center"
                                  "Поздравляем! Вы прошли все задачи онбординга!")))))))

