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
                #:redirect-button))
(in-package #:app/widgets/board-progress)


(defwidget board-progress-widget ()
  ((board-progress :initarg :board-progress
                   :reader board-progress)))


(defun make-board-progress-widget (board-progress)
  (make-instance 'board-progress-widget
                 :board-progress board-progress))


(defmethod render ((widget board-progress-widget))
  (with-html
    (let ((columns
            (list (column "Знание"
                          :getter #'period-knowledge-progress-title)
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
                                                                (mito:object-id obj)))
                                          "")))))))
      (loop with has-progresses = nil
            for period in (get-periods (board-progress widget))
            for progresses = (get-knowledge-progresses period)
            when progresses
            do (setf has-progresses t)
               (:h1 (period-title period))
               (render (make-table columns
                                   progresses))
            finally (unless has-progresses
                      (:div :class "text-xl font-bold text-center"
                            "Поздравляем! Вы прошли все задачи онбординга!"))))))

