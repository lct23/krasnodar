(uiop:define-package #:app/pages/dashboard/hr
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks-ui2/tables/table
                #:column
                #:make-table)
  (:import-from #:app/pages/utils
                #:title)
  (:import-from #:app/analytics
                #:period-title
                #:get-stats-for-hr-dashboard)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:app/widgets/service-stats
                #:make-service-stats-widget))
(in-package #:app/pages/dashboard/hr)


(defwidget hr-dashboard-widget ()
  ())


(defun make-hr-dashboard-widget ()
  (make-instance 'hr-dashboard-widget))


(defmethod render ((widget hr-dashboard-widget))
  (title "Дашборд HR")

  (with-html
    ;; Статистика по кадрам
    (:div :class "flex flex-col gap-8"
          (render (make-service-stats-widget))

          (:h1 :class "text-xl font-bold text-center"
               "Успешность прохождения онбординга")
          ;; А это статистика по прохождению онбординга
          (let* ((data (get-stats-for-hr-dashboard))
                 (period-titles (list "1 день"
                                      "1 неделя"
                                      "1 месяц"
                                      "3 месяца"
                                      "6 месяцев"
                                      "12 месяцев"
                                      "18 месяцев"
                                      "24 месяцев"))
                 (columns (list* (column "Этап")
                                 (mapcar #'column period-titles)))
                 (data-by-title (loop with result = (make-hash-table :test 'equal)
                                      for item in data
                                      do (setf (gethash (period-title item) result)
                                               item)
                                      finally (return result))))
            (flet ((make-row (title getter &key perc)
                     "Возвращает списки типа:
                      (list \"Прошло\" 2 1 6 11 23 6 30 44)"
                     (list* title
                            (loop for period-title in period-titles
                                  for item = (gethash period-title data-by-title)
                                  for value = (when item
                                                (funcall getter item))
                                  for processed-value = (or
                                                         (if (and perc value)
                                                             (fmt "~,1F%"
                                                                  (float value))
                                                             value)
                                                         "-")
                                  collect (if item
                                              (princ-to-string processed-value)
                                              "-")))))
              (render (make-table columns
                                  (list (make-row "Прошло" #'app/analytics::proshlo)
                                        (make-row "Проходит" #'app/analytics::prohodit)
                                        (make-row "Успех" #'app/analytics::avg-progress :perc t)
                                        (make-row "Провалы" #'app/analytics::incorrect-answers)
                                        (make-row "Отставания" #'app/analytics::delayed-answers)
                                        (make-row "Просрочки" #'app/analytics::overdue-answers)))))))))

