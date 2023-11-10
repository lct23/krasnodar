(uiop:define-package #:app/utils
  (:use #:cl)
  (:import-from #:local-time
                #:now
                #:+iso-8601-date-format+)
  (:import-from #:str
                #:replace-all)
  (:import-from #:reblocks/html)
  (:import-from #:3bmd)
  (:import-from #:humanize-duration
                #:humanize-duration)
  (:import-from #:local-time-duration
                #:timestamp-difference)
  (:import-from #:humanize-duration/ru))
(in-package #:app/utils)


(defparameter +human-datetime-format+
  ;; 2008-11-18 02:32
  (append +iso-8601-date-format+
          (list #\Space)
          '((:hour 2) #\: (:min 2))))


(defun normalize-markdown (text)
  "Команда pandoc -f docx -t markdown -o FAQ.md FAQ.docx
   переводы строк почему-то кодирует в виде \ на конце строки.

   Но 3BMD не понимает этого. Надо заменять на двойные пробелы, которые тоже
   обозначают перевод строки."

  (replace-all "(?m)\\\\"
               "  "
               text
               :regex t))


(defun markdown-to-html (text &key (stream reblocks/html:*stream*))
  ;; TODO: Здесь надо встроить проверку документа по white-list,
  ;; чтобы предотвратить XSS уязвимость в markdown документах.
  (3bmd:parse-string-and-print-to-stream
   (normalize-markdown text)
   stream))


(defun time-to (timestamp &key (base-ts (now)))
  (humanize-duration
   (timestamp-difference timestamp base-ts)
   :format-part #'humanize-duration/ru:format-part))
