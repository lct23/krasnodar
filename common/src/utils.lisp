(uiop:define-package #:common/utils
  (:use #:cl)
  (:import-from #:cl-json-pointer
                #:get-by-json-pointer)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:uuid)
  (:import-from #:yason
                #:with-output-to-string*)
  (:import-from #:mito
                #:object-id
                #:create-dao
                #:select-dao
                #:retrieve-dao
                #:retrieve-by-sql
                #:find-dao)
  (:import-from #:mito.dao
                #:select-by-sql)
  (:import-from #:sxql
                #:where
                #:order-by
                #:limit)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:40ants-pg/utils
                #:select-dao-by-ids)
  (:import-from #:local-time
                #:format-timestring)
  (:import-from #:function-cache
                #:defcached)
  (:export #:el
           #:dict
           #:encode-json
           #:decode-json
           ;; Полезные символы чтобы делать SQL запросы
           #:object-id
           #:create-dao
           #:with-connection
           #:select-dao
           #:select-dao-by-ids
           #:retrieve-dao
           #:retrieve-by-sql
           #:select-by-sql
           #:find-dao
           #:where
           #:order-by
           #:limit
           #:make-uuid
           #:format-datetime
           #:format-datetime-msk))
(in-package #:common/utils)


(defun el (hash path)
  (let ((path (if (char= (elt path 0) #\/)
                  path
                  (concatenate 'string "/"
                               path))))
    (get-by-json-pointer hash path :flavor :yason)))


(defun encode-json (obj)
  (with-output-to-string* ()
    (yason:encode obj)))


(defun decode-json (obj)
  (yason:parse obj))


(defun make-uuid ()
  (format nil "~(~A~)"
          (uuid:make-v4-uuid)))


(defun format-datetime (ts)
  (format-timestring nil ts
                     :format '((:year 4) #\- (:month 2) #\- (:day 2)
                               #\Space
                               (:hour 2) #\: (:min 2) #\: (:sec 2)
                               :gmt-offset-or-z)))


(defcached (msk-timezone :timeout 60) ()
  (local-time:find-timezone-by-location-name "Europe/Moscow"))


(defun format-datetime-msk (ts)
  (format-timestring nil ts
                     :format '((:year 4) #\- (:month 2) #\- (:day 2)
                               #\Space
                               (:hour 2) #\: (:min 2) #\: (:sec 2))
                     :timezone (msk-timezone)))
