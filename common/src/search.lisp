(uiop:define-package #:common/search
  (:use #:cl #:common/utils)
  (:import-from #:jonathan)
  (:import-from #:log)
  (:import-from #:dexador)
  (:import-from #:quri)
  (:import-from #:serapeum
                #:dict
                #:fmt)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:import-from #:yason
                #:with-output-to-string*)
  (:import-from #:alexandria
                #:make-keyword
                #:symbolicate
                #:lastcar
                #:length=
                #:ensure-list)
  (:import-from #:mito
                #:object-updated-at
                #:object-created-at
                #:object-id)
  (:import-from #:local-time
                #:timestamp-to-unix)
  (:import-from #:openrpc-server/method
                #:define-rpc-method)
  (:import-from #:common/event-bus
                #:on-event
                #:emit-event)
  (:export
   #:delete-object-from-index))
(in-package #:common/search)


(defun get-elastic-host ()
  (or (uiop:getenv "ELASTIC_HOST")
      "192.168.0.103"))

(defun get-elastic-port ()
  (or (uiop:getenv "ELASTIC_PORT")
      "9200"))

(defun get-elastic-user ()
  (or (uiop:getenv "ELASTIC_USER")
      "admin"))

(defun get-elastic-password ()
  (uiop:getenv "ELASTIC_PASS"))


(defun index (collection id data)
  (let ((content (jonathan:to-json data))
        (url (fmt "https://~A:~A/~A/_doc/~A?refresh&routing=1"
                  (get-elastic-host)
                  (get-elastic-port)
                  collection
                  (quri:url-encode id))))
    ;; (log:info "Sending data to Elastic Search" collection id)
    (jonathan:parse
     (dex:put url
              :content content
              :headers '(("Content-Type" . "application/json"))
              :insecure t
              :basic-auth (cons (get-elastic-user)
                                (get-elastic-password))))))

(defun check-analyzer (analyzer text)
  (let ((content (jonathan:to-json (dict "analyzer" analyzer
                                         "text" text)))
        (url (fmt "https://~A:~A/_analyze"
                  (get-elastic-host)
                  (get-elastic-port))))
    (log:info "Checking ~S analyzer" analyzer)
    (jonathan:parse
     (dex:post url
               :content content
               :headers '(("Content-Type" . "application/json"))
               :insecure t
               :basic-auth (cons (get-elastic-user)
                                 (get-elastic-password))))))


(defun create-index (index &key relation properties)
  "Создаёт Elastic индекс с включенной русской морфологией."
  (let ((content (dict "settings"
                       (dict "analysis"
                             (dict "analyzer"
                                   (dict "default"
                                         (dict "type" "russian"))))))
        (properties (or properties
                        (dict)))
        (url (fmt "https://~A:~A/~A"
                  (get-elastic-host)
                  (get-elastic-port)
                  index)))
    (when relation
      (destructuring-bind (parent child)
          relation
        (setf (gethash "object_type" properties)
              (dict "type" "join"
                    "relations"
                    (dict parent child)))))

    (unless (zerop (hash-table-count properties))
      (setf (gethash "mappings" content)
            (dict "properties" properties)))
    
    (jonathan:parse
     (dex:put url
              :content (jonathan:to-json content)
              :headers '(("Content-Type" . "application/json"))
              :insecure t
              :basic-auth (cons (get-elastic-user)
                                (get-elastic-password))))))

;; (defun create-indices ()
;;   (create-index "projects")
;;   (create-index "users")
;;   (create-index "jobs")
;;   (values))


(defun delete-index (collection)
  (let ((url (fmt "https://~A:~A/~A"
                  (get-elastic-host)
                  (get-elastic-port)
                  collection)))
    (log:info "Deleting index" collection)
    (jonathan:parse
     (dex:delete url
                 :headers '(("Content-Type" . "application/json"))
                 :insecure t
                 :basic-auth (cons (get-elastic-user)
                                   (get-elastic-password))))))


(defun delete-from-index (collection doc-id)
  (with-fields (:document-id doc-id)
    (let ((url (fmt "https://~A:~A/~A/_doc/~A"
                    (get-elastic-host)
                    (get-elastic-port)
                    collection
                    (quri:url-encode doc-id))))
      (log:info "Deleting document from index" collection)
      (jonathan:parse
       (dex:delete url
                   :headers '(("Content-Type" . "application/json"))
                   :insecure t
                   :basic-auth (cons (get-elastic-user)
                                     (get-elastic-password)))))))


(define-condition bad-query (error)
  ((original-error :initarg :original-error
                   :reader get-original-error)))


(defun make-sort-param (plist)
  "Делает из (list :created-at :desc :title :asc) список словарей:
   (list (dict \"created-at\" \"desc\")
         (dict \"title\" \"asc\"))
   "
  (loop for (key value) on plist by #'cddr
        collect (dict (string-downcase key)
                      (string-downcase value))))


(defun extract-next-page-key (sort-plist dict)
  "Достаёт из словаря элементы по ключам из параметров сортировки."
  (loop for key in (serapeum:plist-keys sort-plist)
        collect (gethash (string-downcase key) dict)))


(defun search-objects (collection term &key
                                         (sort (list :created-at :desc))
                                         (limit 10)
                                         (fields '("title" "description"))
                                         (page-key nil))
  ;; TODO: научиться обрабатывать 400 ответы от Elastic
  ;; например на запрос: TYPE:macro AND storage NAME:FLEXI-STREAMS:WITH-OUTPUT-TO-SEQUENCE
  (handler-case
      (loop with url = (fmt "https://~A:~A/~A/_search"
                            (get-elastic-host)
                            (get-elastic-port)
                            collection)
            with sort-param = (make-sort-param sort)
            with query = (dict "query" (etypecase term
                                         (hash-table term)
                                         (string
                                          (dict
                                           "query_string"
                                           (dict "fields" fields
                                                 "query" term))))
                               ;; Сортировка используется для keyset пейджинации
                               ;; и подгрузки результатов:
                               ;; https://www.elastic.co/guide/en/elasticsearch/reference/current/paginate-search-results.html
                               "sort" sort-param
                               "size" limit)
            with content = (with-output-to-string* ()
                             (when page-key
                               (setf (gethash "search_after" query)
                                     (ensure-list page-key)))
                             (yason:encode query))
            with body = (dex:post url
                                  :content content
                                  :headers '(("Content-Type" . "application/json"))
                                  :insecure t
                                  :basic-auth (cons (get-elastic-user)
                                                    (get-elastic-password)))
            with response = (yason:parse body)
            with total = (el response "hits/total/value")
            for hit in (el response "hits/hits")
            for id = (el hit "_id")
            for source = (el hit "_source")
            for doc = (el source "documentation")
            collect source into results
            finally (return (if (length= limit results)
                                ;; Возможно есть следующая страница
                                (values results
                                        total
                                        (extract-next-page-key sort (lastcar results)))
                                (values results
                                        total))))
    (dexador.error:http-request-not-found ()
      (values nil 0 nil))
    (dexador.error:http-request-bad-request (condition)
      (error 'bad-query :original-error condition))))


(defun all-objects (collection)
  (search-objects collection
                  (dict
                   "match_all" (dict))
                  :sort (list :_id :desc)))


(defgeneric make-document-for-index (obj)
  (:documentation "Индексирует проект указанного типа"))

(defgeneric get-index-name (obj)
  (:documentation "Возвращает название индекса для указанного типа объектов.")
  (:method ((obj symbol))
    (fmt "~As"
         (string-downcase obj)))
  (:method ((obj standard-object))
    (fmt "~As"
         (string-downcase (type-of obj)))))


(defgeneric get-objects-to-index (class-name)
  (:method ((class-name symbol))
    (mito:retrieve-dao class-name)))


(defgeneric get-fields-to-search (class-name)
  (:documentation "Возвращает список полей, по которым надо производить поиск.")
  (:method ((class-name symbol))
    (list "title" "description")))


(defun index-object (object)
  (let* ((docs (ensure-list (make-document-for-index object))))

    (destructuring-bind (doc . subdocuments)
        docs
      ;; Первый объект в списке особый, для него мы автоматически можем прописать id, created-at и updated-at
      (unless (gethash "id" doc)
        (setf (gethash "id" doc)
              (object-id object)))
      
      ;; Эти поля есть у всех объектов из базы
      (setf (gethash "created-at" doc)
            (timestamp-to-unix (object-created-at object))
            (gethash "updated-at" doc)
            (timestamp-to-unix (object-updated-at object)))

      (let ((index-name (get-index-name object)))
        (index index-name
               (fmt "~A" (gethash "id" doc))
               doc)

        ;; У поддокументов всегда должен быть прописан id
        (loop for subdoc in subdocuments
              do (index index-name
                        (fmt "~A" (gethash "id" subdoc))
                        subdoc))))))


(defun delete-object-from-index (object)
  (let* ((docs (ensure-list (make-document-for-index object))))
    ;; Первый объект в списке особый, для него мы автоматически можем прописать id, created-at и updated-at
    (let ((doc (first docs)))
      (unless (gethash "id" doc)
        (setf (gethash "id" doc)
              (object-id object))))
    
    (let ((index-name (get-index-name object)))
      (loop for doc in docs
            do (delete-from-index index-name
                                  (princ-to-string (gethash "id" doc)))))))


(defun index-objects-of-type (class-name)
  "Индексируем все объекты указанного типа.
   Для прода надо будет сделать какой-то pipeline, чтобы добавлять в индекс только новые или обновлённые."
  ;; (with-connection ()
  ;;   (common/db:with-lock ((fmt "index-~A" class-name) :timeout 600)
  ;;     (loop for obj in (get-objects-to-index class-name)
  ;;           do (index-object obj))))
  )


(defun noop-enricher (objects fields)
  (declare (ignore fields))
  objects)


(defmacro define-search-rpc-method ((api-name method-name class-name
                                     &key (enrich-func ''noop-enricher))
                                    &body metadata)
  "Генератор методов для поиска по разным типам объектов.
   Просто чтобы не повторять примерно один и тот же код."
  (let* ((index-func-name (symbolicate "INDEX-" class-name "S-IN-THREAD"))
         (create-event (make-keyword (symbolicate class-name "-CREATED")))
         (update-event (make-keyword (symbolicate class-name "-UPDATED")))
         (delete-event (make-keyword (symbolicate class-name "-DELETED"))))
    `(progn
       (define-rpc-method (,api-name ,method-name) (query &key (limit 10) page-key additional-fields)
         (:param query string "Запрос для поиска на языке запросов ElasticSearch.")
         (:param limit integer)
         (:param page-key string)
         (:param additional-fields (list-of string)
                 "Список дополнительных полей, которые надо подгрузить из других таблиц.")
         (:result (paginated-list-of ,class-name))
         ,@metadata

         (when page-key
           (setf page-key
                 (decode-json page-key)))

         (let ((index-name (get-index-name ',class-name))
               (fields (get-fields-to-search ',class-name)))
           (log:info "Searching for objects in index ~S by query ~S among fields ~{~A~^, ~}."
                     index-name query fields)
        
           (multiple-value-bind (search-results total next-page-key)
               (search-objects index-name
                               query
                               :fields fields
                               :limit limit
                               :page-key page-key)
             (declare (ignore total))
             (let* ((ids (loop for result in search-results
                               collect (el result "id")))
                    (results (when ids
                               (with-connection ()
                                 (funcall ,enrich-func
                                          (mito:select-dao ',class-name
                                            (where (:in :id ids)))
                                          additional-fields)))))
               (if next-page-key
                   (values results
                           (encode-json next-page-key))
                   results)))))


       (defun ,index-func-name (&rest args)
         (declare (ignore args))
         (bt:make-thread (lambda ()
                           ;; Небольшая задержка, чтобы убедиться,
                           ;; что в соседнем потоке все данные успеют закоммититься:
                           (sleep 5)
                           (index-objects-of-type ',class-name))
                         :name (fmt "Index ~A"
                                    (get-index-name ',class-name))))

       ;; Эти методы будут посылать сигнал о том, что изменён объект
       ;; определённого типа:
       (defmethod mito:insert-dao :after ((obj ,class-name))
         (emit-event ,create-event obj))
       
       
       (defmethod mito:update-dao :after ((obj ,class-name))
         (emit-event ,update-event obj))


       (defmethod mito:delete-dao :after ((obj ,class-name))
         (emit-event ,delete-event obj))

       ;; А тут мы привязываем обработчики этих сигналов,
       ;; чтобы происходила автоматическая индексация:
       (loop for event in '(,create-event ,update-event ,delete-event)
             do (on-event event ',index-func-name)))))


