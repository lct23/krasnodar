## HR Zero – проект для хакатона Лидеры Цифровой Трансформации (Краснодар) 2023

### Что успели сделать за 192 часа!

- 🎉 Бэкенд и фронт сервиса проведения онбординга сотрудников.
- 🌏 Интерфейс для добавления отделов сотрудников в базу данных
- 📕 Редактор документов.
- ✅ Редактор опросов.
- 💾 Две обучающие игры, чтобы знакомить новичка с коллективом
- 🧨 Server-side рендеринг на фронте, для лучшего SEO!

## Репозиторий

* https://github.com/lct23/krasnodar

## Стек технологий

* Бэкенд и вся бизнес-логика на Common Lisp.
* На фронтенде TailwindCSS + немного AlpineJS.
* База данных - PostgreSQL в облаке.
* Resend.com - для отправки email уведомлений.
* Российский аналог S3 для хранения аватаров.
* Развёртывание в Docker.

## Архитектура MVP

Для простоты и скоростиi MVP реализован в виде одного сервиса связанного с базой.
Мы использовали server-side рендеринг для того, чтобы всю бизнес-логику можно было
реализовать на фронтенде и не требовалось много JavaScript кода.

Так же, для простоты задачи которые должны выполняться по расписанию, запускаются тем же бэкендом.

Вот так выглядит сейчас архитектура сервиса:

```mermaid
graph TD;
   subgraph backend[back + front]
   Application
   end
   
   subgraph cloud
   DB[(Postgres)]
   S3[[S3]]
   end

   subgraph external-services
   Resend
   end

   Application --> DB
   Application --> Resend
   Application --> S3
```

## Развитие архитектуры

Понятно, что для стабильной работы сервис надо масштабировать, нужно интегрировать его с другими сервисами: аутентификацией, DWH, BI системой.
Для этого стоит добавить в картину "парочку" компонент:

```mermaid
graph TD;
   subgraph backend
   Application
   ImageStore
   DocumentStore
   IntegrationApps
   CronJobProcessor
   end

   subgraph frontend
   Frontend;
   end
   
   subgraph cloud
   ElasticSearch(((ElasticSearch)))
   DB[(Postgres)]
   S3[[S3]]
   CronJobCoordinator
   end

   subgraph external-services
   AuthProvider
   CompaniesStructure
   Resend
   DWH
   BI[BI Analytics]
   end
   
   Frontend --> Application
   Frontend --> ImageStore
   Frontend --> DocumentStore

   Application --> AuthProvider
   Application --> DB
   Application --> ElasticSearch
   Application --> CronJobCoordinator
   Application --> Resend

   IntegrationApps --> Application
   IntegrationApps --> DWH
   BI --> DWH
   
   CronJobProcessor --> CronJobCoordinator
   ImageStore --> S3
```

## Что интересного есть в коде???

* [Схема базы данных](https://github.com/lct23/krasnodar/blob/master/schema.sql)
* [Код моделей данных](https://github.com/lct23/krasnodar/tree/master/server/src/models) – используем Common Lisp ORM [Mito](https://github.com/fukamachi/mito) 
* [Код UI виджетов](https://github.com/lct23/krasnodar/tree/master/server/src/widgets) – в них почти вся бизнес-логика
* [Игры для знакомства с коллегами](https://github.com/lct23/krasnodar/tree/master/server/src/games)
* [SQL запросы для отображения статистики](https://github.com/lct23/krasnodar/blob/master/server/src/analytics.lisp)

## Несколько скринов с демо интерфейса

### Дашборд HR сотрудника

![](https://storage.yandexcloud.net/hrzero-files/lct-hr-dashboard.gif)

### Игра помогающая лучше узнать коллег
![](https://storage.yandexcloud.net/hrzero-files/lct-hr-game.gif)

