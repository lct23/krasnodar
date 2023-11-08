
CREATE TABLE department (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE "user" (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    nickname TEXT NOT NULL,
    email VARCHAR(255),
    name TEXT NOT NULL,
    avatar_url TEXT,
    can_be_mentor BOOLEAN NOT NULL DEFAULT FALSE,
    mentor_id BIGINT references "user" on delete set null,
    boss BOOLEAN NOT NULL DEFAULT FALSE,
    department_id BIGINT NOT NULL DEFAULT 1 references department on delete cascade,
    position TEXT NOT NULL DEFAULT 'employee',
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE UNIQUE INDEX unique_user_email ON "user" (email);
CREATE UNIQUE INDEX unique_user_nickname ON "user" (nickname);



CREATE TABLE social_profile (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    user_id BIGINT NOT NULL,
    service TEXT NOT NULL,
    service_user_id TEXT NOT NULL,
    metadata JSONB NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE UNIQUE INDEX unique_social_profile_user_id_service_service_user_id ON social_profile (user_id, service, service_user_id);


CREATE TABLE registration_code (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    email VARCHAR(255) NOT NULL,
    code VARCHAR(255) NOT NULL,
    valid_until TIMESTAMP NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE TABLE document (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    text TEXT NOT NULL,
    department_id BIGINT,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE who_can_help (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    document_id BIGINT NOT NULL,
    person_id BIGINT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


-- Вопросники

CREATE TABLE questionnaire (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    document_id BIGINT,
    title TEXT NOT NULL DEFAULT '',
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE question (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    questionnaire_id BIGINT NOT NULL REFERENCES questionnaire ON DELETE CASCADE,
    question TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE possible_answer (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    question_id BIGINT NOT NULL  REFERENCES question ON DELETE CASCADE,
    text TEXT NOT NULL,
    correct BOOLEAN NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


-- Онбординг

CREATE TABLE board (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    department_id BIGINT REFERENCES department ON DELETE SET NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE board_period (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    board_id BIGINT NOT NULL REFERENCES board ON DELETE CASCADE,
    title TEXT NOT NULL,
    from_day INTEGER NOT NULL,
    to_day INTEGER NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE knowledge (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    department_id BIGINT REFERENCES department ON DELETE SET NULL,
    document_id BIGINT  REFERENCES document ON DELETE SET NULL,
    questionnaire_id BIGINT REFERENCES questionnaire ON DELETE SET NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE period_knowledge (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    period_id BIGINT NOT NULL REFERENCES board_period ON DELETE CASCADE,
    knowledge_id BIGINT NOT NULL REFERENCES knowledge ON DELETE CASCADE,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


-- Структура для логгированию прогресса прохождения онбордингов

CREATE TABLE board_progress (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    user_id BIGINT NOT NULL REFERENCES "user" ON DELETE CASCADE,
    board_id BIGINT NOT NULL REFERENCES board ON DELETE CASCADE,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE TABLE period_progress (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    board_progress_id BIGINT NOT NULL REFERENCES "board_progress" ON DELETE CASCADE,
    title TEXT NOT NULL,
    starts_at TIMESTAMPTZ NOT NULL,
    ends_at TIMESTAMPTZ NOT NULL,
    progress INTEGER NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE TABLE questionnaire_results (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    questionnaire_id BIGINT NOT NULL REFERENCES "questionnaire" ON DELETE CASCADE,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE TABLE question_response (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    questionnaire_results_id BIGINT NOT NULL REFERENCES "questionnaire_results" ON DELETE CASCADE,
    question_id BIGINT NOT NULL REFERENCES "question" ON DELETE CASCADE,
    answer_id BIGINT REFERENCES "possible_answer" ON DELETE CASCADE,
    answered_at TIMESTAMPTZ,
    answer_is_correct BOOLEAN,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE period_knowledge_progress (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    period_progress_id BIGINT NOT NULL REFERENCES "period_progress" ON DELETE CASCADE,
    period_knowledge_id BIGINT NOT NULL REFERENCES "period_knowledge" ON DELETE CASCADE,
    questionnaire_results_id BIGINT NOT NULL REFERENCES "questionnaire_results" ON DELETE CASCADE,
    questionnaire_progress INTEGER NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);
