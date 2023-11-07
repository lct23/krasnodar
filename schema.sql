
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
    roles text[] NOT NULL DEFAULT ARRAY[]::TEXT[],
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
