CREATE TABLE "user" (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    nickname TEXT NOT NULL,
    email VARCHAR(255),
    roles text[] NOT NULL DEFAULT ARRAY[]::TEXT[],
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


CREATE TABLE department (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);
