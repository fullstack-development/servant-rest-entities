
CREATE TABLE IF NOT EXISTS auth (
  id SERIAL PRIMARY KEY ,
  password VARCHAR(255) not NULL,
  create_at DATE NOT NULL
);

CREATE TABLE IF NOT EXISTS users (
  id SERIAL PRIMARY KEY,
  first_name VARCHAR(255) not NULL,
  last_name VARCHAR(255) not NULL,
  created_at DATE NOT NULL,
  is_staff BOOLEAN NOT NULL,
  auth_id__id INTEGER REFERENCES auth
);
