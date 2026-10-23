-- Lab 1, variant 4
-- Information system "Information services of the faculty network"
-- 7 tables (> 5 as the assignment requires)

CREATE DATABASE IF NOT EXISTS faculty_services
  CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE faculty_services;

-- 1. Service kinds
CREATE TABLE IF NOT EXISTS kinds (
  id   INT AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(100) NOT NULL UNIQUE
);

-- 2. Service authors
CREATE TABLE IF NOT EXISTS authors (
  id         INT AUTO_INCREMENT PRIMARY KEY,
  full_name  VARCHAR(200) NOT NULL,
  email      VARCHAR(200) NOT NULL,
  department VARCHAR(200) NOT NULL
);

-- 3. Services: name, annotation, kind, version, terms,
--    end date and usage rules
CREATE TABLE IF NOT EXISTS services (
  id         INT AUTO_INCREMENT PRIMARY KEY,
  name       VARCHAR(200) NOT NULL,
  annotation TEXT NOT NULL,
  kind_id    INT NOT NULL,
  version    VARCHAR(50) NOT NULL,
  terms      TEXT NOT NULL,
  deadline   DATE NOT NULL,
  rules      TEXT NOT NULL,
  FOREIGN KEY (kind_id) REFERENCES kinds(id)
);

-- 4. Service-author link (many-to-many)
CREATE TABLE IF NOT EXISTS service_authors (
  id         INT AUTO_INCREMENT PRIMARY KEY,
  service_id INT NOT NULL,
  author_id  INT NOT NULL,
  UNIQUE KEY uq_sa (service_id, author_id),
  FOREIGN KEY (service_id) REFERENCES services(id) ON DELETE CASCADE,
  FOREIGN KEY (author_id)  REFERENCES authors(id)
);

-- 5. Faculty network users
CREATE TABLE IF NOT EXISTS users (
  id            INT AUTO_INCREMENT PRIMARY KEY,
  login         VARCHAR(100) NOT NULL UNIQUE,
  full_name     VARCHAR(200) NOT NULL,
  email         VARCHAR(200) NOT NULL,
  registered_at DATETIME NOT NULL
);

-- 6. User registrations on services
CREATE TABLE IF NOT EXISTS registrations (
  id            INT AUTO_INCREMENT PRIMARY KEY,
  user_id       INT NOT NULL,
  service_id    INT NOT NULL,
  registered_at DATETIME NOT NULL,
  UNIQUE KEY uq_reg (user_id, service_id),
  FOREIGN KEY (user_id)    REFERENCES users(id),
  FOREIGN KEY (service_id) REFERENCES services(id) ON DELETE CASCADE
);

-- 7. Usage statistics
CREATE TABLE IF NOT EXISTS usage_stats (
  id         INT AUTO_INCREMENT PRIMARY KEY,
  service_id INT NOT NULL,
  user_id    INT NOT NULL,
  used_at    DATETIME NOT NULL,
  action     VARCHAR(200) NOT NULL,
  FOREIGN KEY (service_id) REFERENCES services(id) ON DELETE CASCADE,
  FOREIGN KEY (user_id)    REFERENCES users(id)
);

-- Seed data for the demo
INSERT IGNORE INTO kinds (id, name) VALUES
  (1, 'Web service'),
  (2, 'File service'),
  (3, 'Mail service'),
  (4, 'Learning platform');

INSERT IGNORE INTO authors (id, full_name, email, department) VALUES
  (1, 'Ivan Petrenko',    'petrenko@univ.edu',  'Software Engineering Department'),
  (2, 'Olena Kovalenko',  'kovalenko@univ.edu', 'Information Systems Department'),
  (3, 'Andrii Sydorenko', 'sydorenko@univ.edu', 'Computing Center');

INSERT IGNORE INTO services (id, name, annotation, kind_id, version, terms, deadline, rules) VALUES
  (1, 'Class schedule', 'Faculty class schedule viewer', 1, '2.1',
     'Free for students and staff', '2027-06-30', 'Access from the faculty network only'),
  (2, 'File storage', 'Storage for teaching materials', 2, '1.4',
     '5 GB quota per user', '2026-12-31', 'No third-party content allowed'),
  (3, 'Faculty Moodle', 'Distance learning platform', 4, '4.3',
     'For enrolled students', '2028-01-15', 'Registration via the dean office');

INSERT IGNORE INTO service_authors (id, service_id, author_id) VALUES
  (1, 1, 1), (2, 2, 3), (3, 3, 2), (4, 3, 1);

INSERT IGNORE INTO users (id, login, full_name, email, registered_at) VALUES
  (1, 'shevniuk', 'Mykhailo Shevniuk', 'shevniuk@univ.edu', '2026-09-01 10:00:00'),
  (2, 'ivanova',  'Maria Ivanova',     'ivanova@univ.edu',  '2026-09-02 11:30:00');

INSERT IGNORE INTO registrations (id, user_id, service_id, registered_at) VALUES
  (1, 1, 1, '2026-09-01 10:05:00'),
  (2, 1, 3, '2026-09-01 10:10:00'),
  (3, 2, 1, '2026-09-02 11:35:00');

INSERT IGNORE INTO usage_stats (id, service_id, user_id, used_at, action) VALUES
  (1, 1, 1, '2026-09-05 09:00:00', 'view schedule'),
  (2, 1, 2, '2026-09-05 09:15:00', 'view schedule'),
  (3, 3, 1, '2026-09-06 14:00:00', 'course login'),
  (4, 1, 1, '2026-09-07 08:50:00', 'view schedule');
