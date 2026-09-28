-- Information system "Faculty display classes schedule"
-- Database schema (MySQL 8+). Re-running this script recreates the database from scratch.

DROP DATABASE IF EXISTS display_classes;
CREATE DATABASE display_classes CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE display_classes;

-- 1. Display classes (computer rooms)
CREATE TABLE classrooms (
    id           INT AUTO_INCREMENT PRIMARY KEY,
    room_number  VARCHAR(20)  NOT NULL UNIQUE,
    building     VARCHAR(50)  NOT NULL,
    floor        INT          NOT NULL,
    capacity     INT          NOT NULL,
    open_time    TIME         NOT NULL,
    close_time   TIME         NOT NULL,
    description  VARCHAR(255) NULL,
    CONSTRAINT chk_classroom_capacity CHECK (capacity > 0),
    CONSTRAINT chk_classroom_hours    CHECK (open_time < close_time)
);

-- 2. Individual workstations inside classes
CREATE TABLE workstations (
    id                INT AUTO_INCREMENT PRIMARY KEY,
    classroom_id      INT          NOT NULL,
    seat_number       INT          NOT NULL,
    inventory_number  VARCHAR(30)  NOT NULL UNIQUE,
    cpu               VARCHAR(60)  NOT NULL,
    ram_gb            INT          NOT NULL,
    os                VARCHAR(60)  NOT NULL,
    status            ENUM('working', 'repair', 'off') NOT NULL DEFAULT 'working',
    CONSTRAINT uq_workstation_seat UNIQUE (classroom_id, seat_number),
    CONSTRAINT chk_workstation_ram CHECK (ram_gb > 0),
    CONSTRAINT fk_workstation_classroom FOREIGN KEY (classroom_id)
        REFERENCES classrooms (id) ON DELETE CASCADE
);

-- 3. Users (students, teachers, administrators)
CREATE TABLE users (
    id           INT AUTO_INCREMENT PRIMARY KEY,
    full_name    VARCHAR(100) NOT NULL,
    role         ENUM('student', 'teacher', 'admin') NOT NULL,
    study_group  VARCHAR(20)  NULL,
    email        VARCHAR(100) NOT NULL UNIQUE,
    phone        VARCHAR(20)  NULL
);

-- 4. Teachers (extra data for users with role 'teacher')
CREATE TABLE teachers (
    id          INT AUTO_INCREMENT PRIMARY KEY,
    user_id     INT          NOT NULL UNIQUE,
    department  VARCHAR(100) NOT NULL,
    position    VARCHAR(60)  NOT NULL,
    CONSTRAINT fk_teacher_user FOREIGN KEY (user_id)
        REFERENCES users (id) ON DELETE CASCADE
);

-- 5. Disciplines taught in display classes
CREATE TABLE disciplines (
    id     INT AUTO_INCREMENT PRIMARY KEY,
    name   VARCHAR(100) NOT NULL UNIQUE,
    hours  INT          NOT NULL,
    CONSTRAINT chk_discipline_hours CHECK (hours > 0)
);

-- 6. Planned lessons (weekly schedule)
CREATE TABLE schedule (
    id             INT AUTO_INCREMENT PRIMARY KEY,
    classroom_id   INT         NOT NULL,
    teacher_id     INT         NOT NULL,
    discipline_id  INT         NOT NULL,
    study_group    VARCHAR(20) NOT NULL,
    week_day       ENUM('mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun') NOT NULL,
    start_time     TIME        NOT NULL,
    end_time       TIME        NOT NULL,
    lesson_type    ENUM('lecture', 'lab', 'practice') NOT NULL,
    valid_from     DATE        NOT NULL,
    valid_to       DATE        NOT NULL,
    CONSTRAINT chk_schedule_time  CHECK (start_time < end_time),
    CONSTRAINT chk_schedule_dates CHECK (valid_from <= valid_to),
    CONSTRAINT fk_schedule_classroom FOREIGN KEY (classroom_id)
        REFERENCES classrooms (id) ON DELETE RESTRICT,
    CONSTRAINT fk_schedule_teacher FOREIGN KEY (teacher_id)
        REFERENCES teachers (id) ON DELETE RESTRICT,
    CONSTRAINT fk_schedule_discipline FOREIGN KEY (discipline_id)
        REFERENCES disciplines (id) ON DELETE RESTRICT
);

-- 7. Free access time (classes open for individual work)
CREATE TABLE free_access (
    id            INT AUTO_INCREMENT PRIMARY KEY,
    classroom_id  INT          NOT NULL,
    week_day      ENUM('mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun') NOT NULL,
    start_time    TIME         NOT NULL,
    end_time      TIME         NOT NULL,
    duty_user_id  INT          NULL,
    note          VARCHAR(255) NULL,
    CONSTRAINT chk_free_access_time CHECK (start_time < end_time),
    CONSTRAINT fk_free_access_classroom FOREIGN KEY (classroom_id)
        REFERENCES classrooms (id) ON DELETE CASCADE,
    CONSTRAINT fk_free_access_duty FOREIGN KEY (duty_user_id)
        REFERENCES users (id) ON DELETE SET NULL
);

-- 8. Actual usage of workstations (source for load statistics)
CREATE TABLE workstation_sessions (
    id              INT AUTO_INCREMENT PRIMARY KEY,
    workstation_id  INT          NOT NULL,
    user_id         INT          NOT NULL,
    session_date    DATE         NOT NULL,
    start_time      TIME         NOT NULL,
    end_time        TIME         NOT NULL,
    purpose         VARCHAR(255) NULL,
    CONSTRAINT chk_session_time CHECK (start_time < end_time),
    CONSTRAINT fk_session_workstation FOREIGN KEY (workstation_id)
        REFERENCES workstations (id) ON DELETE CASCADE,
    CONSTRAINT fk_session_user FOREIGN KEY (user_id)
        REFERENCES users (id) ON DELETE CASCADE
);

-- 9. Maintenance log of workstations
CREATE TABLE maintenance_log (
    id              INT AUTO_INCREMENT PRIMARY KEY,
    workstation_id  INT          NOT NULL,
    log_date        DATE         NOT NULL,
    description     VARCHAR(255) NOT NULL,
    performed_by    INT          NULL,
    CONSTRAINT fk_maintenance_workstation FOREIGN KEY (workstation_id)
        REFERENCES workstations (id) ON DELETE CASCADE,
    CONSTRAINT fk_maintenance_user FOREIGN KEY (performed_by)
        REFERENCES users (id) ON DELETE SET NULL
);

-- Weekly load of each class: planned lessons and free access vs. working hours (Mon-Sat)
CREATE VIEW v_classroom_load AS
SELECT c.id,
       c.room_number,
       ROUND((TIME_TO_SEC(c.close_time) - TIME_TO_SEC(c.open_time)) * 6 / 3600, 1) AS open_hours,
       ROUND(COALESCE(s.secs, 0) / 3600, 1) AS lesson_hours,
       ROUND(COALESCE(f.secs, 0) / 3600, 1) AS free_access_hours,
       ROUND(COALESCE(s.secs, 0) * 100
             / ((TIME_TO_SEC(c.close_time) - TIME_TO_SEC(c.open_time)) * 6), 1) AS load_percent
FROM classrooms c
LEFT JOIN (SELECT classroom_id, SUM(TIME_TO_SEC(end_time) - TIME_TO_SEC(start_time)) AS secs
           FROM schedule GROUP BY classroom_id) s ON s.classroom_id = c.id
LEFT JOIN (SELECT classroom_id, SUM(TIME_TO_SEC(end_time) - TIME_TO_SEC(start_time)) AS secs
           FROM free_access GROUP BY classroom_id) f ON f.classroom_id = c.id;

-- Total usage of each workstation over all recorded sessions
CREATE VIEW v_workstation_load AS
SELECT w.id,
       c.room_number,
       w.seat_number,
       w.inventory_number,
       w.status,
       COUNT(ws.id) AS sessions,
       ROUND(COALESCE(SUM(TIME_TO_SEC(ws.end_time) - TIME_TO_SEC(ws.start_time)), 0) / 3600, 1) AS used_hours,
       MAX(ws.session_date) AS last_used
FROM workstations w
JOIN classrooms c ON c.id = w.classroom_id
LEFT JOIN workstation_sessions ws ON ws.workstation_id = w.id
GROUP BY w.id, c.room_number, w.seat_number, w.inventory_number, w.status;
