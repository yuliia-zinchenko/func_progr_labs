-- Test data for "Faculty display classes schedule". Run after schema.sql.
USE display_classes;

INSERT INTO classrooms (room_number, building, floor, capacity, open_time, close_time, description) VALUES
    ('101', 'Main building', 1, 12, '08:00', '20:00', 'General purpose display class'),
    ('205', 'Main building', 2, 15, '08:00', '20:00', 'Database and web development lab'),
    ('310', 'Main building', 3, 10, '08:30', '18:00', 'GPU workstations for machine learning'),
    ('401', 'Building B',    4, 12, '08:00', '21:00', 'Networks and operating systems lab'),
    ('115', 'Building B',    1,  8, '09:00', '17:00', 'Information security lab');

-- One workstation per seat in every class
INSERT INTO workstations (classroom_id, seat_number, inventory_number, cpu, ram_gb, os)
WITH RECURSIVE seats (n) AS (SELECT 1 UNION ALL SELECT n + 1 FROM seats WHERE n < 20)
SELECT c.id,
       s.n,
       CONCAT('INV-', c.room_number, '-', LPAD(s.n, 2, '0')),
       CASE c.room_number
           WHEN '310' THEN 'AMD Ryzen 9 7900'
           WHEN '115' THEN 'Intel Core i5-12400'
           ELSE 'Intel Core i7-12700'
       END,
       CASE c.room_number WHEN '310' THEN 64 WHEN '115' THEN 16 ELSE 32 END,
       CASE c.room_number
           WHEN '401' THEN 'Ubuntu 24.04'
           WHEN '115' THEN 'Kali Linux 2026.1'
           ELSE 'Windows 11'
       END
FROM classrooms c
JOIN seats s ON s.n <= c.capacity
ORDER BY c.id, s.n;

UPDATE workstations SET status = 'repair' WHERE inventory_number IN ('INV-101-05', 'INV-310-03');
UPDATE workstations SET status = 'off'    WHERE inventory_number = 'INV-115-06';

INSERT INTO users (full_name, role, study_group, email, phone) VALUES
    ('Oksana Melnyk',      'admin',   NULL,     'o.melnyk@univ.edu.ua',      '+380501112233'),
    ('Ivan Petrenko',      'teacher', NULL,     'i.petrenko@univ.edu.ua',    '+380502223344'),
    ('Olena Kovalenko',    'teacher', NULL,     'o.kovalenko@univ.edu.ua',   '+380503334455'),
    ('Andrii Shevchenko',  'teacher', NULL,     'a.shevchenko@univ.edu.ua',  NULL),
    ('Natalia Bondarenko', 'teacher', NULL,     'n.bondarenko@univ.edu.ua',  '+380505556677'),
    ('Serhii Tkachenko',   'teacher', NULL,     's.tkachenko@univ.edu.ua',   NULL),
    ('Iryna Kravchenko',   'teacher', NULL,     'i.kravchenko@univ.edu.ua',  '+380507778899'),
    ('Maksym Boiko',       'student', 'KN-41',  'm.boiko@student.edu.ua',    NULL),
    ('Anna Lysenko',       'student', 'KN-41',  'a.lysenko@student.edu.ua',  '+380631234567'),
    ('Dmytro Hrytsenko',   'student', 'KN-41',  'd.hrytsenko@student.edu.ua', NULL),
    ('Sofiia Moroz',       'student', 'KN-41',  's.moroz@student.edu.ua',    NULL),
    ('Taras Savchenko',    'student', 'KN-41',  't.savchenko@student.edu.ua', NULL),
    ('Yuliia Rudenko',     'student', 'KN-42',  'y.rudenko@student.edu.ua',  '+380632345678'),
    ('Bohdan Marchenko',   'student', 'KN-42',  'b.marchenko@student.edu.ua', NULL),
    ('Kateryna Pavlenko',  'student', 'KN-42',  'k.pavlenko@student.edu.ua', NULL),
    ('Oleh Klymenko',      'student', 'KN-42',  'o.klymenko@student.edu.ua', NULL),
    ('Viktoriia Honchar',  'student', 'IPZ-31', 'v.honchar@student.edu.ua',  NULL),
    ('Roman Kuzmenko',     'student', 'IPZ-31', 'r.kuzmenko@student.edu.ua', '+380633456789'),
    ('Daryna Polishchuk',  'student', 'IPZ-31', 'd.polishchuk@student.edu.ua', NULL),
    ('Artem Voloshyn',     'student', 'IPZ-31', 'a.voloshyn@student.edu.ua', NULL);

INSERT INTO teachers (user_id, department, position) VALUES
    (2, 'Computer Science',        'Associate Professor'),
    (3, 'Information Systems',     'Professor'),
    (4, 'Computer Networks',       'Senior Lecturer'),
    (5, 'Software Engineering',    'Associate Professor'),
    (6, 'Applied Mathematics',     'Assistant'),
    (7, 'Cybersecurity',           'Senior Lecturer');

INSERT INTO disciplines (name, hours) VALUES
    ('Functional Programming', 60),
    ('Databases',              90),
    ('Computer Networks',      60),
    ('Web Development',        90),
    ('Operating Systems',      60),
    ('Machine Learning',       90),
    ('Computer Graphics',      60),
    ('Information Security',   60);

-- Lesson slots: 08:30-09:50, 10:10-11:30, 11:50-13:10, 13:30-14:50, 15:10-16:30
INSERT INTO schedule (classroom_id, teacher_id, discipline_id, study_group, week_day,
                      start_time, end_time, lesson_type, valid_from, valid_to) VALUES
    (1, 1, 1, 'KN-41',  'mon', '08:30', '09:50', 'lab',      '2026-09-01', '2026-12-25'),
    (1, 1, 1, 'KN-42',  'mon', '10:10', '11:30', 'lab',      '2026-09-01', '2026-12-25'),
    (2, 2, 2, 'IPZ-31', 'mon', '08:30', '09:50', 'lab',      '2026-09-01', '2026-12-25'),
    (2, 2, 2, 'KN-41',  'mon', '11:50', '13:10', 'lab',      '2026-09-01', '2026-12-25'),
    (4, 3, 3, 'KN-42',  'mon', '11:50', '13:10', 'practice', '2026-09-01', '2026-12-25'),
    (1, 4, 4, 'IPZ-31', 'tue', '10:10', '11:30', 'lab',      '2026-09-01', '2026-12-25'),
    (2, 5, 5, 'KN-41',  'tue', '08:30', '09:50', 'lab',      '2026-09-01', '2026-12-25'),
    (3, 6, 6, 'KN-42',  'tue', '10:10', '11:30', 'lab',      '2026-09-01', '2026-12-25'),
    (4, 3, 3, 'IPZ-31', 'tue', '13:30', '14:50', 'lab',      '2026-09-01', '2026-12-25'),
    (1, 1, 1, 'IPZ-31', 'wed', '08:30', '09:50', 'lecture',  '2026-09-01', '2026-12-25'),
    (2, 2, 2, 'KN-42',  'wed', '10:10', '11:30', 'lab',      '2026-09-01', '2026-12-25'),
    (4, 4, 4, 'KN-41',  'wed', '10:10', '11:30', 'lab',      '2026-09-01', '2026-12-25'),
    (5, 6, 8, 'KN-41',  'wed', '13:30', '14:50', 'lab',      '2026-09-01', '2026-12-25'),
    (1, 5, 5, 'KN-42',  'thu', '08:30', '09:50', 'lab',      '2026-09-01', '2026-12-25'),
    (2, 4, 7, 'IPZ-31', 'thu', '10:10', '11:30', 'practice', '2026-09-01', '2026-12-25'),
    (3, 6, 6, 'KN-41',  'thu', '11:50', '13:10', 'lab',      '2026-09-01', '2026-12-25'),
    (4, 3, 3, 'KN-41',  'thu', '08:30', '09:50', 'lab',      '2026-09-01', '2026-12-25'),
    (1, 2, 2, 'KN-41',  'fri', '10:10', '11:30', 'lab',      '2026-09-01', '2026-12-25'),
    (2, 1, 1, 'KN-42',  'fri', '11:50', '13:10', 'practice', '2026-09-01', '2026-12-25'),
    (4, 5, 5, 'IPZ-31', 'fri', '08:30', '09:50', 'lab',      '2026-09-01', '2026-12-25');

INSERT INTO free_access (classroom_id, week_day, start_time, end_time, duty_user_id, note) VALUES
    (1, 'mon', '15:00', '19:00', 1,    'Course projects'),
    (1, 'wed', '13:30', '19:00', 1,    NULL),
    (2, 'tue', '13:30', '19:00', 1,    'Database homework'),
    (2, 'thu', '13:30', '19:00', NULL, NULL),
    (3, 'fri', '09:00', '17:00', 6,    'GPU access by request'),
    (4, 'wed', '13:30', '20:00', 1,    NULL),
    (4, 'sat', '10:00', '16:00', 1,    'Weekend access'),
    (5, 'fri', '09:00', '16:00', 7,    'Security lab practice');

INSERT INTO workstation_sessions (workstation_id, user_id, session_date, start_time, end_time, purpose) VALUES
    ( 1,  8, '2026-09-07', '08:30', '09:50', 'Lab: Functional Programming'),
    ( 2,  9, '2026-09-07', '08:30', '09:50', 'Lab: Functional Programming'),
    ( 3, 10, '2026-09-07', '08:30', '09:50', 'Lab: Functional Programming'),
    ( 4, 11, '2026-09-07', '08:30', '09:50', 'Lab: Functional Programming'),
    ( 6, 12, '2026-09-07', '08:30', '09:50', 'Lab: Functional Programming'),
    ( 1, 13, '2026-09-07', '10:10', '11:30', 'Lab: Functional Programming'),
    ( 2, 14, '2026-09-07', '10:10', '11:30', 'Lab: Functional Programming'),
    ( 3, 15, '2026-09-07', '10:10', '11:30', 'Lab: Functional Programming'),
    ( 1, 17, '2026-09-07', '15:00', '17:30', 'Course project'),
    ( 7, 18, '2026-09-07', '15:30', '18:00', 'Self-study'),
    (13, 17, '2026-09-08', '13:30', '16:00', 'Database homework'),
    (14, 19, '2026-09-08', '14:00', '17:00', 'Database homework'),
    (28, 13, '2026-09-08', '10:10', '11:30', 'Lab: Machine Learning'),
    (29, 14, '2026-09-08', '10:10', '11:30', 'Lab: Machine Learning'),
    (38,  8, '2026-09-09', '10:10', '11:30', 'Lab: Web Development'),
    (39,  9, '2026-09-09', '10:10', '11:30', 'Lab: Web Development'),
    (40, 10, '2026-09-09', '10:10', '11:30', 'Lab: Web Development'),
    ( 1, 20, '2026-09-09', '13:30', '15:00', 'Self-study'),
    ( 2,  8, '2026-09-09', '14:00', '18:30', 'Course project'),
    (38, 16, '2026-09-09', '14:00', '17:00', 'Self-study'),
    (50, 11, '2026-09-09', '13:30', '14:50', 'Lab: Information Security'),
    (51, 12, '2026-09-09', '13:30', '14:50', 'Lab: Information Security'),
    (13, 19, '2026-09-10', '10:10', '11:30', 'Practice: Computer Graphics'),
    (14, 20, '2026-09-10', '10:10', '11:30', 'Practice: Computer Graphics'),
    (28,  8, '2026-09-11', '10:00', '13:00', 'Machine learning experiments'),
    (29,  9, '2026-09-11', '09:30', '12:00', 'Self-study'),
    (50, 10, '2026-09-11', '09:00', '11:00', 'Network lab practice'),
    (38, 17, '2026-09-12', '10:00', '14:00', 'Course project'),
    (39, 18, '2026-09-12', '11:00', '15:30', 'Self-study'),
    ( 1,  8, '2026-09-14', '08:30', '09:50', 'Lab: Functional Programming'),
    ( 1, 19, '2026-09-14', '15:00', '18:00', 'Self-study'),
    ( 2,  9, '2026-09-14', '08:30', '09:50', 'Lab: Functional Programming');

INSERT INTO maintenance_log (workstation_id, log_date, description, performed_by) VALUES
    ( 5, '2026-09-05', 'Power supply failure, sent to repair',          1),
    (30, '2026-09-10', 'Monitor replaced',                              1),
    (30, '2026-09-15', 'Keyboard malfunction, waiting for spare parts', 1),
    (55, '2026-09-02', 'Decommissioned: outdated hardware',             1),
    (12, '2026-09-01', 'OS reinstalled, software updated',              NULL);
