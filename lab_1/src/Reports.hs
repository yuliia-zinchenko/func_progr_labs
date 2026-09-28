{-# LANGUAGE OverloadedStrings #-}

-- | Queries and reports: schedules, free workstations, load of classes and workstations.
--
-- A report is plain data: a list of parameters and a function that turns parameter values
-- into result tables. The console ("Menu") and the web interface ("Web") only differ in how
-- they ask for parameters and show the tables.
module Reports
  ( Report (..)
  , ReportParam (..)
  , ParamKind (..)
  , ReportTable (..)
  , Args
  , reports
  , lookupReport
  ) where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Time (Day, TimeOfDay, dayOfWeek)
import qualified Data.Time as Time
import Database.MySQL.Simple
import Database.MySQL.Simple.Param (render)
import Database.MySQL.Simple.QueryParams (QueryParams)
import Database.MySQL.Simple.QueryResults (QueryResults (..))
import Input
import Instances.Enums ()
import Text.Printf (printf)
import Types

-- | Parameter values by name, as typed by the user.
type Args = Map.Map String String

data ParamKind
  = -- | id of a row of the given table
    PRef String
  | PDate
  | PTime
  | -- | one of the values returned by the query
    PChoice (Connection -> IO [String])

data ReportParam = ReportParam
  { rpName    :: String
  , rpLabel   :: String
  , rpKind    :: ParamKind
  , rpDefault :: String
  }

data ReportTable = ReportTable
  { rtTitle   :: String
  , rtHeaders :: [String]
  , rtRows    :: [[String]]
  , -- | column with a percentage that is drawn as a bar
    rtBar     :: Maybe Int
  , -- | text shown when there are no rows
    rtEmpty   :: String
  }

data Report = Report
  { reportKey    :: String
  , reportTitle  :: String
  , reportParams :: [ReportParam]
  , reportRun    :: Connection -> Args -> IO [ReportTable]
  }

reports :: [Report]
reports =
  [ Report "class-schedule" "Weekly schedule of a class" [classParam] classSchedule
  , Report "teacher-schedule" "Weekly schedule of a teacher" [teacherParam] teacherSchedule
  , Report "group-schedule" "Weekly schedule of a study group" [groupParam] groupSchedule
  , Report "free-access" "Free access time of all classes" [] freeAccessTable
  , Report "free-workstations" "Free workstations at a given date and time" [dateParam, timeParam] freeWorkstations
  , Report "classroom-load" "Load of display classes (per week)" [] classroomLoad
  , Report "workstation-load" "Load of workstations (all time)" [] workstationLoad
  , Report "usage-period" "Usage of workstations for a period" [fromParam, toParam] usageForPeriod
  , Report "user-activity" "Activity of users for a period" [fromParam, toParam] userActivity
  , Report "workstation-state" "Technical state of workstations by class" [] workstationState
  , Report "schedule-conflicts" "Check schedule for conflicts" [] scheduleConflicts
  ]
  where
    classParam = ReportParam "classroom_id" "Classroom" (PRef "classrooms") ""
    teacherParam = ReportParam "teacher_id" "Teacher" (PRef "teachers") ""
    groupParam = ReportParam "group" "Study group" (PChoice groups) ""
    dateParam = ReportParam "date" "Date" PDate "2026-09-09"
    timeParam = ReportParam "time" "Time" PTime "14:00"
    fromParam = ReportParam "from" "From date" PDate "2026-09-01"
    toParam = ReportParam "to" "To date" PDate "2026-09-30"
    groups conn = map fromOnly <$> query_ conn "SELECT DISTINCT study_group FROM schedule ORDER BY study_group"

lookupReport :: String -> Maybe Report
lookupReport key = find ((== key) . reportKey) reports

-- | Typed value of a parameter.
arg :: FieldInput a => Args -> String -> IO a
arg args name = case Map.lookup name args >>= parseField of
  Just v -> pure v
  Nothing -> throwIO (UserAbort ("Parameter '" ++ name ++ "' is missing or invalid."))

table :: String -> [String] -> [[String]] -> ReportTable
table title hs rows = ReportTable title hs rows Nothing "No records."

-- | Any result row as a list of strings (MySQL sends values as text).
newtype TextRow = TextRow [String]

instance QueryResults TextRow where
  convertResults _ vs = TextRow (map (maybe "-" decode) vs)
    where
      decode :: ByteString -> String
      decode = T.unpack . TE.decodeUtf8With TE.lenientDecode

textQuery :: QueryParams q => Connection -> Query -> q -> IO [[String]]
textQuery conn q ps = map (\(TextRow r) -> r) <$> query conn q ps

lessonHeaders :: [String]
lessonHeaders = ["Day", "Start", "End", "Discipline", "Type", "Group", "Class", "Teacher", "Period"]

lessonSelect :: Query
lessonSelect =
  "SELECT s.week_day, TIME_FORMAT(s.start_time, '%H:%i'), TIME_FORMAT(s.end_time, '%H:%i'), \
  \d.name, s.lesson_type, s.study_group, c.room_number, u.full_name, \
  \CONCAT(s.valid_from, ' - ', s.valid_to) \
  \FROM schedule s \
  \JOIN classrooms c ON c.id = s.classroom_id \
  \JOIN disciplines d ON d.id = s.discipline_id \
  \JOIN teachers t ON t.id = s.teacher_id \
  \JOIN users u ON u.id = t.user_id "

lessons :: Connection -> Query -> Int -> IO ReportTable
lessons conn cond value =
  table "Planned lessons" lessonHeaders
    <$> textQuery conn (lessonSelect <> cond <> " ORDER BY s.week_day, s.start_time") (Only value)

classSchedule :: Connection -> Args -> IO [ReportTable]
classSchedule conn args = do
  cid <- arg args "classroom_id" :: IO Int
  planned <- lessons conn "WHERE s.classroom_id = ?" cid
  free <-
    textQuery
      conn
      "SELECT f.week_day, TIME_FORMAT(f.start_time, '%H:%i'), TIME_FORMAT(f.end_time, '%H:%i'), u.full_name, f.note \
      \FROM free_access f LEFT JOIN users u ON u.id = f.duty_user_id \
      \WHERE f.classroom_id = ? ORDER BY f.week_day, f.start_time"
      (Only cid)
  pure [planned, table "Free access" ["Day", "Start", "End", "On duty", "Note"] free]

teacherSchedule :: Connection -> Args -> IO [ReportTable]
teacherSchedule conn args = do
  tid <- arg args "teacher_id" :: IO Int
  (: []) <$> lessons conn "WHERE s.teacher_id = ?" tid

groupSchedule :: Connection -> Args -> IO [ReportTable]
groupSchedule conn args = do
  grp <- arg args "group" :: IO String
  rows <- textQuery conn (lessonSelect <> "WHERE s.study_group = ? ORDER BY s.week_day, s.start_time") (Only grp)
  pure [table "Planned lessons" lessonHeaders rows]

freeAccessTable :: Connection -> Args -> IO [ReportTable]
freeAccessTable conn _ = do
  rows <-
    textQuery
      conn
      "SELECT c.room_number, c.building, f.week_day, TIME_FORMAT(f.start_time, '%H:%i'), \
      \TIME_FORMAT(f.end_time, '%H:%i'), u.full_name, f.note \
      \FROM free_access f JOIN classrooms c ON c.id = f.classroom_id \
      \LEFT JOIN users u ON u.id = f.duty_user_id \
      \ORDER BY f.week_day, f.start_time, c.room_number"
      ()
  pure [table "" ["Class", "Building", "Day", "Start", "End", "On duty", "Note"] rows]

toWeekDay :: Day -> WeekDay
toWeekDay d = case dayOfWeek d of
  Time.Monday -> Mon
  Time.Tuesday -> Tue
  Time.Wednesday -> Wed
  Time.Thursday -> Thu
  Time.Friday -> Fri
  Time.Saturday -> Sat
  Time.Sunday -> Sun

-- | Working workstations in open classes that have no lesson and no session at the moment.
freeWorkstations :: Connection -> Args -> IO [ReportTable]
freeWorkstations conn args = do
  day <- arg args "date" :: IO Day
  time <- arg args "time" :: IO TimeOfDay
  let wd = toWeekDay day
  rows <-
    textQuery
      conn
      "SELECT c.room_number, w.seat_number, w.inventory_number, w.cpu, w.ram_gb, w.os, \
      \IF(EXISTS (SELECT 1 FROM free_access f WHERE f.classroom_id = c.id AND f.week_day = ? \
      \           AND f.start_time <= ? AND f.end_time > ?), 'free access', 'open hours') \
      \FROM workstations w JOIN classrooms c ON c.id = w.classroom_id \
      \WHERE w.status = 'working' AND c.open_time <= ? AND c.close_time > ? \
      \AND NOT EXISTS (SELECT 1 FROM schedule s WHERE s.classroom_id = c.id AND s.week_day = ? \
      \                AND s.start_time <= ? AND s.end_time > ? AND ? BETWEEN s.valid_from AND s.valid_to) \
      \AND NOT EXISTS (SELECT 1 FROM workstation_sessions ss WHERE ss.workstation_id = w.id \
      \                AND ss.session_date = ? AND ss.start_time <= ? AND ss.end_time > ?) \
      \ORDER BY c.room_number, w.seat_number"
      [ render wd, render time, render time
      , render time, render time
      , render wd, render time, render time, render day
      , render day, render time, render time
      ]
  let perClass = Map.fromListWith (+) [(room, 1 :: Int) | (room : _) <- rows]
      title = "Free workstations on " ++ showField wd ++ ", " ++ showField day ++ " at " ++ showField time
  pure
    [ table "Free workstations by class" ["Class", "Free workstations"] [[room, show n] | (room, n) <- Map.toList perClass]
    , table title ["Class", "Seat", "Inventory No", "CPU", "RAM, GB", "OS", "Access"] rows
    ]

-- | Load from view v_classroom_load; total occupancy is computed here.
classroomLoad :: Connection -> Args -> IO [ReportTable]
classroomLoad conn _ = do
  rows <-
    query_
      conn
      "SELECT room_number, open_hours, lesson_hours, free_access_hours, load_percent \
      \FROM v_classroom_load ORDER BY load_percent DESC"
  pure
    [ (table "" ["Class", "Open h/week", "Lessons h", "Free access h", "Lessons %", "Busy total %", "Lessons load"]
        [ [room, fmt open, fmt lesson, fmt free, fmt pct, fmt busy, fmt pct]
        | (room, open, lesson, free, pct) <- rows :: [(String, Double, Double, Double, Double)]
        , let busy = if open > 0 then (lesson + free) * 100 / open else 0
        ])
        { rtBar = Just 6 }
    ]
  where
    fmt = printf "%.1f" :: Double -> String

workstationLoad :: Connection -> Args -> IO [ReportTable]
workstationLoad conn _ = do
  rows <-
    textQuery
      conn
      "SELECT id, room_number, seat_number, inventory_number, status, sessions, used_hours, last_used \
      \FROM v_workstation_load ORDER BY used_hours DESC, room_number, seat_number"
      ()
  pure [table "" ["ID", "Class", "Seat", "Inventory No", "Status", "Sessions", "Hours", "Last used"] rows]

usageForPeriod :: Connection -> Args -> IO [ReportTable]
usageForPeriod conn args = do
  from <- arg args "from" :: IO Day
  to <- arg args "to" :: IO Day
  rows <-
    query
      conn
      "SELECT c.room_number, w.seat_number, w.inventory_number, COUNT(*), \
      \ROUND(SUM(TIME_TO_SEC(ss.end_time) - TIME_TO_SEC(ss.start_time)) / 3600, 1) \
      \FROM workstation_sessions ss \
      \JOIN workstations w ON w.id = ss.workstation_id \
      \JOIN classrooms c ON c.id = w.classroom_id \
      \WHERE ss.session_date BETWEEN ? AND ? \
      \GROUP BY c.room_number, w.seat_number, w.inventory_number \
      \ORDER BY c.room_number, w.seat_number"
      (from, to)
  let typed = rows :: [(String, Int, String, Int, Double)]
      totals = Map.fromListWith (\(a, b) (c, d) -> (a + c, b + d)) [(room, (n, h)) | (room, _, _, n, h) <- typed]
  pure
    [ table "Totals by class" ["Class", "Sessions", "Hours"] [[room, show n, printf "%.1f" h] | (room, (n, h)) <- Map.toList totals]
    , table "By workstation" ["Class", "Seat", "Inventory No", "Sessions", "Hours"] [[room, show seat, inv, show n, show h] | (room, seat, inv, n, h) <- typed]
    ]

userActivity :: Connection -> Args -> IO [ReportTable]
userActivity conn args = do
  from <- arg args "from" :: IO Day
  to <- arg args "to" :: IO Day
  rows <-
    textQuery
      conn
      "SELECT u.full_name, u.role, u.study_group, COUNT(*), \
      \ROUND(SUM(TIME_TO_SEC(ss.end_time) - TIME_TO_SEC(ss.start_time)) / 3600, 1), \
      \GROUP_CONCAT(DISTINCT c.room_number ORDER BY c.room_number SEPARATOR ', ') \
      \FROM workstation_sessions ss \
      \JOIN users u ON u.id = ss.user_id \
      \JOIN workstations w ON w.id = ss.workstation_id \
      \JOIN classrooms c ON c.id = w.classroom_id \
      \WHERE ss.session_date BETWEEN ? AND ? \
      \GROUP BY u.id, u.full_name, u.role, u.study_group \
      \ORDER BY 5 DESC"
      (from, to)
  pure [table "" ["User", "Role", "Group", "Sessions", "Hours", "Classes used"] rows]

workstationState :: Connection -> Args -> IO [ReportTable]
workstationState conn _ = do
  rows <-
    textQuery
      conn
      "SELECT c.room_number, COUNT(w.id), \
      \SUM(w.status = 'working'), SUM(w.status = 'repair'), SUM(w.status = 'off'), \
      \(SELECT MAX(m.log_date) FROM maintenance_log m JOIN workstations w2 ON w2.id = m.workstation_id \
      \ WHERE w2.classroom_id = c.id) \
      \FROM classrooms c LEFT JOIN workstations w ON w.classroom_id = c.id \
      \GROUP BY c.id, c.room_number ORDER BY c.room_number"
      ()
  pure [table "" ["Class", "Total", "Working", "Repair", "Off", "Last maintenance"] rows]

-- | Pairs of lessons that overlap in class, teacher or group (e.g. after direct edits in SQL).
scheduleConflicts :: Connection -> Args -> IO [ReportTable]
scheduleConflicts conn _ = do
  rows <-
    textQuery
      conn
      "SELECT a.id, b.id, a.week_day, \
      \CONCAT(TIME_FORMAT(a.start_time, '%H:%i'), '-', TIME_FORMAT(a.end_time, '%H:%i')), \
      \CONCAT(TIME_FORMAT(b.start_time, '%H:%i'), '-', TIME_FORMAT(b.end_time, '%H:%i')), \
      \CONCAT_WS(', ', IF(a.classroom_id = b.classroom_id, 'same class', NULL), \
      \                IF(a.teacher_id = b.teacher_id, 'same teacher', NULL), \
      \                IF(a.study_group = b.study_group, 'same group', NULL)) \
      \FROM schedule a JOIN schedule b ON a.id < b.id AND a.week_day = b.week_day \
      \AND a.start_time < b.end_time AND b.start_time < a.end_time \
      \AND a.valid_from <= b.valid_to AND b.valid_from <= a.valid_to \
      \AND (a.classroom_id = b.classroom_id OR a.teacher_id = b.teacher_id OR a.study_group = b.study_group) \
      \ORDER BY a.week_day, a.start_time"
      ()
  pure [(table "" ["Lesson A", "Lesson B", "Day", "Time A", "Time B", "Reason"] rows) {rtEmpty = "No conflicts found."}]
