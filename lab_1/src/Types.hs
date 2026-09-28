-- | Domain types: one record per database table plus enumerations for ENUM columns.
module Types
  ( Role (..)
  , WeekDay (..)
  , WsStatus (..)
  , LessonType (..)
  , Classroom (..)
  , Workstation (..)
  , User (..)
  , Teacher (..)
  , Discipline (..)
  , Lesson (..)
  , FreeAccess (..)
  , Session (..)
  , Maintenance (..)
  ) where

import Data.Time (Day, TimeOfDay)

data Role = Student | TeacherRole | Admin
  deriving (Show, Eq, Enum, Bounded)

data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show, Eq, Ord, Enum, Bounded)

data WsStatus = Working | Repair | Off
  deriving (Show, Eq, Enum, Bounded)

data LessonType = Lecture | Lab | Practice
  deriving (Show, Eq, Enum, Bounded)

-- | Table `classrooms`: a display class.
data Classroom = Classroom
  { crId          :: Int
  , crRoom        :: String
  , crBuilding    :: String
  , crFloor       :: Int
  , crCapacity    :: Int
  , crOpen        :: TimeOfDay
  , crClose       :: TimeOfDay
  , crDescription :: Maybe String
  }

-- | Table `workstations`: an individual seat with a computer.
data Workstation = Workstation
  { wsId          :: Int
  , wsClassroomId :: Int
  , wsSeat        :: Int
  , wsInventory   :: String
  , wsCpu         :: String
  , wsRamGb       :: Int
  , wsOs          :: String
  , wsStatus      :: WsStatus
  }

-- | Table `users`.
data User = User
  { usrId       :: Int
  , usrFullName :: String
  , usrRole     :: Role
  , usrGroup    :: Maybe String
  , usrEmail    :: String
  , usrPhone    :: Maybe String
  }

-- | Table `teachers`: extra data for users with role teacher.
data Teacher = Teacher
  { tchId         :: Int
  , tchUserId     :: Int
  , tchDepartment :: String
  , tchPosition   :: String
  }

-- | Table `disciplines`.
data Discipline = Discipline
  { dsId    :: Int
  , dsName  :: String
  , dsHours :: Int
  }

-- | Table `schedule`: a planned weekly lesson.
data Lesson = Lesson
  { lsId           :: Int
  , lsClassroomId  :: Int
  , lsTeacherId    :: Int
  , lsDisciplineId :: Int
  , lsGroup        :: String
  , lsDay          :: WeekDay
  , lsStart        :: TimeOfDay
  , lsEnd          :: TimeOfDay
  , lsType         :: LessonType
  , lsValidFrom    :: Day
  , lsValidTo      :: Day
  }

-- | Table `free_access`: weekly window of free access to a class.
data FreeAccess = FreeAccess
  { faId          :: Int
  , faClassroomId :: Int
  , faDay         :: WeekDay
  , faStart       :: TimeOfDay
  , faEnd         :: TimeOfDay
  , faDutyUserId  :: Maybe Int
  , faNote        :: Maybe String
  }

-- | Table `workstation_sessions`: actual usage of a workstation.
data Session = Session
  { ssId            :: Int
  , ssWorkstationId :: Int
  , ssUserId        :: Int
  , ssDate          :: Day
  , ssStart         :: TimeOfDay
  , ssEnd           :: TimeOfDay
  , ssPurpose       :: Maybe String
  }

-- | Table `maintenance_log`.
data Maintenance = Maintenance
  { mtId            :: Int
  , mtWorkstationId :: Int
  , mtDate          :: Day
  , mtDescription   :: String
  , mtPerformedBy   :: Maybe Int
  }
