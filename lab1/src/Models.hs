-- | Data types — one per DB table — and their
-- 'Entity' and 'Pretty' instances.
module Models where

import           Data.Int (Int32)
import qualified Data.Text as T
import           Data.Time (Day, LocalTime, defaultTimeLocale, formatTime)
import           Database.MySQL.Base
import           Entity

-- MySQL value parsers ---------------------------------------------------------

vInt :: MySQLValue -> Maybe Int32
vInt (MySQLInt32 n)  = Just n
vInt (MySQLInt32U n) = Just (fromIntegral n)
vInt (MySQLInt64 n)  = Just (fromIntegral n)
vInt (MySQLInt64U n) = Just (fromIntegral n)
vInt (MySQLInt16 n)  = Just (fromIntegral n)
vInt (MySQLInt8 n)   = Just (fromIntegral n)
vInt _               = Nothing

vText :: MySQLValue -> Maybe T.Text
vText (MySQLText t) = Just t
vText _             = Nothing

vDay :: MySQLValue -> Maybe Day
vDay (MySQLDate d) = Just d
vDay _             = Nothing

vTime :: MySQLValue -> Maybe LocalTime
vTime (MySQLDateTime t)  = Just t
vTime (MySQLTimeStamp t) = Just t
vTime _                  = Nothing

fmtDay :: Day -> String
fmtDay = formatTime defaultTimeLocale "%Y-%m-%d"

fmtTime :: LocalTime -> String
fmtTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

-- 1. Service kind --------------------------------------------------------------

data Kind = Kind
  { kindId   :: Int32
  , kindName :: T.Text
  } deriving (Show)

instance Entity Kind where
  tableName _ = "kinds"
  columns _   = ["name"]
  toRow k     = [MySQLText (kindName k)]
  fromRow [i, n] = Kind <$> vInt i <*> vText n
  fromRow _      = Nothing
  entityId    = kindId

instance Pretty Kind where
  pretty k = "  [" ++ show (kindId k) ++ "] " ++ T.unpack (kindName k)

-- 2. Author --------------------------------------------------------------------

data Author = Author
  { authorId    :: Int32
  , authorName  :: T.Text
  , authorEmail :: T.Text
  , authorDept  :: T.Text
  } deriving (Show)

instance Entity Author where
  tableName _ = "authors"
  columns _   = ["full_name", "email", "department"]
  toRow a     = [ MySQLText (authorName a)
                , MySQLText (authorEmail a)
                , MySQLText (authorDept a) ]
  fromRow [i, n, e, d] =
    Author <$> vInt i <*> vText n <*> vText e <*> vText d
  fromRow _ = Nothing
  entityId  = authorId

instance Pretty Author where
  pretty a = "  [" ++ show (authorId a) ++ "] " ++ T.unpack (authorName a)
          ++ " <" ++ T.unpack (authorEmail a) ++ ">, "
          ++ T.unpack (authorDept a)

-- 3. Service -------------------------------------------------------------------

data Service = Service
  { serviceId         :: Int32
  , serviceName       :: T.Text
  , serviceAnnotation :: T.Text
  , serviceKindId     :: Int32
  , serviceVersion    :: T.Text
  , serviceTerms      :: T.Text   -- usage terms
  , serviceDeadline   :: Day      -- end date of use
  , serviceRules      :: T.Text   -- usage rules
  } deriving (Show)

instance Entity Service where
  tableName _ = "services"
  columns _   = [ "name", "annotation", "kind_id", "version"
                , "terms", "deadline", "rules" ]
  toRow s     = [ MySQLText  (serviceName s)
                , MySQLText  (serviceAnnotation s)
                , MySQLInt32 (serviceKindId s)
                , MySQLText  (serviceVersion s)
                , MySQLText  (serviceTerms s)
                , MySQLDate  (serviceDeadline s)
                , MySQLText  (serviceRules s) ]
  fromRow [i, n, an, k, v, t, dl, r] =
    Service <$> vInt i <*> vText n <*> vText an <*> vInt k
            <*> vText v <*> vText t <*> vDay dl <*> vText r
  fromRow _ = Nothing
  entityId  = serviceId

instance Pretty Service where
  pretty s = "  [" ++ show (serviceId s) ++ "] "
          ++ T.unpack (serviceName s) ++ " (v" ++ T.unpack (serviceVersion s) ++ ")\n"
          ++ "      Анотація: " ++ T.unpack (serviceAnnotation s) ++ "\n"
          ++ "      Умови: " ++ T.unpack (serviceTerms s) ++ "\n"
          ++ "      Правила: " ++ T.unpack (serviceRules s) ++ "\n"
          ++ "      Кінцевий термін: " ++ fmtDay (serviceDeadline s)

-- 4. Service-author link ---------------------------------------------------------

data ServiceAuthor = ServiceAuthor
  { saId        :: Int32
  , saServiceId :: Int32
  , saAuthorId  :: Int32
  } deriving (Show)

instance Entity ServiceAuthor where
  tableName _ = "service_authors"
  columns _   = ["service_id", "author_id"]
  toRow sa    = [MySQLInt32 (saServiceId sa), MySQLInt32 (saAuthorId sa)]
  fromRow [i, s, a] = ServiceAuthor <$> vInt i <*> vInt s <*> vInt a
  fromRow _         = Nothing
  entityId    = saId

-- 5. User ------------------------------------------------------------------------

data User = User
  { userId           :: Int32
  , userLogin        :: T.Text
  , userName         :: T.Text
  , userEmail        :: T.Text
  , userRegisteredAt :: LocalTime
  } deriving (Show)

instance Entity User where
  tableName _ = "users"
  columns _   = ["login", "full_name", "email", "registered_at"]
  toRow u     = [ MySQLText (userLogin u)
                , MySQLText (userName u)
                , MySQLText (userEmail u)
                , MySQLDateTime (userRegisteredAt u) ]
  fromRow [i, l, n, e, r] =
    User <$> vInt i <*> vText l <*> vText n <*> vText e <*> vTime r
  fromRow _ = Nothing
  entityId  = userId

instance Pretty User where
  pretty u = "  [" ++ show (userId u) ++ "] " ++ T.unpack (userLogin u)
          ++ " — " ++ T.unpack (userName u)
          ++ " <" ++ T.unpack (userEmail u) ++ ">"
          ++ ", зареєстрований " ++ fmtTime (userRegisteredAt u)

-- 6. User registration on a service -----------------------------------------------

data Registration = Registration
  { regId        :: Int32
  , regUserId    :: Int32
  , regServiceId :: Int32
  , regAt        :: LocalTime
  } deriving (Show)

instance Entity Registration where
  tableName _ = "registrations"
  columns _   = ["user_id", "service_id", "registered_at"]
  toRow r     = [ MySQLInt32 (regUserId r)
                , MySQLInt32 (regServiceId r)
                , MySQLDateTime (regAt r) ]
  fromRow [i, u, s, t] =
    Registration <$> vInt i <*> vInt u <*> vInt s <*> vTime t
  fromRow _ = Nothing
  entityId  = regId

-- 7. Usage statistics record -------------------------------------------------------

data UsageStat = UsageStat
  { usId        :: Int32
  , usServiceId :: Int32
  , usUserId    :: Int32
  , usAt        :: LocalTime
  , usAction    :: T.Text
  } deriving (Show)

instance Entity UsageStat where
  tableName _ = "usage_stats"
  columns _   = ["service_id", "user_id", "used_at", "action"]
  toRow u     = [ MySQLInt32 (usServiceId u)
                , MySQLInt32 (usUserId u)
                , MySQLDateTime (usAt u)
                , MySQLText (usAction u) ]
  fromRow [i, s, u, t, a] =
    UsageStat <$> vInt i <*> vInt s <*> vInt u <*> vTime t <*> vText a
  fromRow _ = Nothing
  entityId  = usId

instance Pretty UsageStat where
  pretty u = "  [" ++ show (usId u) ++ "] сервіс #" ++ show (usServiceId u)
          ++ ", користувач #" ++ show (usUserId u)
          ++ ", " ++ fmtTime (usAt u)
          ++ " — " ++ T.unpack (usAction u)
