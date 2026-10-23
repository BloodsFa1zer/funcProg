{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Console menu of the information system.
module Menu (run) where

import           Control.Exception (catch, throwIO)
import           Control.Monad (forM_, unless, void, when)
import           Data.Char (toLower)
import           Data.Int (Int32)
import           Data.Maybe (mapMaybe)
import           Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Time (Day, LocalTime, defaultTimeLocale, getZonedTime,
                            parseTimeM, zonedTimeToLocalTime)
import           Database.MySQL.Base
import           System.IO.Error (isEOFError)
import qualified System.IO.Streams as Streams
import           Text.Read (readMaybe)

import           Entity
import           Models

-- | Menu items in one place: number, title, action.
-- The menu text and the dispatch are both derived from this table.
menuItems :: [(String, (String, MySQLConn -> IO ()))]
menuItems =
  [ ("1",  ("Сервіси: список",                    listServices))
  , ("2",  ("Сервіси: додати",                    addService))
  , ("3",  ("Сервіси: редагувати",                editService))
  , ("4",  ("Сервіси: видалити",                  removeService))
  , ("5",  ("Сервіси: пошук за назвою/анотацією", searchServices))
  , ("6",  ("Автори: список / додати",            authorsMenu))
  , ("7",  ("Користувачі: список / додати",       usersMenu))
  , ("8",  ("Реєстрація користувача на сервіс",   registerUser))
  , ("9",  ("Зафіксувати використання сервісу",   recordUsage))
  , ("10", ("Статистика використання",            showStats))
  ]

run :: MySQLConn -> IO ()
run conn = do
  putStrLn ""
  putStrLn "=== ІС «Інформаційні сервіси мережі факультету» ==="
  forM_ menuItems $ \(n, (title, _)) ->
    putStrLn (replicate (3 - length n) ' ' ++ n ++ ". " ++ title)
  putStrLn "  0. Вихід"
  choice <- promptMenu
  case choice of
    "0" -> putStrLn "До побачення!"
    _   -> do
      case lookup choice menuItems of
        Just (_, action) -> safely (action conn)
        Nothing          -> putStrLn "Невідомий пункт меню."
      run conn

-- | EOF (Ctrl-D) at the main prompt means exit, not an error.
promptMenu :: IO String
promptMenu = prompt "Ваш вибір" `catch` \e ->
  if isEOFError e then putStrLn "" >> pure "0" else throwIO e

-- | Keep the menu loop alive when MySQL rejects an operation (duplicate
-- key, broken foreign key, ...). Only server errors are caught here:
-- Ctrl-C, EOF and genuine bugs still terminate the program.
safely :: IO () -> IO ()
safely act = act `catch` \(ERRException err) ->
  putStrLn ("Помилка БД (" ++ show (errCode err) ++ "): "
            ++ T.unpack (TE.decodeUtf8With lenientDecode (errMsg err)))

-- Input helpers ---------------------------------------------------------------

prompt :: String -> IO String
prompt label = putStr (label ++ ": ") >> getLine

promptText :: String -> IO T.Text
promptText label = T.pack <$> prompt label

-- | Edit with a current value: empty input keeps the old one.
promptDef :: String -> T.Text -> IO T.Text
promptDef label def = do
  s <- prompt (label ++ " [" ++ T.unpack def ++ "]")
  pure (if null s then def else T.pack s)

-- | Ids are parsed through Integer: GHC's Read Int32 silently wraps
-- out-of-range numbers, which would turn a typo into a valid id.
parseId :: String -> Maybe Int32
parseId s = do
  n <- readMaybe s
  if n >= 0 && n <= toInteger (maxBound :: Int32)
    then Just (fromInteger n)
    else Nothing

promptInt :: String -> IO Int32
promptInt label = do
  s <- prompt label
  case parseId s of
    Just n  -> pure n
    Nothing -> putStrLn "Потрібне ціле невідʼємне число." >> promptInt label

promptIntDef :: String -> Int32 -> IO Int32
promptIntDef label def = do
  s <- prompt (label ++ " [" ++ show def ++ "]")
  if null s
    then pure def
    else case parseId s of
           Just n  -> pure n
           Nothing -> putStrLn "Потрібне ціле невідʼємне число."
                   >> promptIntDef label def

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

promptDay :: String -> IO Day
promptDay label = do
  s <- prompt (label ++ " (РРРР-ММ-ДД)")
  case parseDay s of
    Just d  -> pure d
    Nothing -> putStrLn "Невірний формат дати." >> promptDay label

promptDayDef :: String -> Day -> IO Day
promptDayDef label def = do
  s <- prompt (label ++ " (РРРР-ММ-ДД) [" ++ fmtDay def ++ "]")
  if null s
    then pure def
    else case parseDay s of
           Just d  -> pure d
           Nothing -> putStrLn "Невірний формат дати." >> promptDayDef label def

askYes :: String -> IO Bool
askYes q = do
  ans <- prompt (q ++ " (y/n)")
  pure (map toLower ans == "y")

now :: IO LocalTime
now = zonedTimeToLocalTime <$> getZonedTime

printList :: Pretty a => String -> [a] -> IO ()
printList header xs = putStrLn header >> mapM_ (putStrLn . pretty) xs

-- Services --------------------------------------------------------------------

listServices :: MySQLConn -> IO ()
listServices conn = do
  services <- selectAll conn :: IO [Service]
  if null services
    then putStrLn "Сервісів немає."
    else forM_ services (printServiceFull conn)

-- | Print a service together with its kind and authors (data from three tables).
printServiceFull :: MySQLConn -> Service -> IO ()
printServiceFull conn s = do
  putStrLn (pretty s)
  mkind <- selectById conn (serviceKindId s) :: IO (Maybe Kind)
  forM_ mkind $ \k -> putStrLn ("      Вид: " ++ T.unpack (kindName k))
  links <- selectWhere conn "service_id = ?" [MySQLInt32 (serviceId s)]
             :: IO [ServiceAuthor]
  authors <- mapM (selectById conn . saAuthorId) links :: IO [Maybe Author]
  let names = [authorName a | Just a <- authors]
  unless (null names) $
    putStrLn ("      Автори: " ++ T.unpack (T.intercalate ", " names))

-- | Show the kinds and ask for an id until it names an existing kind,
-- so the user cannot lose a fully typed form to a foreign-key error.
promptKindId :: MySQLConn -> Maybe Int32 -> IO Int32
promptKindId conn mdef = do
  kinds <- selectAll conn :: IO [Kind]
  printList "Доступні види сервісів:" kinds
  let loop = do
        kid <- case mdef of
                 Just def -> promptIntDef "Id виду" def
                 Nothing  -> promptInt "Id виду"
        if kid `elem` map kindId kinds
          then pure kid
          else putStrLn "Такого виду немає." >> loop
  loop

addService :: MySQLConn -> IO ()
addService conn = do
  kid  <- promptKindId conn Nothing
  name <- promptText "Назва сервісу"
  ann  <- promptText "Анотація"
  ver  <- promptText "Версія"
  trm  <- promptText "Умови використання"
  dl   <- promptDay "Кінцевий термін використання"
  rls  <- promptText "Правила використання"
  sid <- insertEntity conn (Service 0 name ann kid ver trm dl rls)
  putStrLn ("Сервіс додано з id = " ++ show sid)
  linkAuthors conn sid

linkAuthors :: MySQLConn -> Int32 -> IO ()
linkAuthors conn sid = do
  authors <- selectAll conn :: IO [Author]
  printList "Автори (0 — завершити привʼязку):" authors
  let loop = do
        aid <- promptInt "Id автора"
        unless (aid == 0) $ do
          if aid `elem` map authorId authors
            -- a duplicate link only reports the DB error and the loop goes on
            then safely (void (insertEntity conn (ServiceAuthor 0 sid aid)))
            else putStrLn "Такого автора немає."
          loop
  loop

editService :: MySQLConn -> IO ()
editService conn = do
  sid <- promptInt "Id сервісу для редагування"
  ms <- selectById conn sid :: IO (Maybe Service)
  case ms of
    Nothing -> putStrLn "Сервіс не знайдено."
    Just s  -> do
      putStrLn "Порожній ввід залишає поточне значення."
      kid  <- promptKindId conn (Just (serviceKindId s))
      name <- promptDef "Назва" (serviceName s)
      ann  <- promptDef "Анотація" (serviceAnnotation s)
      ver  <- promptDef "Версія" (serviceVersion s)
      trm  <- promptDef "Умови" (serviceTerms s)
      dl   <- promptDayDef "Кінцевий термін" (serviceDeadline s)
      rls  <- promptDef "Правила" (serviceRules s)
      updateEntity conn s { serviceKindId = kid
                          , serviceName = name
                          , serviceAnnotation = ann
                          , serviceVersion = ver
                          , serviceTerms = trm
                          , serviceDeadline = dl
                          , serviceRules = rls }
      putStrLn "Сервіс оновлено."

removeService :: MySQLConn -> IO ()
removeService conn = do
  sid <- promptInt "Id сервісу для видалення"
  ms <- selectById conn sid :: IO (Maybe Service)
  case ms of
    Nothing -> putStrLn "Сервіс не знайдено."
    Just _  -> do
      -- dependent rows disappear via ON DELETE CASCADE (see schema.sql)
      deleteEntity (Proxy :: Proxy Service) conn sid
      putStrLn "Сервіс та повʼязані записи видалено."

-- | Escape LIKE metacharacters so the user searches for literal text.
escapeLike :: T.Text -> T.Text
escapeLike = T.concatMap esc
  where esc c | c `elem` ("%_\\" :: String) = T.pack ['\\', c]
              | otherwise                   = T.singleton c

searchServices :: MySQLConn -> IO ()
searchServices conn = do
  needle <- promptText "Фрагмент назви або анотації"
  let p = MySQLText (T.concat ["%", escapeLike needle, "%"])
  found <- selectWhere conn "name LIKE ? OR annotation LIKE ?" [p, p]
             :: IO [Service]
  if null found
    then putStrLn "Нічого не знайдено."
    else forM_ found (printServiceFull conn)

-- Catalog submenus --------------------------------------------------------------

-- | Generic "list all, then optionally add one" submenu: works for any
-- 'Entity' with a 'Pretty' instance; the builder prompts for the fields.
listAddMenu :: forall a. (Entity a, Pretty a)
            => MySQLConn -> String -> String -> IO a -> IO ()
listAddMenu conn header question build = do
  xs <- selectAll conn :: IO [a]
  printList header xs
  yes <- askYes question
  when yes $ do
    x <- build
    i <- insertEntity conn x
    putStrLn ("Додано з id = " ++ show i)

authorsMenu :: MySQLConn -> IO ()
authorsMenu conn = listAddMenu conn "Автори:" "Додати автора?" $ do
  name <- promptText "ПІБ"
  mail <- promptText "Email"
  dept <- promptText "Підрозділ"
  pure (Author 0 name mail dept)

usersMenu :: MySQLConn -> IO ()
usersMenu conn = listAddMenu conn "Користувачі:" "Додати користувача?" $ do
  lgn  <- promptText "Логін"
  name <- promptText "ПІБ"
  mail <- promptText "Email"
  t <- now
  pure (User 0 lgn name mail t)

-- Registrations and statistics ---------------------------------------------------

registerUser :: MySQLConn -> IO ()
registerUser conn = do
  uid <- promptInt "Id користувача"
  sid <- promptInt "Id сервісу"
  t <- now
  void (insertEntity conn (Registration 0 uid sid t))
  putStrLn "Користувача зареєстровано на сервіс."

recordUsage :: MySQLConn -> IO ()
recordUsage conn = do
  uid <- promptInt "Id користувача"
  sid <- promptInt "Id сервісу"
  act <- promptText "Дія (наприклад: вхід, перегляд, завантаження)"
  t <- now
  void (insertEntity conn (UsageStat 0 sid uid t act))
  putStrLn "Використання зафіксовано."

-- | Aggregate query joining three tables.
showStats :: MySQLConn -> IO ()
showStats conn = do
  putStrLn "Використання сервісів (за спаданням):"
  (_, is) <- query_ conn
    "SELECT s.name, COUNT(st.id) AS uses, COUNT(DISTINCT st.user_id) AS users \
    \FROM services s \
    \LEFT JOIN usage_stats st ON st.service_id = s.id \
    \GROUP BY s.id, s.name ORDER BY uses DESC"
  rows <- Streams.toList is
  forM_ (mapMaybe parseStat rows) $ \(name, uses, users) ->
    putStrLn ("  " ++ T.unpack name ++ ": звернень " ++ show uses
           ++ ", унікальних користувачів " ++ show users)
  where
    parseStat [n, u, c] = (,,) <$> vText n <*> vInt u <*> vInt c
    parseStat _         = Nothing
