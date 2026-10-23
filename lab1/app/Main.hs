module Main (main) where

import           Control.Exception (SomeException, catch, finally)
import           Database.MySQL.Base (MySQLConn, close)
import           GHC.IO.Encoding (setLocaleEncoding, utf8)
import           System.Exit (exitFailure)
import           System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

import           DB (getConn)
import           Menu (run)

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetBuffering stdout NoBuffering
  mconn <- (Just <$> getConn) `catch` explainConnect
  case mconn of
    Nothing   -> exitFailure
    Just conn -> run conn `finally` close conn

-- | Only the connection attempt gets the "is MySQL running?" diagnosis;
-- errors later in the session keep their own messages.
explainConnect :: SomeException -> IO (Maybe MySQLConn)
explainConnect e = do
  putStrLn "Не вдалося підключитися до MySQL."
  putStrLn "Перевірте, що сервер запущено і схему створено (див. README.md)."
  putStrLn ("Деталі: " ++ show e)
  pure Nothing
