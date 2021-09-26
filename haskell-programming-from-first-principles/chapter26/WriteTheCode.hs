import Control.Monad.Reader (Reader, MonadTrans (lift))
import Control.Monad.Identity (Identity)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad

-- Chapter Exercises

-- 1
rDec :: Num a => Reader a a
rDec = do
  a <- ask
  return $ a - 1

-- 2
rDec' :: Num a => Reader a a
rDec' = ask >>= return . flip (-) 1

-- 3
rShow :: Show a => ReaderT a Identity String
rShow = do
  a <- ask
  return $ show a

-- 4
rShow' :: Show a => ReaderT a Identity String
rShow' = ask >>= return . show

-- 5
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  a <- ask
  liftIO $ putStr "Hi: "
  liftIO $ print a
  return $ a + 1

-- 6
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
  a <- get
  liftIO $ putStr "Hi: "
  liftIO $ print a
  put (a + 1)
  return $ show a

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  liftIO $ putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e ->
      putStrLn
      ("Good, was very excite: " ++ e)


