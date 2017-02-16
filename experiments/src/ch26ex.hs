import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Identity

rDec :: Num a => Reader a a
rDec = reader ( flip (-) 1 )

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
                                putStrLn $ "Hi: " ++ show a
                                return (a + 1)

sPrintIntAccum :: (Num a, Show a) => StateT a IO String
sPrintIntAccum = StateT $ \a -> do
                                  let s = show a
                                  putStrLn $ "Hi: " ++ s
                                  return (s, a + 1)

