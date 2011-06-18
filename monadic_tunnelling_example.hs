-- from http://www.haskell.org/pipermail/haskell-cafe/2007-July/028501.html
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error
import Control.Exception

import Data.Typeable

import Data.IORef
-- example small action in the custom monad
stateioact :: StateT Int IO ()
stateioact = do
  x <- get
  liftIO $ putStrLn ("stateioact called, with state as "++
                     show x)
  put (x+1)


-- Main action has type IO () as standard
-- This example main action just defers to another action
-- which is written in a custom monad

main :: IO ()
main =  do
  putStrLn "main starting"
  evalStateT mainAction 42
  putStrLn "main exiting"


-- mainAction is written in the custom monad
mainAction :: StateT Int IO ()
mainAction = do stateioact
                embedIO $ \x -> usesCB (makeCallback stateioact x)
                stateioact

-- a 'library function' in the IO monad, which has a callback as one
-- of its parameters. The library function has no knowledge of the
-- custom monad being used by the main action here
usesCB :: IO () -> IO ()
usesCB f = do putStrLn "usesCB starting"
              f
              putStrLn "usesCB middle"
              f
              putStrLn "usesCB exiting"

-- the glue which makes it work:
embedIO :: (IORef s -> IO a) -> StateT s IO a
embedIO a = do s <- get
               x <-  liftIO $ newIORef s
               r <-  liftIO $ a x
               s' <- liftIO $ readIORef x
               put s'
               return r

makeCallback :: StateT s IO a -> IORef s -> IO a
makeCallback act x = do s <- readIORef x
                        (res,s') <- runStateT act s
                        writeIORef x s'
                        return res




