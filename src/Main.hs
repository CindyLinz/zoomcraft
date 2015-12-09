module Main where

import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M

import Control.Wire

import EventWire

gameWire :: (MonadIO m, HasTime t s) => Wire s e m EventState ()
gameWire = mkGen $ \sess evts -> do
  if M.member GLFW.Key'Escape (keyReleased evts) then
    return (Left undefined, gameWire)
  else
    return (Right (), gameWire)

main = do
  ver <- GLFW.getVersion
  putStrLn $ "GLFW version: " ++ show ver

  initRes <- GLFW.init
  putStrLn $ "GLFW init: " ++ show initRes

  mWin <- GLFW.createWindow 1024 768 "Zoom Craft" Nothing Nothing
  putStrLn $ "createWindow " ++ show mWin

  GLFW.makeContextCurrent mWin

  mCxt <- GLFW.getCurrentContext
  putStrLn $ "currentContext = " ++ show mCxt

  case mWin of
    Just win -> do
      eventWire <- mkEventWire win
      let
        mainWire = eventWire >>> gameWire

        mainLoop wire sess = do
          threadDelay 20000

          (ds, sess') <- stepSession sess

          stepWire wire ds (Right ()) >>= \case
            (Left _, _) -> return ()
            (Right _, nextWire) -> mainLoop nextWire sess'

--          keyEscState <- GLFW.getKey win GLFW.Key'Escape
--          case keyEscState of
--            GLFW.KeyState'Released -> do
--              threadDelay 20000
--              GLFW.swapBuffers win
--              mainLoop
--            _ -> return ()
      mainLoop mainWire clockSession_
    Nothing -> return ()


  GLFW.terminate
