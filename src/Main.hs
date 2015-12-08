import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent

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
      let
        mainLoop = do
          GLFW.pollEvents
          keyEscState <- GLFW.getKey win GLFW.Key'Escape
          case keyEscState of
            GLFW.KeyState'Released -> do
              threadDelay 20000
              GLFW.swapBuffers win
              mainLoop
            _ -> return ()
      mainLoop
    Nothing -> return ()


  GLFW.terminate
