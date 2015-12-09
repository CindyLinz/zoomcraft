module EventWire where

import Prelude hiding ((.))
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Graphics.UI.GLFW as GLFW
import Control.Wire

data EventState = EventState
  { keyPressed :: M.Map GLFW.Key Int
  , keyReleased :: M.Map GLFW.Key Int
  , keyPressing :: S.Set GLFW.Key
  }

initEventState = EventState
  { keyPressed = M.empty
  , keyReleased = M.empty
  , keyPressing = S.empty
  }

type EventStateHandle = IORef EventState

keyCallback :: EventStateHandle -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback hd _win key _scanCode action _mods = modifyIORef' hd $ \state -> case action of
  GLFW.KeyState'Pressed -> state
    { keyPressing = S.insert key (keyPressing state)
    , keyPressed = M.insertWith (+) key 1 (keyPressed state)
    }
  GLFW.KeyState'Released -> state
    { keyPressing = S.delete key (keyPressing state)
    , keyReleased = M.insertWith (+) key 1 (keyPressed state)
    }
  _ -> state

mkEventWire :: MonadIO m => GLFW.Window -> m (Wire s e m a EventState)
mkEventWire win = liftIO $ do
  hd <- newIORef initEventState
  GLFW.setKeyCallback win (Just $ keyCallback hd)

  return $ mkGen_ $ const $ liftIO $ do
    modifyIORef' hd $ \state -> state
      { keyPressed = M.empty
      , keyReleased = M.empty
      }

    GLFW.pollEvents

    state <- readIORef hd
    return (Right state)
