module Main where

import Prelude hiding ((.))
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import Control.Concurrent
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import Data.Array.IO
import Data.Array.Storable
import Foreign.Storable
import Foreign.Ptr

import Control.Wire

import EventWire
import Shader

gameWire :: (MonadIO m, HasTime t s) => Wire s e m EventState ()
gameWire = mkGen $ \sess evts -> do
  if M.member GLFW.Key'Escape (keyReleased evts) then
    return (Left undefined, gameWire)
  else
    return (Right (), gameWire)

adjustFramebufferSizeWire :: (MonadIO m, Monoid s) => Wire s e m EventState ()
adjustFramebufferSizeWire = mkGen_ $ \evts -> do
  case framebufferResize evts of
    Just (w, h) -> viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    Nothing -> return ()
  return (Right ())

mkRenderWire :: (Monoid s, MonadIO m) => IO (Wire s e m a ())
mkRenderWire = do
  rectVBuf <- genObjectName
  -- 0 3
  -- 1 2
  rectVertices <- newArray_ (0,3) :: IO (StorableArray Int (Vertex2 Float))
  let rectVerticesSize = toEnum $ 4 * sizeOf (undefined :: Vertex2 Float)

  (plainShader, posLoc, colorLoc) <- mkPlainShader

  let
    drawRect
      :: Color3 Float -- color
      -> Vertex2 Float -- (left, top) in [(-1,-1), (1,1)]
      -> Vertex2 Float -- (right, bottom) in [(-1,-1), (1,1)]
      -> IO ()
    drawRect color (Vertex2 x1 y1) (Vertex2 x2 y2) = do
      uniform colorLoc $= (fmap realToFrac color :: Color3 GLfloat)

      writeArray rectVertices 0 (Vertex2 x1 y1)
      writeArray rectVertices 1 (Vertex2 x1 y2)
      writeArray rectVertices 2 (Vertex2 x2 y2)
      writeArray rectVertices 3 (Vertex2 x2 y1)

      currentProgram $= Just plainShader

      vertexAttribArray posLoc $= Enabled

      bindBuffer ArrayBuffer $= Just rectVBuf
      withStorableArray rectVertices $ \ptr ->
        bufferData ArrayBuffer $= (rectVerticesSize, ptr, DynamicDraw)

      vertexAttribPointer posLoc $= (ToFloat, VertexArrayDescriptor 2 Float 0 (nullPtr :: Ptr Float))
      drawArrays Quads 0 4

      vertexAttribArray posLoc $= Disabled


  let
    renderWire = mkGen $ \_ _ -> do
      --liftIO $ putStrLn $ "draw!"
      liftIO $ do
        drawRect (Color3 0 0 0) (Vertex2 (-1) (-1)) (Vertex2 1 1)
        drawRect (Color3 0 0 1) (Vertex2 (-0.5) (-0.5)) (Vertex2 0.5 0.5)
        flush
      return (Right (), renderWire)

  return renderWire

main = do
  ver <- GLFW.getVersion
  putStrLn $ "GLFW version: " ++ show ver

  initRes <- GLFW.init
  putStrLn $ "GLFW init: " ++ show initRes

  mWin <- GLFW.createWindow 100 100 "Zoom Craft" Nothing Nothing
  putStrLn $ "createWindow " ++ show mWin

  GLFW.makeContextCurrent mWin

  mCxt <- GLFW.getCurrentContext
  putStrLn $ "currentContext = " ++ show mCxt

  case mWin of
    Just win -> do
      eventWire <- mkEventWire win
      renderWire <- mkRenderWire
      let
        mainWire = eventWire >>> adjustFramebufferSizeWire &&& gameWire >>> first renderWire

        mainLoop wire sess = do

          (ds, sess') <- stepSession sess

          stepWire wire ds (Right ()) >>= \case
            (Left _, _) -> return ()
            (Right _, nextWire) -> do
              GLFW.swapBuffers win
              threadDelay 20000
              mainLoop nextWire sess'

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
