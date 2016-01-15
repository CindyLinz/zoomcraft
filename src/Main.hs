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
import TextureLoader

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
  cullFace $= Just Back
  clearColor $= Color4 0.2 0.2 0.2 1

  rectVBuf <- genObjectName
  -- 0 1
  -- 3 2
  rectVertices <- newArray_ (0,3) :: IO (StorableArray Int (Vertex2 Float))
  let rectVerticesSize = toEnum $ 4 * sizeOf (undefined :: Vertex2 Float)

  drawRect <- do
    (shader, posLoc, colorLoc) <- mkPlainShader

    let
      drawRect
        :: Color3 Float -- color
        -> Vertex2 Float -- (left, top) in [(-1,-1), (1,1)]
        -> Vertex2 Float -- (right, bottom) in [(-1,-1), (1,1)]
        -> IO ()
      drawRect color (Vertex2 x1 y1) (Vertex2 x2 y2) = do
        currentProgram $= Just shader

        uniform colorLoc $= (fmap realToFrac color :: Color3 GLfloat)

        writeArray rectVertices 0 (Vertex2 x1 y1)
        writeArray rectVertices 1 (Vertex2 x2 y1)
        writeArray rectVertices 2 (Vertex2 x2 y2)
        writeArray rectVertices 3 (Vertex2 x1 y2)

        vertexAttribArray posLoc $= Enabled

        bindBuffer ArrayBuffer $= Just rectVBuf
        withStorableArray rectVertices $ \ptr ->
          bufferData ArrayBuffer $= (rectVerticesSize, ptr, DynamicDraw)

        vertexAttribPointer posLoc $= (ToFloat, VertexArrayDescriptor 2 Float 0 (nullPtr :: Ptr Float))
        drawArrays Quads 0 4

        vertexAttribArray posLoc $= Disabled
    return drawRect

  drawImage <- do
    rectUVBuf <- genObjectName
    -- 0 1
    -- 3 2

    rectUVs <- newListArray (0,3)
      [ Vertex2 0 1
      , Vertex2 1 1
      , Vertex2 1 0
      , Vertex2 0 0
      ] :: IO (StorableArray Int (Vertex2 Float))
    let rectUVSize = toEnum $ 4 * sizeOf (undefined :: Vertex2 Float)

    bindBuffer ArrayBuffer $= Just rectUVBuf
    withStorableArray rectUVs $ \ptr ->
      bufferData ArrayBuffer $= (rectUVSize, ptr, StaticDraw)

    texObj <- genObjectName

    (shader, posLoc, uvLoc, texLoc) <- mkImageShader

    let
      drawImage
        :: TextureImage () -- image
        -> Vertex2 Float -- (left, top) in [(-1,-1), (1,1)]
        -> Vertex2 Float -- (right, bottom) in [(-1,-1), (1,1)]
        -> IO ()
      drawImage texImg (Vertex2 x1 y1) (Vertex2 x2 y2) = do
        texImg $ \intFmt size dat -> do
          currentProgram $= Just shader

          texture Texture2D $= Enabled
          --textureFunction $= Combine
          textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
          --textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
          textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
          textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
          activeTexture $= TextureUnit 0
          textureBinding Texture2D $= Just texObj

          texImage2D Texture2D NoProxy 0 intFmt size 0 dat
--            do
--              arr <- newListArray (0,3)
--                [ Color3 1 0 0
--                , Color3 0 1 0
--                , Color3 0 0 1
--                , Color3 1 1 0
--                ] :: IO (StorableArray Int (Color3 Float))
--              withStorableArray arr $ \ptr ->
--                texImage2D Texture2D NoProxy 0 RGB' (TextureSize2D 2 2) 0 (PixelData RGB Float ptr)
          uniform texLoc $= TextureUnit 0

          writeArray rectVertices 0 (Vertex2 x1 y1)
          writeArray rectVertices 1 (Vertex2 x2 y1)
          writeArray rectVertices 2 (Vertex2 x2 y2)
          writeArray rectVertices 3 (Vertex2 x1 y2)

          vertexAttribArray posLoc $= Enabled
          bindBuffer ArrayBuffer $= Just rectVBuf
          withStorableArray rectVertices $ \ptr ->
            bufferData ArrayBuffer $= (rectVerticesSize, ptr, DynamicDraw)
          vertexAttribPointer posLoc $= (ToFloat, VertexArrayDescriptor 2 Float 0 (nullPtr :: Ptr Float))

          vertexAttribArray uvLoc $= Enabled
          bindBuffer ArrayBuffer $= Just rectUVBuf
          --withStorableArray rectUVs $ \ptr ->
          --  bufferData ArrayBuffer $= (rectUVSize, ptr, DynamicDraw)
          vertexAttribPointer uvLoc $= (ToFloat, VertexArrayDescriptor 2 Float 0 (nullPtr :: Ptr Float))

          drawArrays Quads 0 4

          vertexAttribArray uvLoc $= Disabled
          vertexAttribArray posLoc $= Disabled
          texture Texture2D $= Disabled
    return drawImage

  img1 <- sloppyLoadTexture "ERIKA03.png"
  img2 <- sloppyLoadTexture "ERIKA03.jpg"

  let
    renderWire = mkGen $ \_ _ -> do
      --liftIO $ putStrLn $ "draw!"
      liftIO $ do
        clear [ColorBuffer, AccumBuffer, StencilBuffer, DepthBuffer]
        --drawRect (Color3 0 0 0) (Vertex2 (-1) (-1)) (Vertex2 1 1)
        drawRect (Color3 0 0 1) (Vertex2 (-0.5) (-0.5)) (Vertex2 0.5 0.5)
        drawRect (Color3 0 1 0) (Vertex2 (-0.4) (-0.4)) (Vertex2 0.4 0.4)
        drawImage img1 (Vertex2 (-0.3) (-0.3)) (Vertex2 0.0 0.0)
        drawImage img2 (Vertex2 (0.0) (0.0)) (Vertex2 0.3 0.3)
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
