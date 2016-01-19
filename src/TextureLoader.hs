module TextureLoader where

import System.IO
import qualified Data.Vector.Storable as V
import Data.Array.Storable
import Foreign.Marshal.Alloc
import Foreign.Storable

import Graphics.Rendering.OpenGL.GL
import Codec.Picture
import Codec.Picture.Types

type TextureImage b = (forall a. PixelInternalFormat -> TextureSize2D -> PixelData a -> IO b) -> IO b

emptyTextureImage :: TextureImage b
emptyTextureImage act = alloca $ \ptr -> do
  poke ptr (Color4 0 0 0 0 :: Color4 Float)
  act RGBA' (TextureSize2D 1 1) (PixelData RGBA Float ptr)

sloppyLoadTexture :: forall b. FilePath -> IO (TextureImage b)
sloppyLoadTexture filename = loadTexture filename >>= \case
  Left errMsg -> do
    putStrLn $ "load image " ++ filename ++ " failed: " ++ errMsg
    return emptyTextureImage
  Right loader -> return loader

loadTexture :: forall b. FilePath -> IO (Either String (TextureImage b))
loadTexture filename = readImage filename >>= \case
  Left msg -> return $ Left msg
  Right (ImageYF _) -> return $ Left "HDR Luminance image is not supported"
  Right dyImg -> return $ Right f where
    f :: TextureImage b
    !f = case dyImg of
      ImageY8 (Image w h vec) -> \act -> V.unsafeWith vec $ \ptr -> do
        --putStrLn $ "ImageY8 " ++ show w ++ " " ++ show h
        act Luminance8 (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData Luminance UnsignedByte ptr)
      ImageY16 (Image w h vec) -> \act -> V.unsafeWith vec $ \ptr -> do
        --putStrLn $ "ImageY16 " ++ show w ++ " " ++ show h
        act Luminance16 (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData Luminance UnsignedShort ptr)
      ImageYA8 (Image w h vec) -> \act -> V.unsafeWith vec $ \ptr -> do
        --putStrLn $ "ImageYA8 " ++ show w ++ " " ++ show h
        act Luminance8Alpha8 (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData LuminanceAlpha UnsignedByte ptr)
      ImageYA16 (Image w h vec) -> \act -> V.unsafeWith vec $ \ptr -> do
        --putStrLn $ "ImageYA16 " ++ show w ++ " " ++ show h
        act Luminance16Alpha16 (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData LuminanceAlpha UnsignedShort ptr)
      ImageRGB8 img | Image w h vec <- (promoteImage img :: Image PixelRGBF) -> \act -> V.unsafeWith vec $ \ptr -> do
        -- putStrLn $ "ImageRGB8 " ++ show w ++ " " ++ show h
        act RGB' (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData RGB Float ptr)
      ImageRGB16 (Image w h vec) -> \act -> V.unsafeWith vec $ \ptr -> do
        --putStrLn $ "ImageRGB16 " ++ show w ++ " " ++ show h
        act RGB16 (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData RGB UnsignedShort ptr)
      ImageRGBF (Image w h vec) -> \act -> V.unsafeWith vec $ \ptr -> do
        --putStrLn $ "ImageRGBF " ++ show w ++ " " ++ show h
        act RGB32F (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData RGB Double ptr)
      ImageRGBA8 (Image w h vec) -> \act -> V.unsafeWith vec $ \ptr -> do
        --putStrLn $ "ImageRGBA8 " ++ show w ++ " " ++ show h
        act RGBA8 (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData RGBA UnsignedByte ptr)
      ImageRGBA16 (Image w h vec) -> \act -> V.unsafeWith vec $ \ptr -> do
        --putStrLn $ "ImageRGBA16 " ++ show w ++ " " ++ show h
        act RGBA16 (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData RGBA UnsignedShort ptr)
      ImageYCbCr8 img | Image w h vec <- (promoteImage (convertImage img :: Image PixelRGB8) :: Image PixelRGBF) -> \act -> V.unsafeWith vec $ \ptr -> do
        --putStrLn $ "ImageYCbCr8 " ++ show w ++ " " ++ show h
        act RGB' (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData RGB Float ptr)
      ImageCMYK8 img | Image w h vec <- (promoteImage (convertImage img :: Image PixelRGB8) :: Image PixelRGBF) -> \act -> V.unsafeWith vec $ \ptr -> do
        --putStrLn $ "ImageCMYK8 " ++ show w ++ " " ++ show h
        act RGB' (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData RGB Float ptr)
      ImageCMYK16 (Image w h vec) -> \act -> V.unsafeWith vec $ \ptr -> do
        --putStrLn $ "ImageCMYK8 " ++ show w ++ " " ++ show h
        act RGB16 (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData CMYK UnsignedShort ptr)

--    f act = case dyImg of
--      ImageY8 (Image w h vec) -> V.unsafeWith vec $ \ptr -> do
--        --putStrLn $ "ImageY8 " ++ show w ++ " " ++ show h
--        act Luminance8 (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData Luminance UnsignedByte ptr)
--      ImageY16 (Image w h vec) -> V.unsafeWith vec $ \ptr -> do
--        --putStrLn $ "ImageY16 " ++ show w ++ " " ++ show h
--        act Luminance16 (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData Luminance UnsignedShort ptr)
--      ImageYA8 (Image w h vec) -> V.unsafeWith vec $ \ptr -> do
--        --putStrLn $ "ImageYA8 " ++ show w ++ " " ++ show h
--        act Luminance8Alpha8 (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData LuminanceAlpha UnsignedByte ptr)
--      ImageYA16 (Image w h vec) -> V.unsafeWith vec $ \ptr -> do
--        --putStrLn $ "ImageYA16 " ++ show w ++ " " ++ show h
--        act Luminance16Alpha16 (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData LuminanceAlpha UnsignedShort ptr)
--      ImageRGB8 img | Image w h vec <- (promoteImage img :: Image PixelRGBF) -> V.unsafeWith vec $ \ptr -> do
--        -- putStrLn $ "ImageRGB8 " ++ show w ++ " " ++ show h
--        act RGB' (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData RGB Float ptr)
--      ImageRGB16 (Image w h vec) -> V.unsafeWith vec $ \ptr -> do
--        --putStrLn $ "ImageRGB16 " ++ show w ++ " " ++ show h
--        act RGB16 (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData RGB UnsignedShort ptr)
--      ImageRGBF (Image w h vec) -> V.unsafeWith vec $ \ptr -> do
--        --putStrLn $ "ImageRGBF " ++ show w ++ " " ++ show h
--        act RGB32F (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData RGB Double ptr)
--      ImageRGBA8 (Image w h vec) -> V.unsafeWith vec $ \ptr -> do
--        --putStrLn $ "ImageRGBA8 " ++ show w ++ " " ++ show h
--        act RGBA8 (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData RGBA UnsignedByte ptr)
--      ImageRGBA16 (Image w h vec) -> V.unsafeWith vec $ \ptr -> do
--        --putStrLn $ "ImageRGBA16 " ++ show w ++ " " ++ show h
--        act RGBA16 (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData RGBA UnsignedShort ptr)
--      ImageYCbCr8 img | Image w h vec <- (promoteImage (convertImage img :: Image PixelRGB8) :: Image PixelRGBF) -> V.unsafeWith vec $ \ptr -> do
--        --putStrLn $ "ImageYCbCr8 " ++ show w ++ " " ++ show h
--        act RGB' (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData RGB Float ptr)
--      ImageCMYK8 img | Image w h vec <- (promoteImage (convertImage img :: Image PixelRGB8) :: Image PixelRGBF) -> V.unsafeWith vec $ \ptr -> do
--        --putStrLn $ "ImageCMYK8 " ++ show w ++ " " ++ show h
--        act RGB' (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData RGB Float ptr)
--      ImageCMYK16 (Image w h vec) -> V.unsafeWith vec $ \ptr -> do
--        --putStrLn $ "ImageCMYK8 " ++ show w ++ " " ++ show h
--        act RGB16 (TextureSize2D (fromIntegral w) (fromIntegral h)) (PixelData CMYK UnsignedShort ptr)
