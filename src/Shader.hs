module Shader where

import Data.String
import Control.Monad
import qualified Data.ByteString as BS
import System.IO
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

import Graphics.Rendering.OpenGL.GL

plainVertexShaderCode :: IsString a => a
plainVertexShaderCode = fromString $
  $( do
      code <- runIO $ readFile "src/shader/plain_vertex.glsl"
      stringE code
  )

plainFragmentShaderCode :: IsString a => a
plainFragmentShaderCode = fromString $
  $( do
      code <- runIO $ readFile "src/shader/plain_fragment.glsl"
      stringE code
  )

mkShader :: ShaderType -> BS.ByteString -> IO Shader
mkShader ty src = do
  shdr <- createShader ty
  shaderSourceBS shdr $= src
  compileShader shdr

  log <- get $ shaderInfoLog shdr
  putStrLn $ "compile " ++ show ty ++ " shader:"
  putStrLn log

  return shdr

mkPlainShader
  :: IO
    ( Program
    , AttribLocation -- pos
    , UniformLocation -- color
    )
mkPlainShader = do
  vshdr <- mkShader VertexShader plainVertexShaderCode
  fshdr <- mkShader FragmentShader plainFragmentShaderCode

  prg <- createProgram
  attachShader prg vshdr
  attachShader prg fshdr
  linkProgram prg

  posLoc <- get $ attribLocation prg "pos"
  colorLoc <- get $ uniformLocation prg "color"

  return (prg, posLoc, colorLoc)
