module Shader where

import Data.String
import Control.Monad
import qualified Data.ByteString as BS

import Graphics.Rendering.OpenGL.GL

import ShaderTemplate

mkShader :: ShaderType -> BS.ByteString -> IO Shader
mkShader ty src = do
  shdr <- createShader ty
  shaderSourceBS shdr $= src
  compileShader shdr

  log <- get $ shaderInfoLog shdr
  putStrLn $ "compile " ++ show ty ++ " shader:"
  putStrLn log

  return shdr

plainVertexShaderCode :: IsString a => a
plainVertexShaderCode = $(shaderFile "src/shader/plain_vertex.glsl")
plainFragmentShaderCode :: IsString a => a
plainFragmentShaderCode = $(shaderFile "src/shader/plain_fragment.glsl")
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

imageVertexShaderCode :: IsString a => a
imageVertexShaderCode = $(shaderFile "src/shader/image_vertex.glsl")
imageFragmentShaderCode :: IsString a => a
imageFragmentShaderCode = $(shaderFile "src/shader/image_fragment.glsl")
mkImageShader
  :: IO
    ( Program
    , AttribLocation -- pos
    , AttribLocation -- uv
    , UniformLocation -- tex
    )
mkImageShader = do
  vshdr <- mkShader VertexShader imageVertexShaderCode
  fshdr <- mkShader FragmentShader imageFragmentShaderCode

  prg <- createProgram
  attachShader prg vshdr
  attachShader prg fshdr
  linkProgram prg

  posLoc <- get $ attribLocation prg "pos"
  uvLoc <- get $ attribLocation prg "uv"
  texLoc <- get $ uniformLocation prg "tex"

  return (prg, posLoc, uvLoc, texLoc)
