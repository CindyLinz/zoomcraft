module ShaderTemplate where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

import System.IO

shaderFile :: FilePath -> Q Exp
shaderFile filename = do
  code <- runIO $ readFile filename
  varE (mkName "fromString") `appE` stringE code
