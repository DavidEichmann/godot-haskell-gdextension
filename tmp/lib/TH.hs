{-# LANGUAGE TemplateHaskell #-}

module TH where

import Data.Aeson (Value, eitherDecode)
import Data.String (IsString (..))
import GodotExtensionApi
import Language.Haskell.TH (CodeQ, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, joinCode)

readFileAsString' :: String -> CodeQ String
readFileAsString' path = joinCode $ do
  addDependentFile path
  x <- runIO (readFile path)
  return [||x||]

godotExtensionApi' :: String -> CodeQ Value
godotExtensionApi' path =
  [||
  case eitherDecode @Value (fromString $$(readFileAsString' path)) of
    Left err -> error ("Failed to parse" ++ err)
    Right api -> api
  ||]
