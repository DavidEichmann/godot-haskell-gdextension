{-# LANGUAGE TemplateHaskell #-}

module MyLib where

import Data.Aeson (Value, eitherDecode)
import Data.String (IsString (..))
import GodotExtensionApi
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (Exp, Lift (lift), addDependentFile, lift)
import TH

-- godotExtensionApi :: Value
-- godotExtensionApi = $$(godotExtensionApi' "./godot-api/extension_api_4.6.json")
