module Main (main) where

-- import Data.Aeson (eitherDecode)
-- import Data.ByteString.Lazy qualified as BSL
-- import ExtensionApi

import Data.Text.IO qualified as T
import GdExtensionInterface
import GdExtensionInterfaceGen
import Language.Haskell.TH (pprint)

-- import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  Right iface <- readGdExtensionInterface "./godot-api/gdextension_interface.4.6.2.json"
  T.putStrLn $ gdExtensionInterfaceGen iface

-- pPrint iface

-- api <- BSL.readFile "./godot-api/extension_api.4.6.2.json"
-- pPrint (eitherDecode @ExtensionApi api)

-- godotgenerateGodotInterface :: GdExtensionInterface ->