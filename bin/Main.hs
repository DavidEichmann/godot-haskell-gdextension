module Main (main) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import ExtensionApi
import GdExtensionInterface
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  -- iface <- readGdExtensionInterface "./godot-api/gdextension_interface.4.6.2.json"
  -- pPrint iface

  api <- BSL.readFile "./godot-api/extension_api.4.6.2.json"
  pPrint (eitherDecode @ExtensionApi api)
