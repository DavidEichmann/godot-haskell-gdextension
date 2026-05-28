module Main (main) where

import GdExtensionInterface
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  iface <- readGdExtensionInterface "./godot-api/gdextension_interface.4.6.2.json"
  pPrint iface
