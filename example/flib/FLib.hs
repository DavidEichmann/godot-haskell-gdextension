{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FLib where

import Foreign.C
import Foreign.Marshal

godot_ext_init :: IO CBool
godot_ext_init = do
  -- TODO initialize gdext stuff (catch and print exceptions returning False)
  putStrLn "Haskell NativeScript lib initialized"
  return (fromBool True)

foreign export ccall godot_ext_init :: IO CBool
