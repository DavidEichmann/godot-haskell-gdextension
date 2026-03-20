{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module TH where

import Data.Aeson (Value, decodeStrictText, eitherDecode)
import Data.Aeson qualified as Aeson
import Data.Foldable (Foldable (toList))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.IO qualified as T
import Language.Haskell.TH (CodeQ, Dec, Exp, Q, Quote, runIO, runQ)
import Language.Haskell.TH.Syntax (Quasi, addDependentFile, joinCode)
import TValue (JsonType (..), TValue (..))

readFileAsString' :: String -> CodeQ String
readFileAsString' path = joinCode $ do
  addDependentFile path
  x <- runIO (readFile path)
  return [||x||]

readFileAsText' :: String -> Q Text
readFileAsText' path = do
  addDependentFile path
  runIO (T.readFile path)

importJsonFileAsTValue' :: (Quote m, Quasi m) => String -> m Exp
importJsonFileAsTValue' filePath = do
  jsonText <- runQ $ readFileAsText' filePath
  let Just json = decodeStrictText @Value jsonText
  [|valueToTValue json|]

valueToTValue :: Value -> Q Exp
valueToTValue value = case value of
  Aeson.Object obj -> objectToTValue obj
  Aeson.Array xs ->
    if null xs
      then [|TValueArray @JsonTypeNull []|]
      else
        let xs' = fmap valueToTValue (toList xs)
         in return [|TValueArray $xs'|]
  Aeson.String _ -> error "TODO"
  Aeson.Number _ -> error "TODO"
  Aeson.Bool _ -> error "TODO"
  Aeson.Null -> [|TValueNull|]

objectToTValue :: Aeson.Object -> Q Exp
objectToTValue obj = _

godotExtensionApi' :: String -> CodeQ Value
godotExtensionApi' path =
  [||
  case eitherDecode @Value (fromString $$(readFileAsString' path)) of
    Left err -> error ("Failed to parse" ++ err)
    Right api -> api
  ||]
