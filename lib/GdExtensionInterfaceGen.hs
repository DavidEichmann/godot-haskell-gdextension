{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module GdExtensionInterfaceGen (gdExtensionInterfaceGen) where

import Data.Text
import GdExtensionInterface
import StringInterp ()
import Prelude as P hiding (unlines)

gdExtensionInterfaceGen :: GdExtensionInterface -> Text
gdExtensionInterfaceGen
  GdExtensionInterface
    { copyright,
      formatVersion,
      types,
      interface
    } =
    unlines
      [ "",
        "",
        genTypes types,
        "",
        "-- TODO interface"
      ]

genTypes :: [Type] -> Text
genTypes types = intercalate "\n\n" (fmap genType types)

genType :: Type -> Text
genType
  Type
    { description,
      name,
      deprecated,
      typeDesc
    } = case typeDesc of
    TypeDesc_Enum {isBitField, values} ->
      unlines
        [ descToHaddock "" description,
          "data " name " = ",
          unlines
            [ "" (descToHaddock "  " description) "\n  " prefix " " name ""
            | ( prefix,
                EnumValue
                  { name,
                    value,
                    description
                  }
                ) <-
                P.zip
                  (("=" :: Text) : repeat "|")
                  values
            ]
        ]
    TypeDesc_Handle {} -> "-- TODO"
    TypeDesc_Alias {} -> "-- TODO"
    TypeDesc_Struct {} -> "-- TODO"
    TypeDesc_Function {} -> "-- TODO"

descToHaddock :: Text -> Maybe Text -> Text
descToHaddock indent descMay = case descMay of
  Nothing -> ""
  Just desc -> replace "\n" ("\n" <> indent <> "-- ") (indent <> "-- | " <> desc)