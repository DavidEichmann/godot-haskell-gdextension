{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module TValue where

import Data.Data (Proxy)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.TypeLits (Symbol)

data TValue (jsonType :: JsonType) :: Type where
  TValueObject :: TObject t -> TValue (JsonTypeObject t)
  TValueArray :: [TValue elType] -> TValue (JsonTypeArray elType)
  TValueString :: Text -> TValue JsonTypeString
  TValueNumber :: Double -> TValue JsonTypeNumber
  TValueBool :: Bool -> TValue JsonTypeBool
  TValueNull :: TValue JsonTypeNull

data TObject (fields :: [ObjectField]) :: Type where
  TObjectEmpty :: TObject '[]
  TObjectCons ::
    (Proxy fieldName) ->
    TValue fieldType ->
    TObject otherFields ->
    TObject ('ObjectField fieldName fieldType : otherFields)

data JsonType
  = JsonTypeObject [ObjectField]
  | JsonTypeArray JsonType
  | JsonTypeString
  | JsonTypeNumber
  | JsonTypeBool
  | JsonTypeNull

data ObjectField = ObjectField Symbol JsonType