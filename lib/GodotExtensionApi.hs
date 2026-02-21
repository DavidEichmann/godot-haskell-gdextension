{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module GodotExtensionApi where

import Data.Aeson (FromJSON (..))
import Data.Text
import GHC.Generics (Generic)
import Generics.Generic.Aeson (Settings (..), defaultSettings, gparseJsonWithSettings)
import Prelude hiding (Enum)

data GodotExtensionApi = GodotExtensionApi
  { header :: Header,
    builtin_class_sizes :: [BuiltinClassSizes],
    builtin_class_member_offsets :: [BuiltinClassMemberOffsetsBuildConfig],
    global_enums :: [GlobalEnum],
    utility_functions :: [UtilityFunction],
    builtin_classes :: [BuiltinClass],
    classes :: [Class],
    singletons :: [Singleton],
    native_structures :: [NativeStructure]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data Header = Header
  { version_major :: Int,
    version_minor :: Int,
    version_patch :: Int,
    version_status :: Text,
    version_build :: Text,
    version_full_name :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data BuiltinClassSizes = BuiltinClassSizes
  { build_configuration :: Text,
    sizes :: [BuiltinClassSize]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data BuiltinClassSize = BuiltinClassSize
  { name :: Text,
    size :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data BuiltinClassMemberOffsetsBuildConfig = BuiltinClassMemberOffsetsBuildConfig
  { build_configuration :: Text,
    classes :: [BuiltinClassMemberOffsets]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data BuiltinClassMemberOffsets = BuiltinClassMemberOffsets
  { name :: Text,
    members :: [BuiltinClassMemberOffset]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data BuiltinClassMemberOffset = BuiltinClassMemberOffset
  { member :: Text,
    offset :: Int,
    meta :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data GlobalEnum = GlobalEnum
  { name :: Text,
    values :: [GlobalEnumValue]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data GlobalEnumValue = GlobalEnumValue
  { name :: Text,
    value :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data UtilityFunction = UtilityFunction
  { name :: Text,
    return_type :: Maybe Text,
    category :: Text,
    is_vararg :: Bool,
    hash :: Int,
    arguments :: Maybe [FunctionArgument]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data FunctionArgument = FunctionArgument
  { name :: Text,
    _type :: Text,
    default_value :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromJSON FunctionArgument where
  parseJSON =
    gparseJsonWithSettings
      ( defaultSettings
          { stripPrefix = Just "_"
          }
      )

data BuiltinClass = BuiltinClass
  { name :: Text,
    is_keyed :: Bool,
    operators :: [Operator],
    constructors :: [Constructor],
    methods :: Maybe [Method],
    has_destructor :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data Operator = Operator
  { name :: Text,
    right_type :: Maybe Text,
    return_type :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data Constructor = Constructor
  { index :: Int,
    arguments :: Maybe [FunctionArgument]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data Method = Method
  { name :: Text,
    return_type :: Maybe Text,
    is_vararg :: Bool,
    is_const :: Bool,
    is_static :: Bool,
    hash :: Int,
    arguments :: Maybe [FunctionArgument]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data Class = Class
  { name :: Text,
    is_refcounted :: Bool,
    is_instantiable :: Bool,
    inherits :: Maybe Text,
    api_type :: Text,
    enums :: Maybe [Enum]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data Enum = Enum
  { name :: Text,
    is_bitfield :: Bool,
    values :: [EnumValue]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data EnumValue = EnumValue
  { name :: Text,
    value :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data Singleton = Singleton
  { name :: Text,
    _type :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON Singleton where
  parseJSON =
    gparseJsonWithSettings
      ( defaultSettings
          { stripPrefix = Just "_"
          }
      )

data NativeStructure = NativeStructure
  { name :: Text,
    format :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)
