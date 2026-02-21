{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module GodotExtensionInterface (Interface (..), readInterface) where

import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.C
import Language.C.System.GCC
import Text.Pretty.Simple (pPrint)

data Interface = Interface
  { enums :: [InterfaceEnum],
    structs :: [InterfaceStruct],
    unhandled :: [Text]
  }
  deriving stock (Show)

data InterfaceEnum = InterfaceEnum
  { name :: Text,
    variants :: [EnumVariant]
  }
  deriving stock (Show)

data EnumVariant = EnumVariant
  { name :: Text,
    value :: Int
  }
  deriving stock (Show)

data InterfaceStruct = InterfaceStruct
  { name :: Text,
    fields :: [StructField]
  }
  deriving stock (Show)

data StructField = StructField
  { name :: Text,
    typ :: Text
  }
  deriving stock (Show)

-- | Read and parse a gdextension_interface.h file
readInterface ::
  -- | Path to the gdextension_interface.h file
  FilePath ->
  IO Interface
readInterface interfaceHFilePath =
  do
    rawFile <- T.readFile interfaceHFilePath
    parse_result <- parseCFile (newGCC "gcc") Nothing [] interfaceHFilePath
    let CTranslUnit allDecls _ = case parse_result of
          Left parse_err -> error (show parse_err)
          Right ast -> ast

    let godotDecls = filter (\decl -> fileOfNode decl == Just interfaceHFilePath) allDecls

    pPrint ((map (() <$) godotDecls) !! 219)
    error ""

    let -- Find the comment above a declaration line
        findApiComment :: Int -> Maybe ApiComment
        findApiComment declLine =
          let rawLines = T.lines rawFile
              linesUp =
                rawLines
                  & take (declLine - 1)
                  & map T.strip
                  & reverse
              commentLinesMay = case linesUp of
                [] -> Nothing
                (l : ls) -> case l of
                  "*/" -> Just (ls & takeWhile ("*" `T.isPrefixOf`) & mapMaybe (T.stripPrefix "*") & map T.strip & reverse)
                  _ -> Nothing
           in do
                commentLines <- commentLinesMay
                let lookupProp prop = map T.strip $ mapMaybe (prop `T.stripPrefix`) commentLines
                    lookupProp1 = listToMaybe . lookupProp
                    comment = commentLines & filter (\line -> not ("@" `T.isPrefixOf` line)) & T.unlines & T.strip
                    params =
                      lookupProp "@param"
                        & map (\line -> let (key, rest) = T.breakOn " " line in (key, T.drop 1 rest))
                        & M.fromList

                Just
                  ApiComment
                    { name = lookupProp1 "@name",
                      since = lookupProp1 "@since",
                      params,
                      comment,
                      deprecated = lookupProp1 "@deprecated"
                    }

        go :: [Text] -> [Text] -> [CExternalDeclaration NodeInfo] -> Interface
        go typeDefs unhandled decls = case decls of
          [] -> Interface {}
          d : declsTail ->
            let -- ( case d of
                --     CDeclExt (CDecl _ _ i) -> show i
                --     _ -> "-"
                -- )
                lineNum = d & posOf & posRow
                apiComment = findApiComment lineNum
                (typeDef', unhandled') = case apiComment of
                  Just ApiComment {name = Just name} -> ([name], [])
                  _ -> ([], ["Unhandled header at line: " <> T.show lineNum])
             in go
                  (typeDef' ++ typeDefs)
                  (unhandled' ++ unhandled)
                  declsTail

    return (go [] [] godotDecls)

parseDecl :: CExternalDeclaration NodeInfo -> Maybe ParsedDecl
parseDecl decl = case decl of
  CDeclExt _ -> error "TODO"
  CFDefExt _ -> error "TODO"
  CAsmExt _ _ -> error "TODO"

data ParsedDecl = ParsedDecl
  {
  }

data ApiComment = ApiComment
  { name :: Maybe Text,
    since :: Maybe Text,
    params :: Map Text Text,
    comment :: Text,
    deprecated :: Maybe Text
  }
  deriving stock (Show)

{-
data TypeMod
  = PtrConst
  | Ptr

-- | Describes a function pointer type
data FunctionTypeDef = FunctionTypeDef
  { name :: String,
    -- | The return type
    returnType :: String,
    -- | Modifiers to the return type
    returnTypeMod :: Maybe TypeMod,
    -- | TODO function arguments
    args :: [()]
  }

instance Show FunctionTypeDef where
  show FunctionTypeDef {..} =
    ( case returnTypeMod of
        Nothing -> ""
        Just Ptr -> "* "
        Just PtrConst -> "* const "
    )
      ++ returnType
      ++ " "
      ++ name
      ++ "(...)"

typeDefs :: CTranslUnit -> [FunctionTypeDef]
typeDefs header = goCTranslUnit header
  where
    isGodot nodeInfo = fileOfNode nodeInfo == Just headerPath

    goCTranslUnit (CTranslUnit decs _) = concatMap goCExternalDeclaration decs

    goCExternalDeclaration = \case
      (CDeclExt x@(CDecl specs _ cDeclExtNodeInfo))
        | isGodot cDeclExtNodeInfo ->
            case x of
              CDecl
                ((CStorageSpec (CTypedef (NodeInfo _ _ _))) : specs)
                [ ( Just (CDeclr (Just (Ident functionName _ _)) (CPtrDeclr _ _ : CFunDeclr _ _ _ : maybeRetPtr) Nothing [] _),
                    Nothing,
                    Nothing
                    )
                  ]
                _
                  | Just isPtrReturn <- case maybeRetPtr of
                      [] -> Just False
                      [CPtrDeclr _ _] -> Just True
                      _ -> Nothing,
                    Just (isConst, cTypeDef) <-
                      ( case specs of
                          [CTypeQual (CConstQual _), CTypeSpec cTypeDef] -> Just (True, cTypeDef)
                          [CTypeSpec cTypeDef] -> Just (False, cTypeDef)
                          _ -> Nothing
                      ),
                    Just returnType <-
                      ( case cTypeDef of
                          CTypeDef (Ident t _ _) _ -> Just t
                          CVoidType _ -> Just "void"
                          CFloatType _ -> Just "float"
                          CDoubleType _ -> Just "double"
                          _ -> Nothing
                      ) ->
                      [ FunctionTypeDef
                          { name = functionName,
                            returnType,
                            returnTypeMod =
                              if isConst
                                then
                                  if isPtrReturn
                                    then Just PtrConst
                                    else error "const but not ptr!"
                                else
                                  if isPtrReturn
                                    then Just Ptr
                                    else Nothing,
                            args = [] -- TODO
                          }
                      ]
              -- Structs
              CDecl [(CStorageSpec (CTypedef (NodeInfo _ _ _))), CTypeSpec (CSUType _ _)] _ _ -> []
              -- Enums
              CDecl [(CStorageSpec (CTypedef (NodeInfo _ _ _))), CTypeSpec (CEnumType _ _)] _ _ -> []
              -- Non-functions
              CDecl _ [(Just (CDeclr _ xs _ _ _), _, _)] _ | not (isFunc xs) -> []
              x -> error $ "Handle this!:\n" ++ T.unpack (pShow x)
        where
          isFunc derDecs =
            any
              ( \case
                  (CFunDeclr _ _ _) -> True
                  _ -> False
              )
              derDecs
      x -> []
-}