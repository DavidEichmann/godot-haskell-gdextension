{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}

import qualified Data.Text.Lazy as T
import Language.C
import Language.C.Data.Ident
import Language.C.System.GCC
import System.IO.Unsafe (unsafePerformIO)
import Text.Pretty.Simple (pPrint, pShow)

main :: IO ()
main = putStrLn (unlines typeDefs)

headerPath :: String
headerPath = "api/gdextension_interface.h"

typeDefs :: [String]
typeDefs = goCTranslUnit header
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
                      [ (if isPtrReturn then "*" else "")
                          ++ (if isConst then " const " else "")
                          ++ returnType
                          ++ " "
                          ++ functionName
                          ++ "(...)"
                      ]
              -- Structs
              CDecl [(CStorageSpec (CTypedef (NodeInfo _ _ _))), CTypeSpec (CSUType _ _)] _ _ -> []
              -- Enums
              CDecl [(CStorageSpec (CTypedef (NodeInfo _ _ _))), CTypeSpec (CEnumType _ _)] _ _ -> []
              -- Non-functions
              CDecl _ [(Just (CDeclr _ xs _ _ _), _, _)] _ | not (isFunc xs) -> []
              x -> ["Handle this!:\n" ++ T.unpack (pShow x)]
        where
          isFunc derDecs =
            any
              ( \case
                  (CFunDeclr _ _ _) -> True
                  _ -> False
              )
              derDecs
      x -> []

-- where
-- failTypeDef = ["Oops! I don't understand this typedef: " ++ show (nodeInfo x)]

header :: CTranslUnit
header = unsafePerformIO parseHeader

parseHeader :: IO CTranslUnit
parseHeader = parseMyFile headerPath

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file =
  do
    parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
    case parse_result of
      Left parse_err -> error (show parse_err)
      Right ast -> return ast
