module Nix.Parser
  ( parseNixFile
  , parseNixFileLoc
  , parseNixText
  , parseNixTextLoc
  , parseFromFileEx
  , Parser
  , parseFromText
  -- TODO re-export
  , Result(..)
  -- Not relevant
  -- , reservedNames
  -- TODO re-export
  , OperatorInfo(..)
  , NSpecialOp(..)
  , NAssoc(..)
  , NOperatorDef
  , getUnaryOperator
  , getBinaryOperator
  , getSpecialOperator
  -- end
  , nixToplevelForm
  , nixExpr
  , nixSet
  , nixBinders
  , nixSelector
  , nixSym
  , nixPath
  , nixString
  , nixUri
  , nixSearchPath
  , nixFloat
  , nixInt
  , nixBool
  , nixNull
  -- Not relevant
  -- , symbol
  -- , whiteSpace
  -- end
  )
where

-- fill in imports

parseNixFile :: MonadFile m => FilePath -> m (Result NExpr)
parseNixFile = undefined

parseNixFileLoc :: MonadFile m => FilePath -> m (Result NExprLoc)
parseNixFileLoc = undefined

parseNixText :: Text -> Result NExpr
pasreNixText = undefined

parseNixTextLoc :: Text -> Result NExprLoc
parseNixTextLoc = undefined

parseFromFileEx :: MonadFile m => Parser a -> FilePath -> m (Result a)
parseFromFileEx = undefined

type Parser = ParsecT Void TokenStream Identity

parseFromText :: Parser a -> Text -> Result a
parseFromText = undefined

-- TODO re-export
data Result a = Success a | Failure (Doc Void) deriving (Show, Functor)

nixToplevelForm :: Parser NExprLoc
nixToplevelForm = undefined

nixExpr :: Parser NExprLoc
nixExpr = undefined

nixSet :: Parser NExprLoc
nixSet = undefined

nixBinders :: Parser [Binding NExprLoc]
nixBinders = undefined

nixSelector :: Parser (Ann SrcSpan (NAttrPath NExprLoc))
nixSelector = undefined

nixSym :: Parser NExprLoc
nixSym = undefiined

nixPath :: Parser NExprLoc
nixPath = undefiined

nixString :: Parser NExprLoc
nixString = undefined

nixUri :: Parser NExprLoc
nixUri = undefined

nixSearchPath :: Parser NExprLoc
nixSearchPath = undefined

nixFloat :: Parser NExprLoc
nixFloat = undefined

nixInt :: Parser NExprLoc
nixInt = undefined

nixBool :: Parser NExprLoc
nixBool = undefined

nixNull :: Parser NExprLoc
nixNull = undefined

--
tif :: Parser ()
tif = void . satisfy $ \case TTk TIf -> True ; _ -> False
-- and likewise for the other ones...

tchar :: Char -> Parser ()
tchar = void . single . TChar

tid :: Parser Text
tid = flip token Set.empty \case TText TId t -> Just t ; _ -> Nothing
-- and likewise for the other ones...
