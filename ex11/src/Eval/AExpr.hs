{-# LANGUAGE OverloadedStrings #-}

module Eval.AExpr
( AExpr
, Sum(..)
, Mult(..)
, APrim(..)

, parseAExpr
, aExprP

, ppAExpr
, ppAExpr'

, evalAExpr
) where

import           Control.Applicative hiding (Const)
import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import           Data.Monoid ((<>))
import           Data.Text.Lazy.Builder
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Prim hiding ((<|>))

import           Eval.Common
import           Eval.Env
import           Eval.Token
import           List1
import           MonadRepl

-------------------------------------------------------------------------------
-- Arithmetic expressions

type AExpr = Sum

newtype Sum = Sum { fromSum :: List1 Mult }
  deriving (Read, Show, Eq, Ord)

newtype Mult = Mult { fromMult :: List1 APrim }
  deriving (Read, Show, Eq, Ord)

data APrim
    = AVar Name
    | AConst Integer
    | AParens Sum
  deriving (Read, Show, Eq, Ord)

-------------------------------------------------------------------------------
-- Parsing

parseAExpr :: Text -> Either ParseError AExpr
parseAExpr = parse aExprP "" <=< tokenize

aExprP :: TokenParser AExpr
aExprP = sumP

sumP :: TokenParser Sum
sumP = Sum <$> list1P multP plusTok

multP :: TokenParser Mult
multP = Mult <$> list1P aPrimP starTok

aPrimP :: TokenParser APrim
aPrimP =   (AVar <$> identTok)
       <|> (AConst <$> naturalLitTok)
       <|> (AParens <$> between parenOpenTok parenCloseTok aExprP)

-------------------------------------------------------------------------------
-- Printing

ppAExpr :: AExpr -> TL.Text
ppAExpr = toLazyText . ppAExpr'

ppAExpr' :: AExpr -> Builder
ppAExpr' = ppSum

ppSum :: Sum -> Builder
ppSum = ppList1 "+" ppProduct . fromSum

ppProduct :: Mult -> Builder
ppProduct = ppList1 "*" ppAPrim . fromMult

ppAPrim :: APrim -> Builder
ppAPrim (AVar name) = fromText . fromName $ name
ppAPrim (AConst n)  = fromString $ show n
ppAPrim (AParens e) = "(" <> ppAExpr' e <> ")"

-------------------------------------------------------------------------------
-- Evaluation

evalAExpr :: (Monad m) => AExpr -> ReplT m Integer
evalAExpr = evalSum

evalSum :: (Monad m) => Sum -> ReplT m Integer
evalSum = fmap sum . traverse evalMult . fromSum

evalMult :: (Monad m) => Mult -> ReplT m Integer
evalMult = fmap product . traverse evalAPrim . fromMult

evalAPrim :: (Monad m) => APrim -> ReplT m Integer
evalAPrim (AVar name) = lookupVarE name
evalAPrim (AConst n)  = return n
evalAPrim (AParens e) = evalAExpr e
