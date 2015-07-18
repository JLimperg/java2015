{-# LANGUAGE OverloadedStrings #-}

module Eval.Stmt
( Stmt(..)

, parseStmt
, stmtP

, ppStmt

, execStmt
) where

import           Control.Applicative
import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import           Data.Foldable
import           Data.Monoid ((<>))
import           Data.Text.Lazy.Builder
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Prim hiding ((<|>))

import           Eval.AExpr
import           Eval.Common
import           Eval.Env
import           Eval.Token
import           List1
import           MonadRepl

-------------------------------------------------------------------------------
-- Data

data Stmt
    = If     AExpr Stmt Stmt
    | While  AExpr Stmt
    | Assign Name AExpr
    | Seq    (List1 Stmt)
 deriving (Eq, Ord, Read, Show)

-------------------------------------------------------------------------------
-- Parsing

parseStmt :: Text -> Either ParseError Stmt
parseStmt = parse stmtP "" <=< tokenize

stmtP :: TokenParser Stmt
stmtP = ifP <|> whileP <|> assignP <|> seqP

ifP :: TokenParser Stmt
ifP = If <$> (ifTok *> parenOpenTok *> aExprP <* parenCloseTok)
         <*> (stmtP <* elseTok) <*> stmtP

whileP :: TokenParser Stmt
whileP = While <$> (whileTok *> parenOpenTok *> aExprP <* parenCloseTok)
               <*> stmtP

assignP :: TokenParser Stmt
assignP = Assign <$> (identTok <* equalsTok) <*> aExprP

seqP :: TokenParser Stmt
seqP = Seq <$> between curlyBraceOpenTok curlyBraceCloseTok
               (list1P stmtP semicolonTok)

-------------------------------------------------------------------------------
-- Printing

ppStmt :: Stmt -> TL.Text
ppStmt = toLazyText . ppStmt'

ppStmt' :: Stmt -> Builder
ppStmt' (If cond thenBranch elseBranch)
    =  "if(" <> ppAExpr' cond <> ") " <> ppStmt' thenBranch
    <> " else " <> ppStmt' elseBranch

ppStmt' (While cond body)
    =  "while(" <> ppAExpr' cond <> ") " <> ppStmt' body

ppStmt' (Assign varname val)
    = fromText (fromName varname) <> "=" <> ppAExpr' val

ppStmt' (Seq stmts)
    = ppList1 ";" ppStmt' stmts

-------------------------------------------------------------------------------
-- Execution

execStmt :: (Monad m) => Stmt -> ReplT m ()
execStmt (If cond thenBranch elseBranch) = do
    condVal <- evalAExpr cond
    if condVal /= 0
       then execStmt thenBranch
       else execStmt elseBranch

execStmt stmt@(While cond body) = do
    condVal <- evalAExpr cond
    if condVal /= 0
       then execStmt body >> execStmt stmt
       else return ()

execStmt (Assign varname val) = assignVar varname =<< evalAExpr val

execStmt (Seq stmts) = mapM_ execStmt . toList $ stmts
