{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Eval.Token
( tokenize

, TokenParser
, watchTok
, unwatchTok
, quitTok
, whileTok
, ifTok
, elseTok
, parenOpenTok
, parenCloseTok
, curlyBraceOpenTok
, curlyBraceCloseTok
, semicolonTok
, equalsTok
, plusTok
, starTok
, identTok
, naturalLitTok
) where

import           Prelude hiding (lex)

import           Data.Text (Text, pack, unpack)
import           Text.Parsec
import           Text.Parsec.Text

import           Eval.Env

-------------------------------------------------------------------------------
-- Tokens

data Token
    = TWatch                    -- :watch
    | TUnwatch                  -- :unwatch
    | TQuit                     -- :quit
    | TWhile                    -- while
    | TIf                       -- if
    | TElse                     -- else
    | TParenOpen                -- (
    | TParenClose               -- )
    | TCurlyBraceOpen           -- {
    | TCurlyBraceClose          -- }
    | TSemicolon                -- ;
    | TEquals                   -- =
    | TPlus                     -- +
    | TStar                     -- *
    | TIdent !Text              -- identifier ([a-zA-Z_][a-zA-Z0-9_]*)
    | TNaturalLit !Integer      -- natural literal ([0-9]+) with # of digits
  deriving (Read, Show, Eq, Ord)

-------------------------------------------------------------------------------
-- Tokenizer

tokenize :: Text -> Either ParseError [(Token, SourcePos)]
tokenize = parse (tokensP <* eof) ""

tokensP :: Parser [(Token, SourcePos)]
tokensP = spaces *> many (withPos tokenP)
  where
    withPos :: Parser Token -> Parser (Token, SourcePos)
    withPos parser = do
        start  <- getPosition
        result <- parser
        return (result, start)

tokenP :: Parser Token
tokenP = choice -- [1]
    [ parenOpenP, parenCloseP, curlyBraceOpenP, curlyBraceCloseP, semicolonP
    , equalsP, plusP, starP, natLitP
    , try watchP, try unwatchP, quitP
    , try whileP, try ifP, try elseP, identP
    ]
    <* spaces
  where
    watchP           = string ":watch"   *> pure TWatch
    unwatchP         = string ":unwatch" *> pure TUnwatch
    quitP            = string ":quit"    *> pure TQuit
    whileP           = keywordP "while"  *> pure TWhile
    ifP              = keywordP "if"     *> pure TIf
    elseP            = keywordP "else"   *> pure TElse
    parenOpenP       = char '(' *> pure TParenOpen
    parenCloseP      = char ')' *> pure TParenClose
    curlyBraceOpenP  = char '{' *> pure TCurlyBraceOpen
    curlyBraceCloseP = char '}' *> pure TCurlyBraceClose
    semicolonP       = char ';' *> pure TSemicolon
    equalsP          = char '=' *> pure TEquals
    plusP            = char '+' *> pure TPlus
    starP            = char '*' *> pure TStar
    identP           = TIdent . pack
                     <$> ((:) <$> firstIdentChar <*> many identChar)
                     <?> "identifier"
    natLitP          = TNaturalLit <$> natural

-- [1] Note the somewhat tricky usage of @try@.

keywordP :: String -> Parser String
keywordP keyword = (try (string keyword <* notFollowedBy identChar)) <?> keyword

identChar :: Parser Char
identChar = alphaNum <|> char '_'

firstIdentChar :: Parser Char
firstIdentChar = letter <|> char '_'

natural :: Parser Integer
natural = read <$> many1 digit

-------------------------------------------------------------------------------
-- Token parsers

type TokenParser a = Parsec [(Token, SourcePos)] () a

ppToken :: Token -> String
ppToken TWatch           = ":watch"
ppToken TUnwatch         = ":unwatch"
ppToken TQuit            = ":quit"
ppToken TWhile           = "while"
ppToken TIf              = "if"
ppToken TElse            = "else"
ppToken TParenOpen       = "("
ppToken TParenClose      = ")"
ppToken TCurlyBraceOpen  = "{"
ppToken TCurlyBraceClose = "}"
ppToken TSemicolon       = ";"
ppToken TEquals          = "="
ppToken TPlus            = "+"
ppToken TStar            = "*"
ppToken (TIdent ident)   = unpack ident
ppToken (TNaturalLit n)  = show n

myToken :: (Token -> Maybe a) -> TokenParser a
myToken match = token (ppToken . fst) snd (match . fst)

watchTok :: TokenParser ()
watchTok = myToken $ \case TWatch -> Just (); _ -> Nothing

unwatchTok :: TokenParser ()
unwatchTok = myToken $ \case TUnwatch -> Just (); _ -> Nothing

quitTok :: TokenParser ()
quitTok = myToken $ \case TQuit -> Just (); _ -> Nothing

whileTok :: TokenParser ()
whileTok = myToken $ \case TWhile -> Just (); _ -> Nothing

ifTok :: TokenParser ()
ifTok = myToken $ \case TIf -> Just (); _ -> Nothing

elseTok :: TokenParser ()
elseTok = myToken $ \case TElse -> Just (); _ -> Nothing

parenOpenTok :: TokenParser ()
parenOpenTok = myToken $ \case TParenOpen -> Just (); _ -> Nothing

parenCloseTok :: TokenParser ()
parenCloseTok = myToken $ \case TParenClose -> Just (); _ -> Nothing

curlyBraceOpenTok :: TokenParser ()
curlyBraceOpenTok = myToken $ \case TCurlyBraceOpen -> Just (); _ -> Nothing

curlyBraceCloseTok :: TokenParser ()
curlyBraceCloseTok = myToken $ \case TCurlyBraceClose -> Just (); _ -> Nothing

semicolonTok :: TokenParser ()
semicolonTok = myToken $ \case TSemicolon -> Just (); _ -> Nothing

equalsTok :: TokenParser ()
equalsTok = myToken $ \case TEquals -> Just (); _ -> Nothing

plusTok :: TokenParser ()
plusTok = myToken $ \case TPlus -> Just (); _ -> Nothing

starTok :: TokenParser ()
starTok = myToken $ \case TStar -> Just (); _ -> Nothing

identTok :: TokenParser Name
identTok = myToken $ \case TIdent ident -> Just (Name ident); _ -> Nothing

naturalLitTok :: TokenParser Integer
naturalLitTok = myToken $ \case TNaturalLit n -> Just n; _ -> Nothing
