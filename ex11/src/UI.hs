{-# LANGUAGE OverloadedStrings #-}

module UI
( uiLoop
, parseCommand
, commandP
) where

import           Prelude hiding (putStrLn)

import           Control.Monad
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T (pack, unlines)
import           Data.Text.IO (putStrLn)
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Prim hiding ((<|>))

import           Eval.AExpr
import           Eval.Env
import           Eval.Stmt
import           Eval.Token
import           MonadRepl

-------------------------------------------------------------------------------
-- Input

data Command
    = Watch [Name]
    | Unwatch [Name]
    | Quit
    | ReplStmt Stmt
    | ReplExpr AExpr
  deriving (Read, Show, Eq, Ord)

parseCommand :: Text -> Either ParseError Command
parseCommand = parse commandP "" <=< tokenize

commandP :: TokenParser Command
commandP = choice [watchP, unwatchP, quitP, try replStmtP, replExprP] <* eof
  where
    watchP    = Watch   <$> (watchTok   *> varListP)
    unwatchP  = Unwatch <$> (unwatchTok *> varListP)
    quitP     = quitTok *> pure Quit
    replStmtP = ReplStmt <$> stmtP
    replExprP = ReplExpr <$> aExprP
    varListP  = many1 identTok

-------------------------------------------------------------------------------
-- Output

watchOutput :: Env -> Set Name -> Text
watchOutput env = T.unlines . map mkOutput . Set.toList
  where
    mkOutput :: Name -> Text
    mkOutput varname
        = fromName varname <> ": " <> displayVal (lookupEnv varname env)

    displayVal :: Maybe Integer -> Text
    displayVal = maybe "undefined" (T.pack . show)

watchOutputM :: ReplT IO Text
watchOutputM = watchOutput <$> getEnv <*> getWatchList

data Continue = Continue | DontContinue
  deriving (Read, Show, Eq, Ord)

-- | First parameter is command's output, second is whether to continue the
-- REPL after this command.
data CommandResult = CommandResult !(Maybe Text) !Continue
  deriving (Read, Show, Eq, Ord)

executeCommand :: (Monad m) => Command -> ReplT m CommandResult
executeCommand (Watch vars)
    = mapM_ addToWatchList vars >> return (CommandResult Nothing Continue)

executeCommand (Unwatch vars)
    = mapM_ removeFromWatchList vars >> return (CommandResult Nothing Continue)

executeCommand Quit
    = return $ CommandResult Nothing DontContinue

executeCommand (ReplStmt s)
    = execStmt s >> return (CommandResult Nothing Continue)

executeCommand (ReplExpr e) = do
    result <- T.pack . show <$> evalAExpr e
    return $ CommandResult (Just result) Continue

-------------------------------------------------------------------------------
-- Logic

interactionCycle :: ReplT IO Continue
interactionCycle = do
    -- Input
    input <- getInputLine ">>> "
    let cmd  = parseCommand input
        cmd' = either (const $ replError "Invalid command") return cmd

    -- Command execution
    (CommandResult output cont) <-
      handleErrors errHandler $ executeCommand =<< cmd'

    -- Watch output
    when (cont == Continue) $ liftBase . putStrLn =<< watchOutputM

    -- Command output
    maybe (return ()) (liftBase . putStrLn) output

    -- Continuation
    return cont
  where
    errHandler msg
        = liftBase (putStrLn msg) >> return (CommandResult Nothing Continue)

whileM :: (Monad m) => m Continue -> m ()
whileM act = do
    continue <- act
    case continue of
      Continue     -> whileM act
      DontContinue -> return ()

uiLoop :: IO ()
uiLoop = void
       . runReplT (emptyEnv, Set.empty)
       $ whileM interactionCycle
