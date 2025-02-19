-- | This module provides a wrapper around Node.Readline providing a cleaner interface
-- | when prompting for input.
-- |
-- | The provided combinators require `MonadAff (readline :: READLINE | eff) m => MonadAsk Interface m` as a
-- | constraint. So long as this is filled you can use them wherever you like.
-- |
-- | Running your linereader program looks something like this:
-- | runAff_ resultHandler $ runLineReader Nothing myLineReaderProgram
-- |
-- | This specializes the monad stack to `ReaderT Interface (Aff (LineReaderEff e)) a.` then runs it into Aff.
-- |
-- | Example usage:
-- |
-- | ```
-- | main :: forall e. Eff (console :: CONSOLE, readline :: READLINE, exception :: EXCEPTION | e) Unit
-- | main = do
-- |   interface <- createConsoleInterface noCompletion
-- |   runAff_ (either (error <<< show) log) (runReaderT loop interface)
-- |   where
-- |     loop = do
-- |       setPrompt "$ "
-- |       dog <- question "What's your dog's name?\n"
-- |       liftEff <<< log $ "Can I pet " <> dog <> "?"
-- |       str <- readLine
-- |       case uncons str of
-- |         Just {head: 'y'} -> liftEff $ log "Thanks!"
-- |         _ -> liftEff $ log "C'mon! Be a sport about it!"
-- |       loop
-- | ````
module LineReader
  ( runLineReader
  , readLine
  , question
  , module RLExports
  ) where

import Prelude

import Effect.Aff (Aff, bracket)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Data.Maybe (Maybe(..))
import Data.Options (Options)
import Data.Tuple (Tuple(..))
import Node.Process (stdin)
import Node.ReadLine.Aff (Interface, InterfaceOptions, close, createConsoleInterface, createInterface, noCompletion, prompt, setPrompt)
import Node.ReadLine.Aff (InterfaceOptions, createConsoleInterface, createInterface, output, completer, terminal, historySize, Completer, noCompletion) as RLExports
import Node.ReadLine.Aff (question) as RL
import Node.Stream (Readable)

type LineReaderM a = ReaderT Interface Aff a

-- | Run a line reader program.
-- | You can pass your own options and readable stream or pass Nothing to use
-- | the node console.
runLineReader
  :: forall r a e
   . Maybe (Tuple (Readable r) (Options InterfaceOptions))
  -> LineReaderM a
  -> Aff a
runLineReader readeropts prog = bracket buildInterface close runner
  where
    runner :: Interface -> Aff a
    runner = runReaderT prog
    buildInterface :: Aff Interface
    buildInterface = do
      interface <- liftEffect $ case readeropts of
                     Just (Tuple r opts) -> createInterface stdin opts
                     Nothing -> createConsoleInterface noCompletion
      setPrompt "" interface
      pure interface

-- | Read a single line from input.
readLine
  :: forall m
  . MonadAff m
  => MonadAsk Interface m
  => m String
readLine = ask >>= prompt

-- | Prompt for input, then read a line
question
  :: forall m
  . MonadAff  m
  => MonadAsk Interface m
  => String
  -> m String
question txt = do
  interface <- ask
  RL.question txt interface
