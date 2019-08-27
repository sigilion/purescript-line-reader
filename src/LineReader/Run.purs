-- | This module provides an interface to read/write from a console interface using Run effects.
-- |
-- | Example usage:
-- |
-- | ```
-- | getEssay :: forall e. Aff (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | e) (Array String)
-- | getEssay = runLineReader Nothing (getWords 10)
-- |   where
-- |     getWords :: Int -> Run (aff :: AFF (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | e), reader :: READER Interface) (Array String)
-- |     getWords n
-- |       | n <= 0 = pure []
-- |       | otherwise = do
-- |           newWords <- question $ ("Please enter at least " <> show n <> "words" :: String)
-- |           let splitWords = words newWords
-- |           (append splitWords <$> getWords (n - length splitWords))
-- | ```
module LineReader.Run
  ( LineReaderF
  , runLineReader
  , lineReaderToAff
  , readLine
  , question
  , LINEREADER
  , module RLExports
  ) where

import Prelude

import Effect.Aff (Aff)
import Control.Monad.Reader (ReaderT(ReaderT))
import Data.Maybe (Maybe)
import Data.Options (Options)
import Data.Tuple (Tuple)
import LineReader (runLineReader) as LR
import Node.ReadLine.Aff (Interface, InterfaceOptions, prompt)
import Node.ReadLine.Aff (InterfaceOptions, createConsoleInterface, createInterface, output, completer, terminal, historySize, Completer, noCompletion) as RLExports
import Node.ReadLine.Aff (question) as A
import Node.Stream (Readable)
import Run (AFF, FProxy, Run, SProxy(SProxy), interpretRec, lift, liftAff, on, runBaseAff, send)
import Run.Reader (READER, ask, runReader)

data LineReaderF r
  = ReadLine (String -> r)
  | Question String (String -> r)

derive instance functorLineReaderF :: Functor LineReaderF

type LINEREADER = FProxy LineReaderF

_linereader = SProxy :: SProxy "linereader"

handleLineReader :: forall r. LineReaderF ~> Run (reader :: READER Interface, aff :: AFF | r)
handleLineReader = case _ of
  ReadLine handleInput -> do
    interface <- ask
    response <- liftAff $ prompt interface
    pure $ handleInput response
  Question q handleInput -> do
    interface <- ask
    response <- liftAff $ A.question q interface
    pure $ handleInput response

readLine :: forall r. Run (linereader :: LINEREADER | r) String
readLine = lift _linereader (ReadLine identity)

question :: forall r. String -> Run (linereader :: LINEREADER | r) String
question q = lift _linereader (Question q identity)

runLineReader
  :: forall r
   . Run (linereader :: LINEREADER, reader :: READER Interface, aff :: AFF | r)
  ~> Run (reader :: READER Interface, aff :: AFF | r)
runLineReader = interpretRec (on _linereader handleLineReader send)

-- | Run a Line Reader computation from the Run Monad into the Aff Monad
lineReaderToAff
  :: forall r a
   . Maybe (Tuple (Readable r) (Options InterfaceOptions))
  -> Run (reader :: READER Interface, aff :: AFF) a
  -> Aff a
lineReaderToAff opts prog =
  LR.runLineReader opts $ ReaderT runprog
    where
      runprog :: Interface -> Aff a
      runprog interface = runBaseAff (runReader interface prog)
