{-# LANGUAGE FlexibleContexts #-}

module FauxCombinator.Parser
    ( peek
    , expect
    , attempt
    , zeroOrPlus
    , oneOrPlus
    , runParserT
    , runParser
    , mkToken
    ) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Lazy (StateT(..), MonadState, get, put, evalStateT)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List.NonEmpty (NonEmpty(..))

data Token tt = Token { _tokenType :: tt, _tokenData :: String }
mkToken :: tt -> String -> Token tt
mkToken = Token

data ParserError tt
  = ParserErrorUnexpected tt tt -- (Actual, Expected)
  | ParserErrorEOF

data ParserData tt = ParserData
  { _tokens :: [Token tt]
  , _idx :: Int
  }

type ParserT tt m = StateT (ParserData tt) (ExceptT (ParserError tt) m)
type Parser tt = ParserT tt Identity

next :: (MonadState (ParserData tt) m, MonadError (ParserError tt) m) => m (Token tt)
next = do -- TODO lenses? (idx <<+= 1)
  state <- get
  let idx = _idx state
  if idx >= length (_tokens state)
  then throwError ParserErrorEOF
  else do
    put state {_idx = idx + 1}
    pure $ _tokens state !! idx

peek :: (MonadState (ParserData tt) m) => m (Token tt)
peek = do
  state <- get
  pure $ _tokens state !! _idx state

expect :: (MonadState (ParserData tt) m, MonadError (ParserError tt) m, Eq tt) => tt -> m (Token tt)
expect t = do
  token <- next
  unless (_tokenType token == t) $
    throwError $ ParserErrorUnexpected (_tokenType token) t
  pure token

attempt :: ParserT tt m r -> ParserT tt m (Maybe r)
attempt act = undefined act -- (Just <$> act) <|> pure Nothing

-- ???either = (<|>)
zeroOrPlus :: (Monad m) => ParserT tt m r -> ParserT tt m [r]
zeroOrPlus act = go []
  where
    go xs = do
      got <- attempt act
      case got of
        Just x -> go (x:xs)
        Nothing -> pure xs

oneOrPlus :: (Monad m) => ParserT tt m r -> ParserT tt m (NonEmpty r)
oneOrPlus act = do
  x <- act
  xs <- zeroOrPlus act
  pure $ x :| xs

runParserT :: (Monad m) => ParserT tt m r -> [Token tt] -> m (Either (ParserError tt) r)
runParserT act tokens = runExceptT $ evalStateT act parser
  where parser = ParserData { _tokens = tokens, _idx = 0 }

runParser :: Parser tt r -> [Token tt] -> Either (ParserError tt) r
runParser act tokens = runIdentity $ runParserT act tokens
