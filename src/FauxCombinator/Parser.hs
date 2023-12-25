{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FauxCombinator.Parser
    ( isEOF
    , peek
    , expect
    , expect'
    , attempt
    , eitherOf
    , eitherOf'
    , zeroOrPlus
    , oneOrPlus
    , ParserT
    , Parser
    , runParserT
    , runParser
    , mkToken
    , unToken
    , Token
    , ParserError(..)
    ) where

import Control.Monad (when, unless)
import Control.Monad.Error.Class (MonadError, throwError, catchError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.State.Lazy (StateT(..), MonadState, get, put, evalStateT)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List.NonEmpty (NonEmpty(..))

data Token tt = Token
  { _tokenType :: tt
  , _tokenData :: String }
  deriving (Show, Eq)

mkToken :: tt -> String -> Token tt
mkToken = Token
unToken :: Token tt -> String
unToken = _tokenData

data ParserError tt
  = ParserErrorUnexpected tt tt -- (Actual, Expected)
  | ParserErrorEOF
  deriving (Show, Eq)

data ParserData tt = ParserData
  { _tokens :: [Token tt]
  , _idx :: Int
  }

type ParserT tt m = StateT (ParserData tt) (ExceptT (ParserError tt) m)
type Parser tt = ParserT tt Identity

isEOF :: (MonadState (ParserData tt) m) => m Bool
isEOF = do
  state <- get
  return $ _idx state >= length (_tokens state)

next :: (MonadState (ParserData tt) m, MonadError (ParserError tt) m) => m (Token tt)
next = do
  state <- get
  eof <- isEOF
  when eof $ throwError ParserErrorEOF
  let idx = _idx state
  put state {_idx = idx + 1} -- TODO lenses? (idx <<+= 1)
  pure $ _tokens state !! idx

peek :: (MonadState (ParserData tt) m, MonadError (ParserError tt) m) => m (Token tt)
peek = do
  state <- get
  eof <- isEOF
  when eof $ throwError ParserErrorEOF
  pure $ _tokens state !! _idx state

expect :: (MonadState (ParserData tt) m, MonadError (ParserError tt) m, Eq tt) => tt -> m (Token tt)
expect t = do
  token <- next
  unless (_tokenType token == t) $
    throwError $ ParserErrorUnexpected (_tokenType token) t
  pure token

expect' :: (MonadState (ParserData tt) m, MonadError (ParserError tt) m, Eq tt) => tt -> m ()
expect' t = expect t *> pure ()

eitherOf :: (Monad m) => ParserT tt m a -> ParserT tt m b -> ParserT tt m (Either a b)
eitherOf a b = (Left <$> a) `catchError` const (Right <$> b)

eitherOf' :: (Monad m) => ParserT tt m a -> ParserT tt m a -> ParserT tt m a
eitherOf' a b = a `catchError` const b

attempt :: (Monad m) => ParserT tt m a -> ParserT tt m (Maybe a)
attempt act = (Just <$> act) `catchError` const (pure Nothing)

zeroOrPlus :: (Monad m) => ParserT tt m r -> ParserT tt m [r]
zeroOrPlus act = go []
  where
    go xs = do
      got <- attempt act
      case got of
        Just x -> go (x:xs)
        Nothing -> pure $ reverse xs

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
