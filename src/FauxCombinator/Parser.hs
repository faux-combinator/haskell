{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FauxCombinator.Parser
    ( isEOF
    , peek
    , expect
    , expect'
    , eitherOf
    , someNEL
    , ParserT
    , Parser
    , runParserT
    , runParser
    , mkToken
    , unToken
    , Token
    , ParserError(..)
    ) where

import Control.Applicative (Alternative (..), (<|>))
import Control.Monad (unless, when)
import Control.Monad.Error.Class (MonadError, catchError, throwError)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Lazy (MonadState, StateT (..), evalStateT, get, put)
import Data.Functor (($>))
import Data.Functor.Identity (Identity, runIdentity)
import Data.List.NonEmpty (NonEmpty (..))

data Token tt = Token
  { _tokenType :: tt
  , _tokenData :: String }
  deriving stock (Show, Eq)

mkToken :: tt -> String -> Token tt
mkToken = Token
unToken :: Token tt -> String
unToken = _tokenData

data ParserError tt
  = ParserErrorUnexpected tt tt -- (Actual, Expected)
  | ParserErrorEOF
  | ParserErrorNoParse
  deriving stock (Show, Eq)

data ParserData tt = ParserData
  { _tokens :: [Token tt]
  , _idx    :: Int
  }

newtype ParserT tt m r = ParserT { unParser :: StateT (ParserData tt) (ExceptT (ParserError tt) m) r }
  deriving newtype (Functor, Applicative, Monad, MonadState (ParserData tt), MonadError (ParserError tt), MonadIO)

type Parser tt = ParserT tt Identity

-- This means that parser-combinators' Control.Applicative.Combinators all work nicely here
instance (Monad m) => Alternative (ParserT tt m) where
  empty = throwError ParserErrorNoParse
  a <|> b = a `catchError` const b

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
expect' t = expect t $> ()

-- eitherA in Protolude
eitherOf :: (Alternative m) => m a -> m b -> m (Either a b)
eitherOf a b = (Left <$> a) <|> (Right <$> b)

someNEL :: (Alternative m) => m r -> m (NonEmpty r)
someNEL act = (:|) <$> act <*> many act

runParserT :: (Monad m) => ParserT tt m r -> [Token tt] -> m (Either (ParserError tt) r)
runParserT act tokens = runExceptT $ evalStateT (unParser act) parser
  where parser = ParserData { _tokens = tokens, _idx = 0 }

runParser :: Parser tt r -> [Token tt] -> Either (ParserError tt) r
runParser act tokens = runIdentity $ runParserT act tokens
