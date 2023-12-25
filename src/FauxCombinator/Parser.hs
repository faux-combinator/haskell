{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FauxCombinator.Parser
    ( isEOF
    , peek
    , expect
    , attempt
    , zeroOrPlus
    , oneOrPlus
    , runParserT
    , runParser
    , mkToken
    , Token
    , ParserError(..)
    ) where

import Control.Applicative ((<|>), Alternative(..))
import Control.Monad (when, unless, MonadPlus(..), mzero)
import Control.Monad.Error.Class (MonadError, throwError)
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

data ParserError tt
  = ParserErrorUnexpected tt tt -- (Actual, Expected)
  | ParserErrorEOF
  deriving (Show, Eq)

data ParserData tt = ParserData
  { _tokens :: [Token tt]
  , _idx :: Int
  }

newtype ParserT tt m r = ParserT { unParser :: StateT (ParserData tt) (ExceptT (ParserError tt) m) r }
  deriving (Functor, Applicative, Monad)
type Parser tt = ParserT tt Identity

instance (MonadPlus m) => Alternative (ParserT tt m) where
  empty = ParserT $ StateT $ \_ -> undefined
  a <|> b = ParserT $ StateT $ \s -> ExceptT undefined

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

eitherOf :: (Functor m) => ParserT tt m a -> ParserT tt m b -> ParserT tt m (Either a b)
eitherOf a b = (Left <$> a) <|> (Right <$> b)

attempt :: (Monad m) => ParserT tt m r -> ParserT tt m (Maybe r)
attempt act = (Just <$> act) <|> (pure Nothing)

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
runParserT act tokens = runExceptT $ evalStateT (unParser act) parser
  where parser = ParserData { _tokens = tokens, _idx = 0 }

runParser :: Parser tt r -> [Token tt] -> Either (ParserError tt) r
runParser act tokens = runIdentity $ runParserT act tokens
