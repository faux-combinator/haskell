import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.List.NonEmpty
import FauxCombinator.Parser
import Test.Hspec

data TokenType
  = TokenLparen
  | TokenRparen
  | TokenId
  deriving (Show, Eq)

data Expr = Id String | Call Expr [Expr]
  deriving (Eq, Show)

main :: IO ()
main = hspec $ do
  let notokens = [] :: [Token TokenType]
      lparen = mkToken TokenLparen "("
      rparen = mkToken TokenRparen ")"
      idA = mkToken TokenId "a"
      idB = mkToken TokenId "b"
      idC = mkToken TokenId "c"

  describe "isEOF" $ do
    it "returns true when there are no tokens" $ do
      runParser isEOF notokens  `shouldBe` Right True
      runParser isEOF [lparen] `shouldBe` Right False

  describe "peek" $ do
    it "errors trying to peek an eof" $ do
      runParser peek notokens `shouldBe` Left ParserErrorEOF
    it "returns the token if it got one" $ do
      runParser peek [lparen] `shouldBe` Right lparen
    it "returns the token if it got many" $ do
      runParser peek [lparen, idA, idB, idC, rparen] `shouldBe` Right lparen
    it "doesn't advance" $ do
      runParser (peek >> peek >> peek) [lparen] `shouldBe` Right lparen

  describe "expect" $ do
    it "errors on EOF" $ do
      runParser (expect TokenLparen) notokens `shouldBe` Left ParserErrorEOF
      runParser (expect TokenLparen >> expect TokenRparen) [lparen] `shouldBe` Left ParserErrorEOF
    it "errors on wrong token type" $ do
      runParser (expect TokenRparen) [lparen] `shouldBe` Left (ParserErrorUnexpected TokenLparen TokenRparen)
    it "returns the token" $ do
      runParser (expect TokenLparen) [lparen] `shouldBe` Right lparen
    it "advances" $ do
      runParser ((,) <$> expect TokenLparen <*> expect TokenRparen) [lparen, rparen] `shouldBe` Right (lparen, rparen)

  describe "eitherOf" $ do
    it "errors on EOF" $ do
      runParser (eitherOf (expect TokenLparen) (expect TokenRparen)) notokens `shouldBe` Left ParserErrorEOF
    it "returns lhs" $ do
      runParser (eitherOf (expect TokenLparen) (expect TokenRparen)) [lparen] `shouldBe` Right (Left lparen)
    it "returns rhs (backtracks)" $ do
      runParser (eitherOf (expect TokenLparen) (expect TokenRparen)) [rparen] `shouldBe` Right (Right rparen)

  describe "Applicative.<|>" $ do
    it "errors on EOF" $ do
      runParser (expect TokenLparen <|> expect TokenRparen) notokens `shouldBe` Left ParserErrorEOF
    it "returns lhs" $ do
      runParser (expect TokenLparen <|> expect TokenRparen) [lparen] `shouldBe` Right lparen
    it "returns rhs (backtracks)" $ do
      runParser (expect TokenLparen <|> expect TokenRparen) [rparen] `shouldBe` Right rparen

  describe "attempt" $ do
    it "returns empty on EOF" $ do
      runParser (attempt $ expect TokenId) notokens `shouldBe` Right Nothing
    it "returns empty on wrong token type" $ do
      runParser (attempt $ expect TokenLparen) [rparen] `shouldBe` Right Nothing
    it "backtracks" $ do
      runParser (attempt (expect TokenLparen) >> expect TokenRparen) [rparen] `shouldBe` Right rparen

  describe "Applicative.many" $ do
    it "returns empty on EOF" $ do
      runParser (many $ expect TokenId) notokens `shouldBe` Right []
    it "parses one" $ do
      runParser (many $ expect TokenId) [idA] `shouldBe` Right [idA]
    it "parses several" $ do
      runParser (many $ expect TokenId) [idA, idB, idC] `shouldBe` Right [idA, idB, idC]

  describe "Applicative.some" $ do
    it "errors on EOF" $ do
      runParser (some $ expect TokenId) notokens `shouldBe` Left ParserErrorEOF
    it "parses one" $ do
      runParser (some $ expect TokenId) [idA] `shouldBe` Right [idA]
    it "parses several" $ do
      runParser (some $ expect TokenId) [idA, idB, idC] `shouldBe` Right [idA, idB, idC]

  describe "someNEL" $ do
    it "errors on EOF" $ do
      runParser (someNEL $ expect TokenId) notokens `shouldBe` Left ParserErrorEOF
    it "parses one" $ do
      runParser (someNEL $ expect TokenId) [idA] `shouldBe` Right (idA :| [])
    it "parses several" $ do
      runParser (someNEL $ expect TokenId) [idA, idB, idC] `shouldBe` Right (idA :| [idB, idC])

  describe "composition and backtracking" $ do
    it "can mix and match" $ do
      runParser (expect TokenLparen *> expect TokenId <* expect TokenRparen) [lparen, idA, rparen] `shouldBe` Right idA

  describe "actual program" $ do
    it "can be used to parse lisp-style call expressions" $ do
      runParser call [lparen, idA, idB, idC, rparen] `shouldBe` Right (Call (Id "a") [Id "b", Id "c"])

  describe "monad transformer" $ do
    it "can be used with a user monad" $ do
      runParserT (expect TokenId) [idA] `shouldBe` Just (Right idA)
      let ioact = do
            expect' TokenId
            str <- liftIO $ readFile "test/io.txt"
            eof <- isEOF
            guard eof
            pure str
      runParserT ioact [idA] `shouldReturn` Right "io content\n"

call :: Parser TokenType Expr
call = expr
  where
    expr = idExpr <|> callExpr
    idExpr = Id . unToken <$> expect TokenId
    callExpr = do
      expect' TokenLparen
      callee <- expr
      args <- many expr
      expect' TokenRparen
      pure $ Call callee args
