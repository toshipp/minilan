import Text.Parsec
import Text.Parsec.String
import Control.Applicative((<$>), (<*>), liftA2, (<*), (*>))
import qualified Control.Applicative as A ((<|>))
import qualified Data.Map as M

-- TODO :: pp
data Definition = Definition String Expr deriving Show
data Expr = Number Float |
            Variable String |
            -- StringLiteral String |
            FunctionCall Expr [Expr] |
            LambdaE [String] [Definition] [Expr]
            Assignment String Expr |
            Condition Expr Expr (Maybe Expr)
            deriving Show
data Stmt = DefStmt Definition | ExprStmt Expr deriving Show
type Prog = [Stmt]

numberParser :: Parser Float
numberParser = do
  x <- many1 digit
  y <- option [] $ liftA2 (:) (char '.') (many1 digit)
  return . read $ x ++ y

variableParser :: Parser String
variableParser = liftA2 (:) head tails
    where
      exalpha = oneOf "!$%&*+-./:<=>?@^_~"
      head = letter <|> exalpha
      tails = many $ alphaNum <|> exalpha

-- stringParser :: Parser String
-- stringParser = between open close body
--     where
--       open = char '"'
--       close = open
--       body = many $ noneOf ['"']

parenParser :: Parser a -> Parser a
parenParser body = between open close body
    where
      open = char '(' >> spaces
      close = spaces >> char ')'
              
funcCallParser :: Parser Expr
funcCallParser = parenParser $ do
                   fun <- exprParser
                   spaces
                   args <- many $ exprParser <* spaces
                   return $ FunctionCall fun args

lambdaParser :: Parser Expr
lambdaParser = parenParser $ do
                 string "lambda"
                 spaces
                 args <- parenParser $ many $ variableParser <* spaces
                 spaces
                 defs <- many $ defParser <* spaces
                 spaces
                 exps <- many1 $ exprParser <* spaces
                 return $ LambdaE args defs exps

assignParser :: Parser Expr
assignParser = parenParser $ do
                 string "set!"
                 spaces
                 var <- variableParser
                 spaces
                 exp <- exprParser
                 return $ Assignment var exp

condParser :: Parser Expr
condParser = parenParser $ do
               string "if"
               spaces
               cond <- exprParser
               spaces
               thenexpr <- exprParser
               spaces
               elseexpr <- optionMaybe exprParser
               return $ Condition cond thenexpr elseexpr

defParser :: Parser Definition
defParser = parenParser $ do
              string "define"
              spaces
              var <- variableParser
              spaces
              exp <- exprParser
              return $ Definition var exp

-- try を使わないと上手くいかない。
-- ((lambda () 1) 1) をパースしようとすると、先に lambdaParser を適用としようとするが、
-- (( の段階で失敗し、しかも入力を消費しているので、全体が失敗してしまう。
-- try をつかって lambdaParser が失敗した場合、入力を復元すれば後の処理が正常に走るのでOK
exprParser :: Parser Expr
exprParser = Number <$> numberParser <|>
             Variable <$> variableParser <|>
             -- StringLiteral <$> stringParser <|>
             try lambdaParser <|>
             try assignParser <|>
             try condParser <|>
             funcCallParser

stmtParser :: Parser Stmt
stmtParser = DefStmt <$> try defParser <|>
             ExprStmt <$> exprParser

progParser :: Parser Prog
progParser = many stmtp <* eof
  where
    stmtp = between spaces spaces stmtParser
