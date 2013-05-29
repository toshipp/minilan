import Text.Parsec
import Text.Parsec.String
import Control.Applicative((<$>), (<*>), liftA2, (<*), (*>))
import qualified Control.Applicative as A ((<|>))
import qualified Data.Map as M
import Data.IORef
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad
import Data.Maybe
import Data.List
import System.IO

-- TODO :: pp
data Lambda = Lambda [String] [Definition] [Expr] deriving Show 
data Definition = Definition String Expr deriving Show
data Expr = Number Float |
            Variable String |
            -- StringLiteral String |
            FunctionCall Expr [Expr] |
            LambdaE Lambda |
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
                 defs <- (try $ many $ defParser <* spaces) <|> (return [])
                 spaces
                 exps <- many1 $ exprParser <* spaces
                 return . LambdaE $ Lambda args defs exps

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

-- TODO : prity printer
data Procedure = Primitive String |
                 LambdaV Lambda Env
instance Show Procedure where
  show (Primitive name) = "<primitive " ++ name ++ ">"
  show (LambdaV _ _) = "<lambda>"

data Value = NumberV Float |
             Tuple Value Value |
             Undefined |
             Nil |
             ProcedureV Procedure |
             TrueV |
             FalseV
             deriving Show

type Binding = M.Map String Value
type Env = [IORef Binding]
updateBindings :: String -> Value -> Env -> IO ()
updateBindings var val (x:xs) = do
  bind <- readIORef x
  writeIORef x $ M.insert var val bind

makeEnv :: Env -> Binding -> IO Env
makeEnv parent binding = do
  bind <- newIORef binding
  return (bind:parent)

findVarEnv :: String -> Env -> MaybeT IO Env
findVarEnv var [] = MaybeT . return $ Nothing
findVarEnv var env@(x:xs) = do
  bind <- lift $ readIORef x
  if M.member var bind
    then lift $ return env
    else findVarEnv var xs

findBoundValue :: String -> Env -> MaybeT IO Value
findBoundValue var env = do
  e <- findVarEnv var env
  bind <- lift . readIORef . head $ e
  return $ bind M.! var

evalDefine :: Definition -> Env -> IO ()
evalDefine (Definition var expr) env = do
  val <- evalExpr expr env
  updateBindings var val env

apply :: Procedure -> [Value] -> IO Value
apply (Primitive "+") [(NumberV a), (NumberV b)] = return . NumberV $ a + b
apply (Primitive "-") [(NumberV a), (NumberV b)] = return . NumberV $ a - b
apply (Primitive "*") [(NumberV a), (NumberV b)] = return . NumberV $ a * b
apply (Primitive "/") [(NumberV a), (NumberV b)] = return . NumberV $ a / b
apply (Primitive "=") [(NumberV a), (NumberV b)]
  | a == b = return TrueV
  | otherwise = return FalseV
apply (Primitive _) _ = error "Not supported!"
apply (LambdaV (Lambda params defs exprs) env) args
  | length params == length args = do
    new_env <- makeEnv env $ M.fromList $ zip params args
    forM_ defs (flip evalDefine new_env)
    results <- forM exprs (flip evalExpr new_env)
    if length results == 0
      then return Undefined
      else return $ last results
  | otherwise = error "Number of params not matched!"

evalExpr :: Expr -> Env -> IO Value
evalExpr (Number x) _ = return $ NumberV x
evalExpr (Variable var) env = do
  val <- runMaybeT $ findBoundValue var env
  case val of
    Just x -> return x
    Nothing -> error $ "Not found: " ++ var
evalExpr (FunctionCall fexpr aexprs) env = do
  fun <- evalExpr fexpr env
  args <- mapM (flip evalExpr env) aexprs
  case fun of
    ProcedureV p -> apply p args
    _ -> error "Function call needs procedure!"
evalExpr (LambdaE lambda) env = return . ProcedureV $ LambdaV lambda env
evalExpr (Assignment var expr) env = do
  set_env <- runMaybeT $ findVarEnv var env
  case set_env of
    Just e -> do
      val <- evalExpr expr env
      updateBindings var val e
      return Undefined
    Nothing -> error $ "Not found: " ++ var
evalExpr (Condition condexpr thenexpr elseexpr) env = do
  cond <- evalExpr condexpr env
  if isTrue cond
    then evalExpr thenexpr env
    else fromMaybe (return Undefined) $ (flip evalExpr env <$> elseexpr)

isTrue :: Value -> Bool
isTrue Nil = False
isTrue Undefined = False
isTrue FalseV = False
isTrue _ = True
  
evalStmt :: Stmt -> Env -> IO Value
evalStmt (DefStmt def) env = do
  evalDefine def env
  return Undefined
evalStmt (ExprStmt expr) env = evalExpr expr env

makeRootEnv :: IO Env
makeRootEnv = makeEnv [] $
              M.fromList [("+", ProcedureV $ Primitive "+"),
                          ("-", ProcedureV $ Primitive "-"),
                          ("*", ProcedureV $ Primitive "*"),
                          ("/", ProcedureV $ Primitive "/"),
                          ("=", ProcedureV $  Primitive "=")]

main :: IO ()
main = do
  env <- makeRootEnv
  forever $ do
    line <- getLine
    case parse stmtp "stdin" line of
      Left err -> putStrLn . show $ err
      Right stmt -> do
        value <- evalStmt stmt env
        putStrLn . show $ value
  where
    stmtp = stmtParser <* spaces <* eof

-- (define fib (lambda (n) (if (= n 0) 1 (* (fib (- n 1)) n))))
-- (fib 10)