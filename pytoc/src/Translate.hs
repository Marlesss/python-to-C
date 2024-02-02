module Translate
 ( pyToC
 ) where

import qualified Python.Grammar as PG
import qualified Clang.Grammar as CG
import Data.Maybe
import Data.List(intercalate)
import Data.Functor
import Control.Monad.State.Lazy

defaultCProgram :: CG.Program
defaultCProgram = CG.Program [CG.Library "stdio.h"] []

type NameBinds = [(PG.Expr, (CG.Expr, [CG.Expr] -> [CG.Expr]))]

defaultNameBinds :: NameBinds
defaultNameBinds =
  [ (PG.Var "print", (CG.Var "printf", \args -> CG.StrVal (intercalate " " (replicate (length args) "%s")) : args)) ]

fromStatement :: CG.Statement -> CG.Program
fromStatement s = CG.Program [] [s]

pyToC :: PG.Program -> CG.Program
pyToC pyProg = defaultCProgram <> (fst $ runState (translate pyProg) defaultNameBinds)

translate :: PG.Program -> State NameBinds CG.Program
translate [] = pure mempty
translate (statement:rest) = do
  prog <- statementToC statement
  restCProg <- translate rest
  return $ prog <> restCProg

removingNameBind :: String -> State NameBinds a -> State NameBinds a
removingNameBind name = withState $ filter $ (/= PG.Var name) . fst

statementToC :: PG.Statement -> State NameBinds CG.Program
statementToC (PG.If cond prog) = do
  condC <- exprToC cond
  outscopeNameBinds <- get
  let (CG.Program impB mainB) = evalState (translate prog) outscopeNameBinds
  return $ CG.Program impB [CG.If condC mainB]
statementToC (PG.IfElse cond prog1 prog2) = do
  condC <- exprToC cond
  outscopeNameBinds <- get
  let (CG.Program impB1 mainB1) = evalState (translate prog1) outscopeNameBinds
  let (CG.Program impB2 mainB2) = evalState (translate prog2) outscopeNameBinds
  return $ CG.Program (impB1 <> impB2) [CG.IfElse condC mainB1 mainB2]
statementToC (PG.Define varName typeName)        = removingNameBind varName $ pure $
                                                     fromStatement $ CG.Define varName (typeToC typeName)
statementToC (PG.DefineSet varName typeName val) = removingNameBind varName $ do
  valC <- exprToC val
  pure $ fromStatement $ CG.DefineSet varName (typeToC typeName) valC
statementToC (PG.Set varName val) = do
  valC <- exprToC val
  pure $ fromStatement $ CG.Set varName valC
statementToC (PG.Expr expr) = do
  valC <- exprToC expr
  pure $ fromStatement $ CG.Expr valC

exprToC :: PG.Expr -> State NameBinds CG.Expr
exprToC (PG.Call e args) = do
  valC <- exprToC e
  bindedNames <- get
  let (eC, argsFunc) = fromMaybe (valC, id) $ lookup e bindedNames
  argsC <- mapM exprToC args
  pure $ CG.Call eC (argsFunc argsC)
exprToC e@(PG.Var name)      = do
  bindedNames <- get
  pure $ fromMaybe (CG.Var name) $ fst <$> lookup e bindedNames
exprToC (PG.Plus e1 e2)    = binaryExprToC CG.Plus e1 e2
exprToC (PG.Minus e1 e2)   = binaryExprToC CG.Plus e1 e2
exprToC (PG.Times e1 e2)   = binaryExprToC CG.Plus e1 e2
exprToC (PG.Div e1 e2)     = binaryExprToC CG.Plus e1 e2
exprToC (PG.Brack e)       = exprToC e <&> CG.Brack
exprToC (PG.IntVal val)    = pure $ CG.IntVal val
exprToC (PG.StrVal val)    = pure $ CG.StrVal val

binaryExprToC :: (CG.Expr -> CG.Expr -> CG.Expr) -> PG.Expr -> PG.Expr -> State NameBinds CG.Expr
binaryExprToC func e1 e2 = do
  e1C <- exprToC e1
  e2C <- exprToC e2
  pure $ func e1C e2C

typeToC :: PG.PrimitiveType -> CG.PrimitiveType
typeToC PG.Int = CG.Int
typeToC PG.String = CG.String
