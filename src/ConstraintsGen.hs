{-# LANGUAGE GADTs #-}
module ConstraintsGen where

import Control.Monad.Trans.Reader
import Control.Monad.State.Lazy
import Control.Applicative

import Script.AST
import Data.Bitcoin.Script.Types

import qualified Data.ByteString as BS

genConstraints :: ScriptAST -> BConstraints
genConstraints script
  = foldl1 OrConstr
  $ map cnstrs
  $ genBuildStates script

genBuildStates :: ScriptAST -> [BuildState]
genBuildStates script
  = map (\s -> execState s initBuildState)
  $ runReader (genCnstrs script) (return ())

type Ident = Int
type OpIdent = String
data Expr where
  ConstInt :: Int -> Expr
  ConstBS  :: BS.ByteString -> Expr
  Length :: Expr -> Expr
  Var   :: Ident -> Expr
  Hash  :: Expr -> Expr
  Op    :: Expr -> OpIdent -> Expr -> Expr
  deriving (Show)

e2i :: Expr -> Int
e2i (ConstInt i) = i
e2i e = error $ "Error: e2i for expr not implemented: " ++ show e

e2l :: Expr -> Int
e2l (ConstBS bs) = BS.length bs
e2l e = error $ "Error: e2l for expr not implemented: " ++ show e

false = ExprConstr $ Op (ConstInt 0) "==" (ConstInt 1)
true  = ExprConstr $ Op (ConstInt 0) "==" (ConstInt 0)

data BConstraints where
  ExprConstr :: Expr -> BConstraints
  AndConstr  :: BConstraints -> BConstraints -> BConstraints
  OrConstr   :: BConstraints -> BConstraints -> BConstraints
  LeafConstr :: BConstraints

instance Show BConstraints where
  show (ExprConstr e) = show e
  show (AndConstr b0 b1) = show b0 ++ " && " ++ show b1
  show (OrConstr  b0 b1) = show b0 ++ " ||\n" ++ show b1
  show LeafConstr = "True"

type Stack = [Expr]
data BuildState =
  BuildState {
    cnstrs    :: BConstraints,
    stack     :: Stack,
    altStack  :: Stack,
    freshV    :: Ident,
    freshAltV :: Ident
  }
initBuildState =
  BuildState {
    cnstrs    = LeafConstr,
    stack     = [],
    altStack  = [],
    freshV    = 0,
    freshAltV = 0
  }

instance Show BuildState where
  show s = "BuildState {\n\tcnstrs: " ++ show (cnstrs s) ++
           ",\n\tstack: " ++ show (stack s) ++
           ",\n\taltStack: " ++ show (altStack s) ++
           "}\n"

type BranchBuilder a = State BuildState a
type ConstraintBuilder a = Reader (BranchBuilder ()) a


cnstrsMod :: (BConstraints -> BConstraints) -> BranchBuilder ()
cnstrsMod f = do
  st <- get
  put $ st {cnstrs = f $ cnstrs st}


--
-- Main stack operations
--

genV :: BranchBuilder Expr
genV = do
  st <- get
  put $ st {freshV = freshV st + 1}
  return $ Var (freshV st)

popStack :: BranchBuilder Expr
popStack = do
  st <- get
  if null (stack st)
    then genV
    else do
      st <- get
      put $ st {stack = tail $ stack st}
      return $ head (stack st)

popsStack :: Int -> BranchBuilder [Expr]
popsStack n =
  reverse <$> replicateM n popStack

pushStack :: Expr -> BranchBuilder ()
pushStack e = do
  st <- get
  put $ st {stack = e : stack st}

pushsStack :: [Expr] -> BranchBuilder ()
pushsStack es = mapM_ pushStack es

peakStack :: BranchBuilder Expr
peakStack = do
  v <- popStack
  pushStack v
  return v

--
-- Alternative stack operations
--

genAltV :: BranchBuilder Expr
genAltV = do
  st <- get
  put $ st {freshAltV = freshAltV st + 1}
  return $ Var (freshAltV st)

popAltStack :: BranchBuilder Expr
popAltStack = do
  st <- get
  if null (altStack st)
    then genAltV
    else do
      st <- get
      put $ st {altStack = tail $ altStack st}
      return $ head (altStack st)

pushAltStack :: Expr -> BranchBuilder ()
pushAltStack e = do
  st <- get
  put $ st {altStack = e : altStack st}

pushsAltStack :: [Expr] -> BranchBuilder ()
pushsAltStack es = mapM_ pushAltStack es


--withReader_ :: r' -> Reader r' a -> Reader r a
withReader_ x m =
  withReader (const x) m

genCnstrs :: ScriptAST -> ConstraintBuilder [BranchBuilder ()]
genCnstrs (ScriptITE b0 b1 cont) = do
  ss0 <- withReader (>> stModITE True)  (genCnstrs b0)
  ss1 <- withReader (>> stModITE False) (genCnstrs b1)
  concat <$> mapM (\s -> local (const s) (genCnstrs cont)) (ss0 ++ ss1)
genCnstrs (ScriptOp OP_EQUAL cont) = do
  ss0 <- withReader (>> stModEq True)  (genCnstrs cont)
  ss1 <- withReader (>> stModEq False) (genCnstrs cont)
  return $ ss0 ++ ss1
genCnstrs (ScriptOp OP_EQUALVERIFY cont) = do
  genCnstrs (ScriptOp OP_EQUAL (ScriptOp OP_VERIFY cont))
genCnstrs (ScriptOp op cont) = do
  withReader (>> stModOp op) $ genCnstrs cont
genCnstrs ScriptTail = do
  s <- ask
  return [s]

stModEq :: Bool -> BranchBuilder ()
stModEq b = do
  v_2 <- popStack
  v_1 <- popStack
  let nc = ExprConstr $ if b
                          then Op v_1 "==" v_2
                          else Op v_1 "/=" v_2
  cnstrsMod (AndConstr nc)
  if b
    then pushStack (ConstInt 1)
    else pushStack (ConstInt 0)


stModITE :: Bool -> BranchBuilder ()
stModITE b = do
  v <- popStack
  let nc = ExprConstr $ if b
                          then Op v "/=" (ConstInt 0)
                          else Op v "==" (ConstInt 0)
  cnstrsMod (AndConstr nc)

stModOp :: ScriptOp -> BranchBuilder ()
stModOp (OP_PUSHDATA bs _) = pushStack (ConstBS bs)

stModOp OP_0 = pushStack (ConstBS BS.empty)
stModOp OP_1NEGATE = pushStack (ConstInt (-1))
stModOp OP_1 = pushStack (ConstInt 1)
stModOp OP_2 = pushStack (ConstInt 2)
stModOp OP_3 = pushStack (ConstInt 3)
stModOp OP_4 = pushStack (ConstInt 4)
stModOp OP_5 = pushStack (ConstInt 5)
stModOp OP_6 = pushStack (ConstInt 6)
stModOp OP_7 = pushStack (ConstInt 7)
stModOp OP_8 = pushStack (ConstInt 8)
stModOp OP_9 = pushStack (ConstInt 9)
stModOp OP_10 = pushStack (ConstInt 10)
stModOp OP_11 = pushStack (ConstInt 11)
stModOp OP_12 = pushStack (ConstInt 12)
stModOp OP_13 = pushStack (ConstInt 13)
stModOp OP_14 = pushStack (ConstInt 14)
stModOp OP_15 = pushStack (ConstInt 15)
stModOp OP_16 = pushStack (ConstInt 16)

stModOp OP_NOP = return ()

stModOp OP_VERIFY = do
  v <- popStack
  let nc = ExprConstr $ Op v "/=" (ConstInt 0)
  cnstrsMod (AndConstr nc)
stModOp OP_RETURN = cnstrsMod (AndConstr false)

stModOp OP_TOALTSTACK = do
  v <- popStack
  pushAltStack v
stModOp OP_FROMALTSTACK = do
  v <- popAltStack
  pushStack v
stModOp OP_DEPTH = do
  depth <- length . stack <$> get
  pushStack (ConstInt depth)
stModOp OP_DROP = popStack >> return ()
stModOp OP_DUP = popStack >>= \v -> pushsStack [v,v]
stModOp OP_NIP = do
  v <- popStack
  popStack
  pushStack v
stModOp OP_OVER = do
  v2 <- popStack
  v1 <- popStack
  pushsStack [v1,v2,v1]
stModOp OP_PICK = do
  n <- e2i <$> popStack
  es <- popsStack (n-1)
  e_n <- popStack
  pushsStack (e_n : es)
  pushStack e_n
stModOp OP_ROLL = do
  n <- e2i <$> popStack
  es <- popsStack (n-1)
  e_n <- popStack
  pushsStack es
  pushStack e_n
stModOp OP_ROT = do
  v_3 <- popStack
  v_2 <- popStack
  v_1 <- popStack
  pushsStack [v_2, v_3, v_1]
stModOp OP_SWAP = popsStack 2 >>= \vs -> pushsStack (reverse vs)
stModOp OP_TUCK = do
  v_2 <- popStack
  v_1 <- popStack
  pushsStack [v_2, v_1, v_2]
stModOp OP_2DROP = popsStack 2 >> return ()
stModOp OP_2DUP = popsStack 2 >>= \vs -> pushsStack (vs ++ vs)
stModOp OP_3DUP = popsStack 3 >>= \vs -> pushsStack (vs ++ vs)
stModOp OP_2OVER = popsStack 4 >>= \vs -> pushsStack (vs ++ drop 2 vs)
stModOp OP_2ROT = popsStack 6 >>= \vs -> pushsStack (drop 2 vs ++ take 2 vs)
stModOp OP_2SWAP = popsStack 4 >>= \vs -> pushsStack (drop 2 vs ++ take 2 vs)

stModOp OP_SIZE = peakStack >>= \v -> pushStack (Length v)
stModOp OP_1ADD = popStack >>= \v -> pushStack (Op v "+" (ConstInt 1))
stModOp OP_1SUB = popStack >>= \v -> pushStack (Op v "-" (ConstInt 1))
stModOp OP_NEGATE = popStack >>= \v -> pushStack (Op v "*" (ConstInt (-1)))

-- DISABLED OP_CODES
stModOp op | any (== op) disabledOps = cnstrsMod (AndConstr false)

stModOp op =
  error $ "Error, no stModOp implementation for operator: " ++ show op


disabledOps =
  [
  OP_CAT,
  OP_SUBSTR,
  OP_LEFT,
  OP_RIGHT,

  OP_INVERT,
  OP_AND,
  OP_OR,
  OP_XOR,

  OP_2MUL,
  OP_2DIV,

  OP_VERIF
  ]
