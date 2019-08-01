module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Op = Add | Sub | Mul | Div deriving (Show, Eq, Ord)

data Expr = BinOp Op Expr Expr
          | EDouble Double
          | Symb String
          | Neg Expr
          | Recip Expr
          | Sin Expr
          | Cos Expr
          deriving (Show, Eq, Ord)

sub = BinOp Sub 
add = BinOp Add
mul = BinOp Mul
divide = BinOp Div

testLHS = sub (EDouble 3) (EDouble 1)
testRHS = sub (EDouble 4) (EDouble 2)

onlyLits expr =
  let f x y = onlyLits x && onlyLits y
  in case expr of
       BinOp _ e1 e2 -> f e1 e2
       EDouble _ -> True
       Symb _ -> False
       Neg x -> onlyLits x
       Sin x -> onlyLits x
       Cos x -> onlyLits x
       Recip x -> onlyLits x

case1 = Neg $ Sin $ Cos (EDouble 0)
case2 = BinOp Add (Neg $ Sin $ Cos (EDouble 0)) (Symb "A")

-- collapse expressions containing literals.
collapse expr =
  case expr of
    Neg (EDouble x) -> EDouble (-x)
    Neg x -> Neg (collapse x)
    BinOp Add (EDouble x) (EDouble y) -> EDouble (x + y)
    BinOp Sub (EDouble x) (EDouble y) -> EDouble (x - y)
    BinOp Mul (EDouble x) (EDouble y) -> EDouble (x * y)
    BinOp Div (EDouble x) (EDouble y) -> EDouble (x / y)
    BinOp op x y -> let xc = collapse x
                        yc = collapse y
                        newExpr = BinOp op xc yc
                    in if onlyLits xc && onlyLits yc
                       then collapse newExpr
                       else newExpr
    EDouble x -> EDouble x
    Recip (EDouble x) -> EDouble $ 1/x
    Recip x -> Recip (collapse x)
    Symb x -> Symb x
    Sin (EDouble x) -> EDouble $ sin x
    Sin x -> collapse (Sin (collapse x))
    Cos (EDouble x) -> EDouble $ cos x
    Cos x -> collapse (Cos (collapse x))

sortExpr expr =
  case expr of
    BinOp Add e1 e2 -> BinOp Add (sortExpr $ min e1 e2) (sortExpr $ max e1 e2)
    BinOp Sub e1 e2 -> sortExpr $ BinOp Add (sortExpr e1) (Neg $ sortExpr e2)
    BinOp Mul e1 e2 -> BinOp Mul (sortExpr $ min e1 e2) (sortExpr $ max e1 e2)
    BinOp Div e1 e2 -> BinOp Mul (sortExpr e1) (Recip $ sortExpr e2)
    EDouble x -> EDouble x
    Symb x -> Symb x
    Recip x -> Recip $ sortExpr x
    Neg x -> Neg $ sortExpr x
    Sin x -> Sin $ sortExpr x
    Cos x -> Cos $ sortExpr x

testEq x y = let lhs = sortExpr $ collapse x
                 rhs = sortExpr $ collapse y
             in lhs == rhs

testCase1 = testEq (sub (Symb "A") (Symb "B")) (add (Symb "A") (Neg (Symb "B")))
testCase2 = testEq (divide (Symb "A") (Symb "B")) (mul (Symb "A") (Recip (Symb "B")))
