module Lib
    ( someFunc
    ) where

import qualified Test.QuickCheck as Q 
import Control.Monad


someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Op = Add | Sub | Mul | Div deriving (Eq, Ord)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

instance Q.Arbitrary Op where
  arbitrary = Q.oneof (map return [Add, Sub, Mul, Div])

data Expr = BinOp Op Expr Expr
          | EDouble Double
          | Symb String
          | Neg Expr
          | Recip Expr
          | Sin Expr
          | Cos Expr
          deriving (Eq, Ord)

instance Show Expr where
  show (BinOp op e1 e2) = concat [ "(", (show e1), " "
                                 , show op, " "
                                 , show e2, ")"]
  show (EDouble x) = show x
  show (Symb s) = s
  show (Neg x) = "-" ++ show x
  show (Recip x) = "(1 / " ++ show x ++ ")"
  show (Sin x) = "sin(" ++ show x ++ ")"
  show (Cos x) = "cos(" ++ show x ++ ")"
  


instance Q.Arbitrary Expr where
  arbitrary = do
    Q.oneof [ liftM EDouble Q.arbitrary
            , liftM Symb (Q.oneof (map return ["A", "B", "C"]))
            , liftM3 BinOp Q.arbitrary Q.arbitrary Q.arbitrary
            , liftM Neg Q.arbitrary
            , liftM Recip Q.arbitrary
            , liftM Sin Q.arbitrary
            , liftM Cos Q.arbitrary
            ]

sub = BinOp Sub 
add = BinOp Add
mul = BinOp Mul
divide = BinOp Div

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

-- collapse expressions containing literals.
collapse expr =
  case expr of
    Neg (Neg x) -> collapse x
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
    Recip (Recip x) -> collapse x
    Recip (EDouble x) -> EDouble $ 1/x
    Recip x -> Recip (collapse x)
    Symb x -> Symb x
    Sin (EDouble x) -> EDouble $ sin x
    Sin x -> Sin (collapse x)
    Cos (EDouble x) -> EDouble $ cos x
    Cos x -> Cos (collapse x)

collapseAll x = if collapse x == x
                then x
                else collapseAll (collapse x)

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

-- obey commutativity laws of algebra.
flipExpr expr =
  case expr of 
    BinOp Add e1 e2 -> add (flipExpr e2) (flipExpr e1)
    BinOp Sub e1 e2 -> sub (flipExpr e1) (flipExpr e2)
    BinOp Mul e1 e2 -> mul (flipExpr e2) (flipExpr e1)
    BinOp Div e1 e2 -> divide (flipExpr e1) (flipExpr e2)
    EDouble _ -> expr
    Symb _ -> expr
    Recip x -> Recip (flipExpr x)
    Neg x -> Neg $ flipExpr x
    Sin x -> Sin $ flipExpr x
    Cos x -> Cos $ flipExpr x

testEq x y = let lhs = sortExpr $ collapse x
                 rhs = sortExpr $ collapse y
             in lhs == rhs

prop_commute :: Expr -> Bool
prop_commute e = testEq (flipExpr e) e
  
