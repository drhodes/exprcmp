module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Op = Add | Sub | Mul | Div


data Expr = BinOp Op Expr Expr
          | EInt Int
          | EDouble Double
          | Symb String
          | Neg Expr
          | Sin Expr
          | Cos Expr
          deriving (Show, Eq)


-- start small.
-- does (3 - 1) == (4 - 2) ?

-- So, 

-- 

onlyLits expr =
  let f x y = onlyLits x && onlyLits y
  in case expr of
       BinOp _ e1 e2 -> f e1 e2
       EInt -> True
       EDouble -> True
       Neg x -> onlyLits x
       Sin x -> onlyLits x
       Cos x -> onlyLits x
                  



-- collapse expressions containing literals.
--collapse 
