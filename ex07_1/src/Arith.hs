module Arith where

data AExp a
    = Const a
    | Var   String
    | Plus  (AExp a) (AExp a)
    | Mult  (AExp a) (AExp a)
  deriving (Read, Show, Eq, Ord)

prettyPrint :: (Show a) => AExp a -> String
prettyPrint (Const n)    = show n
prettyPrint (Var name)   = '$' : name
prettyPrint (Plus e1 e2) =  "(" ++ prettyPrint e1 ++ " + " ++ prettyPrint e2 ++ " )"
prettyPrint (Mult e1 e2) =  "(" ++ prettyPrint e1 ++ " * " ++ prettyPrint e2 ++ " )"

prettyPrintRP :: (Show a) => AExp a -> String
prettyPrintRP (Const n)    = show n
prettyPrintRP (Var name)   = '$' : name
prettyPrintRP (Plus e1 e2) =  prettyPrintRP e1 ++ " " ++ prettyPrintRP e2 ++ " + "
prettyPrintRP (Mult e1 e2) =  prettyPrintRP e1 ++ " " ++ prettyPrintRP e2 ++ " * "

rightAssociate :: AExp a -> AExp a
rightAssociate (Plus (Plus e1 e2) e3) =
    let e1' = rightAssociate e1
        e2' = rightAssociate e2
        e3' = rightAssociate e3
    in  Plus e1' (Plus e2' e3')
rightAssociate (Mult (Mult e1 e2) e3) =
    let e1' = rightAssociate e1
        e2' = rightAssociate e2
        e3' = rightAssociate e3
    in  Mult e1' (Mult e2' e3')
rightAssociate e = e
