module Bool where

data BExpr
    = Const Bool
    | Var String
    | Not BExpr
    | And BExpr BExpr
    | Or  BExpr BExpr
  deriving (Read, Show, Eq, Ord)

eval :: (String -> Bool) -> BExpr -> Bool
eval _         (Const b)   = b
eval envLookup (Var name)  = envLookup name
eval envLookup (Not e)     = not $ eval envLookup e
eval envLookup (And e1 e2) = (eval envLookup e1) && (eval envLookup e2)
eval envLookup (Or  e1 e2) = (eval envLookup e1) || (eval envLookup e2)
