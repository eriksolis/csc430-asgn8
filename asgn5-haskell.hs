data ExprC
  = NumC Float                 
  | IdC String                   
  | StrC String                  
  | IfC ExprC ExprC ExprC       
  | LamC [ExprC] ExprC          
  | AppC ExprC [ExprC]           
  deriving (Show, Eq)

data Value
  = NumV Float
  | StrV String
  | BoolV Bool
  | CloV [ExprC] ExprC Env
  | PrimV String
  deriving (Show, Eq)

type Env = [(String, Value)]

top_env = [("+", (PrimV "+")),
          ("-", (PrimV "-")),
          ("*", (PrimV "*")),
          ("/", (PrimV "/")),
          ("true", (BoolV True)),
          ("false", (BoolV False)),
          ("<=", (PrimV "<="))]

from_env :: String -> Env -> Value
from_env str (x:xs) = if str == fst x
  then snd x
  else from_env str xs
from_env str [] = error "Could not find symbol in environment"

interp :: ExprC -> Env -> Value
interp (NumC num) env = NumV num
interp (StrC str) env = StrV str
interp (IfC cond l r) env = if (interp cond env) == (BoolV True)
  then interp l env
  else interp r env
interp (IdC id) env = from_env id env
interp (LamC params body) env = CloV params body env

main :: IO()
main = do
  print (interp (NumC 2) top_env)
  print (interp (StrC "abc") top_env)
  print (interp (IdC "true") top_env)
  print (interp (IfC (IdC "true") (NumC 3) (NumC 4)) top_env)
  print (interp (LamC [(IdC "x"), (IdC "y")] (NumC 3)) top_env)