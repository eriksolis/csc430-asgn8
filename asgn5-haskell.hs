data ExprC
  = NumC Float                 
  | IdC String                   
  | StrC String                  
  | IfC ExprC ExprC ExprC       
  | LamC [String] ExprC          
  | AppC ExprC [ExprC]           
  deriving (Show, Eq)

data Value
  = NumV Float
  | StrV String
  | BoolV Bool
  | CloV [String] ExprC Env
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

add_to_env :: [String] -> [Value] -> Env -> Env
add_to_env [] [] env = env
add_to_env (str:rest_str) (val:rest_val) env = (str, val):add_to_env rest_str rest_val env

interp_primv :: String -> [Value] -> Env -> Value
interp_primv "+" vals env = NumV 0

interp :: ExprC -> Env -> Value
interp (NumC num) env = NumV num
interp (StrC str) env = StrV str
interp (IfC cond l r) env = if (interp cond env) == (BoolV True)
  then interp l env
  else interp r env
interp (IdC id) env = from_env id env
interp (LamC params body) env = CloV params body env
interp (AppC fn args) env = interp_app (interp fn env) args env

interp_args :: [ExprC] -> Env -> [Value]
interp_args (x:xs) env = (interp x env):(interp_args xs env)
interp_args [] _ = []

interp_app :: Value -> [ExprC] -> Env -> Value
interp_app (CloV params body clo_env) args env = do
  let new_env = add_to_env params (interp_args args env) env
  interp body new_env
interp_app (PrimV sym) args env = interp_primv sym (interp_args args env) env

main :: IO()
main = do
  print (interp (NumC 2) top_env)
  print (interp (StrC "abc") top_env)
  print (interp (IdC "true") top_env)
  print (interp (IfC (IdC "true") (NumC 3) (NumC 4)) top_env)
  print (interp (LamC ["x", "y"] (NumC 3)) top_env)
  print (interp (AppC (LamC ["x"] (IfC (IdC "x") (IdC "true") (IdC "false"))) [(IdC "true")]) top_env)