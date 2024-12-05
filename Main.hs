import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

{- 
multi-comment
 -}

-- single comment

-- declaring a variable
hello :: [Char]
hello = "pizza" :: [Char] -- return type

-- declaring function input type and output
doubleIt :: Int -> Int
doubleIt x = x * 2  

-- fibonacci sequence 
fib :: Integer -> Integer
fib 0 = 0 
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

-- concatenate two strings
combine :: String -> String
combine x = "potato " ++ x 

combine2 :: String -> String -> String
combine2 x y = x ++ y

data ExprC
  = NumC Float                 
  | IdC String                   
  | StrC String                  
  | IfC ExprC ExprC ExprC       
  | LamC [String] ExprC          
  | AppC ExprC [ExprC]           
  deriving (Show, Eq)


-- IO() print
main :: IO()
main = do
        print(doubleIt 5)
        print(doubleIt 10)
        print(doubleIt 20)

        print hello
        print(combine hello)
        let hello = "tomato"
        print hello
        print(combine hello)

        print(fib 12)

        print(combine2 "Hello " "World!")


