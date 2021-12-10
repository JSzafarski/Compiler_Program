{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Map
import Data.Maybe
import Machine
--re used pre define types and type sysnnyms from Machine.hs
data Com  = Assign String AExp--x is the expression and (N a) is the variable v   
            | Seq  Com Com  -- Denotes a program which first executes c1 and then c2 v
            | If BExp Com Com --c1 id b is true ,c2 else
            | While BExp Com -- Executes c as long as b evaluates to TRUE
            | SKIP 
            deriving (Eq, Read, Show)

data AExp =   N Int
            | V String
            | Plus AExp AExp  -- where a and b are non-deterministic types
            deriving (Eq, Read, Show)

data Bc = TRUE
        | FALSE 
        deriving (Eq, Read, Show)

data BExp =  Not Bc 
            |And Bc Bc
            |Less AExp AExp
            deriving (Eq, Read, Show)   

-- aval ( Plus (N3) (V "x" )) (fromList [("x" ,0)])

  
{- aval :: AExp -> State -> Val
aval (Plus a b) state --i ahve to do a aval function for every combination
                     | (a == N c) && (b == (V d)) = if isNothing(Machine.grabState d state) then -1 else
                                    let f _ = Just((fromJust(Machine.grabState d l) + c))--fucntion that returns v
                                 in (alter f d state)    --nope as we want to return the value not alter the states u mis-read
                     | (a == (V c)) && (b == (N d)) =
                        if isNothing(Machine.grabState c state) then -1 else
                                let f _ = Just((fromJust(Machine.grabState C state) + d))--fucntion that returns v
                                in (alter f c l)
                     | (a == (V c)) && (b == (V d)) = if isNothing(Machine.grabState c state ) || isNothing(Machine.grabState d state )  then -1 else
                               fromJust(Machine.grabState (c state)) + fromJust(Machine.grabState (d state))

                     | (a == (N c)) && (b == (N d)) = c + d               
                     |otherwise = -1 --change this later idk
aval (N a) _ = a 
aval (V a) state = if if isNothing(Machine.grabState a state) then -1 else fromJust(Machine.grabState d state) -}
                         

{- bval :: BExp -> State -> Bool
bVal (Less a b) state
        | (a == (N c)) && (b == (V d)) = if isNothing(Machine.grabState d state) then False else
               if (fromJust(Machine.grabState d state) > c) then True else
                       False
        | (a == (V c)) && (b == (N d)) =
               if isNothing(Machine.grabState c state) then False else
                let f _ = Just((fromJust(Machine.grabState C state) + d))--fucntion that returns v
               in (alter f c state)--nope
        | (a == (V c)) && (b == (V d)) = if isNothing(Machine.grabState c state ) || isNothing(Machine.grabState d state)  then False else
                if fromJust(Machine.grabState (c state)) > fromJust(Machine.grabState (d state)) then True else False
        | (a == (N c)) && (b == (N d)) = if  c > d then True else False               
        |otherwise = False --change this later idk
bVal (And a b) 
        | a == TRUE && b == TRUE = TRUE
        | otherwise = FALSE
bVal (Not a) 
        | a == TRUE = FALSE
        | a == FALSE = TRUE    -}          

eval :: Com -> State -> State

-- > eval ( Seq ( Assign "x" (N 5)) ( Assign "x" (N 6)))                               
--If (Less (V ”x”) (N 3)) (Assign ”x” (N 3)) SKIP
--eval (Seq (If (Less (V ”x”) (N 3)) (Assign ”x” (N 3)) SKIP) (Assign "x" (N 6)))
--eval (Seq  (Assign a (N b)) (Assign c (N d))) state = state
-- > eval (Assign "x" (N 5)) (fromList [("x" ,0)])

eval (Assign a (N b)) state = if isNothing(Machine.grabState a state) then state --consider other opitons?
                              else let f _ = Just b
                                   in alter f a state
--eval (SKIP) state = state                                   
 
eval (Seq a b) stack =  let stack2 = case a of
                                (Assign b (N c)) -> eval (Assign b (N c)) stack  
                        --(If d e f) -> eval (If d e f) stack
                        --(Seq g h) -> eval (Seq g h) stack  
                        --(While i j) -> eval (While i j) stack  
                        --(SKIP) -> eval (SKIP) stack  s
                        in
                         case b of
                                (Assign b (N c)) -> eval (Assign b (N c)) stack2  --passed for evaluaytion Rhs first
                        --(If d e f) ->   =  
                        --(Seq g h) ->    = 
                        --(While i j) ->  =        
                        --(SKIP) ->       =  
                  
 
--eval (If a b c) state =  case a of
             --           (Not d) - > if d == TRUE then else
              --          (Less e f) - > if   
              --          (AND g h) - >      
--eval (While c d) state = case c of 
                   --     (Not b) - > if b == TRUE then eval (While c d) -- will never chnage
                   --     (Less c d)  - > 
                    --    (AND e f) - > 
                     --    case d of



