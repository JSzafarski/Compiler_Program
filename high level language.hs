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
            |And Bc Bc -- this might not be Bc  but BExp
            |Less AExp AExp
            deriving (Eq, Read, Show)   

-- aval (Plus (N 3) (V "x")) (fromList [("x" ,0)])

aval :: AExp -> State -> Val
aval (Plus a b) state --i ahve to do a aval function for every combination
                     | a == N c && b == V d = if isNothing(Machine.grabState d state) then -1 else
                                    let f _ = Just((fromJust(Machine.grabState d l) + c))--fucntion that returns v
                                    in (alter f d state)    --nope as we want to return the value not alter the states u mis-read
                     | a == V c && b == N d = if isNothing(Machine.grabState c state) then -1 else
                                    let f _ = Just((fromJust(Machine.grabState C state) + d))--fucntion that returns v
                                    in (alter f c l)
                     | a == V c && b == V d = if isNothing(Machine.grabState c state ) || isNothing(Machine.grabState d state )  then -1 else
                                    fromJust(Machine.grabState (c state)) + fromJust(Machine.grabState (d state))
                     | (a == (N c)) && (b == (N d)) = c + d               
                     |otherwise = -1 --change this later idk
aval (N a) _ = a 
aval (V a) state = if isNothing(Machine.grabState a state) then -1 else fromJust(Machine.grabState a state)  
                         

bval :: BExp -> State -> Bool
bval (Less a b) state = case a of 
                (N c) -> case b of
                        (V d) -> if isNothing(Machine.grabState d state) then False else if (fromJust(Machine.grabState d state) > c) then True else
                                 False
                        (N d) -> if  c > d then True else False
                (V c) -> case b of
                           (V d) -> if isNothing(Machine.grabState c state ) || isNothing(Machine.grabState d state)  then False else
                                    if fromJust(Machine.grabState c state) > fromJust(Machine.grabState d state) then True else False    
                           (N d) -> if isNothing(Machine.grabState c state) then False else if (fromJust(Machine.grabState c state) < d) then True else
                                False
          
bval (And a b) state
        | a == TRUE && b == TRUE = True
        | otherwise = False
bval (Not a) state
        | a == TRUE = False
        | a == FALSE = True               

eval :: Com -> State -> State

eval (Assign a (N b)) state = if isNothing(Machine.grabState a state) then state --make it add a variable if it doesn't exist?
                              else let f _ = Just b
                                   in alter f a state
eval SKIP state = state                                   
 
eval (Seq a b) stack =  let stack2 = case a of
                                (Assign b (N c)) -> eval (Assign b (N c)) stack  
                                (If d e f) -> eval (If d e f) stack
                                (Seq g h) -> eval (Seq g h) stack  
                                --(While i j) -> eval (While i j) stack  
                                SKIP -> eval SKIP stack  
                        in
                            case b of
                                (Assign b (N c)) -> eval (Assign b (N c)) stack2  --passed for evaluaytion Rhs first
                                (If d e f) -> eval (If d e f) stack2
                                (Seq g h) -> eval (Seq g h) stack2  
                                --(While i j) -> eval (While i j) stack2         
                                SKIP -> eval SKIP stack2  
                  
eval (If a b c) state =  case a of
                        (Less e f) -> if bval (Less e f) state then case b of
                                                                (Assign b (N c)) -> eval (Assign b (N c)) state  --passed for evaluaytion Rhs first
                                                                (If d e f) -> eval (If d e f) state
                                                                (Seq g h) -> eval (Seq g h) state  
                                                                --(While i j) -> eval (While i j) stack2         
                                                                SKIP -> eval SKIP state
                                                                else case c of
                                                                (Assign b (N c)) -> eval (Assign b (N c)) state  --passed for evaluaytion Rhs first
                                                                (If d e f) -> eval (If d e f) state
                                                                (Seq g h) -> eval (Seq g h) state 
                                                                --(While i j) -> eval (While i j) stack2         
                                                                SKIP -> eval SKIP state      
                        (And g h) -> if bval (And g h) state then case b of
                                                                (Assign b (N c)) -> eval (Assign b (N c)) state  --passed for evaluaytion Rhs first
                                                                (If d e f) -> eval (If d e f) state
                                                                (Seq g h) -> eval (Seq g h) state  
                                                                --(While i j) -> eval (While i j) stack2         
                                                                SKIP -> eval SKIP state
                                                                else case c of
                                                                (Assign b (N c)) -> eval (Assign b (N c)) state  --passed for evaluaytion Rhs first
                                                                (If d e f) -> eval (If d e f) state
                                                                (Seq g h) -> eval (Seq g h) state 
                                                                --(While i j) -> eval (While i j) stack2         
                                                                SKIP -> eval SKIP state     
eval (While c d) state = case c of 
                          --(Not b) -> if b == TRUE then eval (While c d) -- will never chnage
                          (Less e f) -> if BExp (Less e f) stack case d of
                                (Assign b (N c)) -> eval while( ) (Assign b (N c)) state  --passed for evaluaytion Rhs first
                                (If d e f) -> eval (If d e f) state
                                (Seq g h) -> eval (Seq g h) state 
                                --(While i j) -> eval (While i j) stack2         
                                SKIP -> eval SKIP state 
                                  While (Bexp (Less c d) )
                          (And g h) ->  case d of
                                (Assign b (N c)) -> eval (Assign b (N c)) state  --passed for evaluaytion Rhs first
                                (If d e f) -> eval (If d e f) state
                                (Seq g h) -> eval (Seq g h) state 
                                --(While i j) -> eval (While i j) stack2         
                                SKIP -> eval SKIP state 
                                  While (Bexp (Less c d) )


