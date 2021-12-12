{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib (
        Com (..),
        AExp (..),
        BExp (..),
        --Bc (..),
        aval,
        bval,
        eval,
) where

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

--data BooleanConstants = TRUE
                 --     | FALSE 
               -- deriving (Eq, Read, Show)

data BExp =  Not BExp 
            |And BExp BExp -- this might not be Bc  but BExp
            |Less AExp AExp
            |Bc Bool
            deriving (Eq, Read, Show)   

-- aval (Plus (N 3) (V "x")) (fromList [("x" ,0)])

aval :: AExp -> State -> Val
aval (Plus a b) state = case a of  -- do the same here as this is non exaustive.
                        (N c) -> case b of
                                (V d) -> if isNothing(Machine.grabState d state) then -1 else
                                         fromJust(Machine.grabState d state) + c
                                (N d) -> c + d
                                (Plus e f) -> c +  aval (Plus e f) state
                        (V c) -> case b of
                                (V d) -> if isNothing(Machine.grabState c state ) || isNothing(Machine.grabState d state)  then -1 else
                                         fromJust(Machine.grabState c state) + fromJust(Machine.grabState d state)  
                                (N d) -> if isNothing(Machine.grabState c state) then -1 else
                                         fromJust(Machine.grabState c state) + d
                                (Plus e f) -> fromJust(Machine.grabState c state) + aval (Plus e f) state
                        (Plus e f) -> case b of
                                (V g) -> if isNothing(Machine.grabState g state) then -1 else
                                         fromJust(Machine.grabState g state) + aval (Plus e f ) state
                                (N h) -> aval (Plus e f) state + h
                                (Plus i j) -> aval (Plus e f) state  + aval (Plus i j) state              
aval (N a) _ = a 
aval (V a) state = fromMaybe (- 1) (Machine.grabState a state)  
                         
bval :: BExp -> State -> Bool
bval (Less a b) state = let leftValue = case a of
                                (N b) -> aval (N b) state
                                (V c) -> aval (V c) state 
                                (Plus d e) -> aval (Plus d e) state
                        in
                        case b of
                                (N f) -> if leftValue < aval (N f) state then True else False
                                (V g) -> if leftValue < aval (V g) state then True else False
                                (Plus h i) -> if leftValue < aval (Plus h i) state then True else False

bval (And a b) state = let leftValue = case a of
                                (Less c d) -> bval (Less c d) state
                                (And e f) -> bval (And e f) state
                                (Not g) -> bval (Not g) state
                                (Bc h) -> bval (Bc h) state
                       in
                       case b of
                                (Less i j) -> if bval (Less i j) state && leftValue then True else False
                                (And k l) -> if bval (And k l) state && leftValue then True else False 
                                (Not m) -> if bval (Not m) state && leftValue then True else False
                                (Bc n) -> if bval (Bc n) state && leftValue then True else False                                     
        
bval (Not a) state = case a of
                        (Less c d) -> bval (Less c d) state
                        (And e f) -> bval (And e f) state
                        (Not g) -> bval (Not g) state
                        (Bc h) -> bval (Bc h) state
                    
eval :: Com -> State -> State

eval (Assign a b) state = case b of
                             (N c) ->   if isNothing(Machine.grabState a state) then state --make it add a variable if it doesn't exist?
                                        else let f _ = Just c
                                             in alter f a state
                             (V d) ->  if isNothing(Machine.grabState a state) then state --make it add a variable if it doesn't exist?
                                       else let f _ = Machine.grabState d state
                                            in alter f a state                    
                             (Plus e g) -> let f _ = Just(aval (Plus e g) state)
                                           in  alter f a state                   
eval SKIP state = state                                   
 
eval (Seq a b) stack =  let stack2 = case a of
                                (Assign b (N c)) -> eval (Assign b (N c)) stack  
                                (If d e f) -> eval (If d e f) stack
                                (Seq g h) -> eval (Seq g h) stack  
                                (While i j) -> eval (While i j) stack  
                                SKIP -> eval SKIP stack  
                        in
                            case b of
                                (Assign b (N c)) -> eval (Assign b (N c)) stack2  --passed for evaluaytion Rhs first
                                (If d e f) -> eval (If d e f) stack2
                                (Seq g h) -> eval (Seq g h) stack2  
                                (While i j) -> eval (While i j) stack2         
                                SKIP -> eval SKIP stack2  
                  
eval (If a b c) state =  case a of
                        (Less e f) -> if bval (Less e f) state then case b of
                                                                (Assign b (N c)) -> eval (Assign b (N c)) state  --passed for evaluaytion Rhs first
                                                                (If d e f) -> eval (If d e f) state
                                                                (Seq g h) -> eval (Seq g h) state  
                                                                (While i j) -> eval (While i j) state         
                                                                SKIP -> eval SKIP state
                                                                else case c of
                                                                (Assign b (N c)) -> eval (Assign b (N c)) state  --passed for evaluaytion Rhs first
                                                                (If d e f) -> eval (If d e f) state
                                                                (Seq g h) -> eval (Seq g h) state 
                                                                (While i j) -> eval (While i j) state        
                                                                SKIP -> eval SKIP state      
                        (And g h) -> if bval (And g h) state then case b of
                                                                (Assign b (N c)) -> eval (Assign b (N c)) state  --passed for evaluaytion Rhs first
                                                                (If d e f) -> eval (If d e f) state
                                                                (Seq g h) -> eval (Seq g h) state  
                                                                (While i j) -> eval (While i j) state         
                                                                SKIP -> eval SKIP state
                                                                else case c of
                                                                (Assign b (N c)) -> eval (Assign b (N c)) state  --passed for evaluaytion Rhs first
                                                                (If d e f) -> eval (If d e f) state
                                                                (Seq g h) -> eval (Seq g h) state 
                                                                (While i j) -> eval (While i j) state         
                                                                SKIP -> eval SKIP state     
eval (While c d) state = case c of 
                          --(Not b) -> if b == TRUE then eval (While c d) -- will never chnage
                          (Less e f) -> if bval(Less e f) state then case d of
                                                                (Assign g q) -> let state2 = eval (Assign g q) state
                                                                                in  eval (While c d) state2  --passed for evaluaytion Rhs first
                                                                (If i j k) -> let state2 = eval (If i j k) state 
                                                                        in  eval (While c d) state2
                                                                (Seq l m)  -> let state2 = eval (Seq l m) state
                                                                        in  eval (While c d) state2
                                                                (While o p) -> let state2 = eval (While o p) state
                                                                        in eval (While c d) state2         
                                                                SKIP -> let state2 = eval SKIP state --will never terminate
                                                                        in eval (While c d) state2
                                        else state 
                          (And g h) ->  if bval(And g h) state then case d of
                                        (Assign i q) -> let state2 = eval (Assign i q) state
                                                            in  eval (While c d) state2  --passed for evaluaytion Rhs first
                                        (If k l m) -> let state2 = eval (If k l m) state 
                                                      in  eval (While c d) state2
                                        (Seq n o)  -> let state2 = eval (Seq n o) state
                                                      in  eval (While c d) state2
                                        (While p q) -> let state2 = eval (While p q) state
                                                       in eval (While c d) state2         
                                        SKIP -> let state2 = eval SKIP state --will never terminate
                                                in eval (While c d) state2
                                        else state         
 

