{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Interpreter (
        Com (..),
        AExp (..),
        BExp (..),
        aval,
        bval,
        eval,
) where

import Data.Map
import Data.Maybe
import Machine

data Com  = Assign String AExp--x is the expression and (N a) is the variable v   
            | Seq  Com Com  -- Denotes a program which first executes c1 and then c2 v
            | If BExp Com Com --c1 id b is true ,c2 else
            | While BExp Com -- Executes c as long as b evaluates to TRUE
            | SKIP 
            deriving (Eq, Read, Show)

data AExp =   N Val -- re-used from machine.hs
            | V Vname
            | Plus AExp AExp  -- where a and b are non-deterministic types
            deriving (Eq, Read, Show)

data BExp =  Not BExp 
            |And BExp BExp -- this might not be Bc  but BExp
            |Less AExp AExp
            |Bc Bool
            deriving (Eq, Read, Show)   

-- aval (Plus (N 3) (V "x")) (fromList [("x" ,0)])

aval :: AExp -> State -> Val
aval (Plus a b) state = case a of  -- check the possible input combinations and if there are nested plus command the it uses recusion
                        (N c) -> case b of
                                (V d) -> if isNothing(Machine.grabState d state) then -1 else
                                         fromJust(Machine.grabState d state) + c
                                (N e) -> c + e
                                (Plus f g) -> c +  aval (Plus f g) state
                        (V h) -> case b of
                                (V i) -> if isNothing(Machine.grabState i state ) || isNothing(Machine.grabState h state)  then -1 else
                                         fromJust(Machine.grabState i state) + fromJust(Machine.grabState h state)  
                                (N j) -> if isNothing(Machine.grabState h state) then -1 else
                                         fromJust(Machine.grabState h state) + j
                                (Plus k l) -> fromJust(Machine.grabState h state) + aval (Plus k l) state
                        (Plus m n) -> case b of
                                (V o) -> if isNothing(Machine.grabState o state) then -1 else
                                         fromJust(Machine.grabState o state) + aval (Plus m n ) state
                                (N p) -> aval (Plus m n) state + p
                                (Plus q r) -> aval (Plus m n) state  + aval (Plus q r) state              
aval (N a) _ = a 
aval (V a) state = fromMaybe (- 1) (Machine.grabState a state)  
                         
bval :: BExp -> State -> Bool
bval (Less a b) state = let leftValue = case a of --checks the left value a and if the result of that is less then b then returns true and flase conversely 
                                (N b) -> aval (N b) state
                                (V c) -> aval (V c) state 
                                (Plus d e) -> aval (Plus d e) state
                        in
                        case b of
                                (N f) -> if leftValue < aval (N f) state then True else False
                                (V g) -> if leftValue < aval (V g) state then True else False
                                (Plus h i) -> if leftValue < aval (Plus h i) state then True else False

bval (And a b) state = let leftValue = case a of -- if both values of a and b are true then it retruns true otherwise false for any other case combnation
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
                        (Less c d) -> not (bval (Less c d) state) -- uses haskells "not" to flip the inputs to opposite state 
                        (And e f) -> not(bval (And e f) state)
                        (Not g) -> not(bval (Not g) state)
                        (Bc h) -> not(bval (Bc h) state)
                    
eval :: Com -> State -> State

eval (Assign a b) state = case b of
                             (N c) ->   if isNothing(Machine.grabState a state) then state --check if the variable exists
                                        else let f _ = Just c --creates a function to assign the mapping
                                             in alter f a state --assigns the new value in the Map to a selected variable name
                             (V d) ->  if isNothing(Machine.grabState a state) then state 
                                       else let f _ = Machine.grabState d state
                                            in alter f a state                    
                             (Plus e g) -> let f _ = Just(aval (Plus e g) state)
                                           in  alter f a state                   
eval SKIP state = state     --returns the same state as input as it does nothing                              
 
eval (Seq a b) stack =  let stack2 = case a of
                                (Assign b c ) -> eval (Assign b c ) stack   -- passes to the appropriate functions to evaluate the result
                                (If d e f) -> eval (If d e f) stack
                                (Seq g h) -> eval (Seq g h) stack  
                                (While i j) -> eval (While i j) stack  
                                SKIP -> eval SKIP stack  
                        in
                            case b of
                                (Assign b c) -> eval (Assign b c) stack2  --passed the stack after executing first left half 
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
eval (While c d) state = if bval c state then case d of
                                        (Assign g q) -> let state2 = eval (Assign g q) state --checks the comman inside the while loop and evaluates it 
                                                        in  eval (While c d) state2   -- evaluates the while command again with the state altered by thefirst command and it's passed recursively  
                                        (If i j k) -> let state2 = eval (If i j k) state 
                                                in  eval (While c d) state2
                                        (Seq l m)  -> let state2 = eval (Seq l m) state
                                                in  eval (While c d) state2
                                        (While o p) -> let state2 = eval (While o p) state
                                                in eval (While c d) state2         
                                        SKIP -> state {- let state2 = eval SKIP state --will never terminate 
                                                in eval (While c d) state2 -}
                                        else state 
                              
 

