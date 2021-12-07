{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Map
import Data.Maybe

import Machine
--re used pre define types and type sysnnyms from Machine.hs
data Com a = Assign String AExp--x is the expression and (N a) is the variable v   
            | Seq a a -- Denotes a program which first executes c1 and then c2
            | If Bc a a --c1 id b is true ,c2 else
            | While Bc a -- Executes c as long as b evaluates to TRUE
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

aval :: AExp -> State -> Val
aval (Plus a b) l --we have to ch4eck the types of a and b to know what operations to do      
                | (a == (N c)) && (b == (V d)) = if isNothing(Machine.grabState d l) then -1 else
                                let f _ = Just((fromJust(Machine.grabState d l) + c))--fucntion that returns v
                                in (alter f d l)    --nope as we want to return the value not alter the states u mis-read
                | (a == (V c)) && (b == (N d)) =
                        if isNothing(Machine.grabState c l) then -1 else
                                let f _ = Just((fromJust(Machine.grabState C l) + d))--fucntion that returns v
                                in (alter f c l)--nope
                | (a == (V c)) && (b == (V d)) = if isNothing(Machine.grabState c l ) || isNothing(Machine.grabState d l )  then -1 else
                                fromJust(Machine.grabState (c l)) + fromJust(Machine.grabState (d l))

                | (a == (N c)) && (b == (N d)) = c + d               
                |otherwise = -1 --change this later idk

bval :: BExp -> State -> Bool
bVal (Less a b) =
bVal (And a b) =
bVal (Not a) =              

eval :: Com -> State -> State
eval (Assign a b) =
eval (Seq a b) =        
eval (If a b c) =
eval (While) =
eval (SKIP) =



