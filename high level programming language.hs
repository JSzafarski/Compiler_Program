{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Map

import Machine

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
                | (a == (N c)) && (b == (N d)) =       
                | (a == V c) && (b == (V d)) =     
                | (a == (N c)) && (b == (V d)) = if isNothing(grabState d l) then l else
                                let f _ = Just((fromJust(grabState d l) + d))--fucntion that returns v
                                in (alter f d l)    
                | (a == (V c)) && (b == (N d)) =
                |otherwsie = 

--bval :: BExp -> State -> Bool

--eval:: Com -> State -> State




