{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib--change this later cba to do it now
(      
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map
import Data.Maybe

type Vname = String

type Val = Int 

type State = Map Vname Val --modelled as : (map k a)

data Instr = LOADI Int 
           | LOAD String 
           | ADD
           | STORE String
           | JMP Int
           | JMPLESS Int
           | JMPGE Int
        --IUndefined (check wtf is thats shit)
        deriving (Eq, Read, Show)--check what this does 

type Stack = [Int]

type Pc = Int

type Config = (Pc,State,Stack)

push :: Int -> Stack -> Stack  -- we need to add a item to the stack from  the right hand side ?(check the convention with the guy)
push value xs = value : xs  --adds value to start of list ye?

pop :: Stack -> Int
pop [] = -1 --means the stack is empty
pop list = head list--add validation for if the list is empty ect

pop2 :: Stack  -> Stack  --not a clean way of doing this ( maybe to let and in or : do)
pop2 [] = []
pop2 (x:xs) = xs

returntwo :: Int -> Stack -> Stack
returntwo _ [] = []
returntwo a (x:xs)
        | a < 3 = x : returntwo (a + 1) xs
        | otherwise = []

add :: Stack -> Stack
add [] = []
add (x:xs) = sum (returntwo 1 (x:xs)) : Prelude.drop 1 xs   


findState :: String -> State -> Bool 
findState st state 
        | isJust(Data.Map.lookup st state) = False
        | otherwise = True

grabState :: String -> State -> Maybe Int--return the value of the state in the state array
grabState  = Data.Map.lookup
        
comparevalues :: Stack  -> Bool
comparevalues [] = False
comparevalues stack
        | head (returntwo 1 stack) > last (returntwo 1 stack) = True --if y<x
        | otherwise = False -- if y >= x

iexec :: Instr -> Config -> Config --validate inputs implement "maybe" (rmeove the a argument somehow)
iexec (LOADI x) (a,b,c) = (a+1,b,push x c)
iexec (LOAD v)  (a,b,c) 
                    | not(Data.Map.null b) = if  Data.Map.valid b then
                                if isNothing(grabState v b) then
                                    (a+1,b,c)
                                else
                                    let value = fromJust(grabState v b)
                                    in (a+1,b,push value c)        
                            else
                            (a,b,c)   
                    | otherwise = (a+1,b,c) -- mini validation,verify that the map is not empty
                                            
iexec  ADD   (a,b,c) 
                | length c < 2 = (a+1,b,c)
                | otherwise = (a+1,b,add c)--check if stack is non empty

iexec (STORE v) (a,b,c) 
                = if  Data.Map.valid b then
                        if isNothing(grabState v b) then
                           if Prelude.null c || pop c == -1  then      
                                (a+1,b,c)
                           else (a+1,insert v (pop c) b, pop2 c) -- insert :: Ord k => k -> a -> Map k a -> Map k a
                        else
                           if pop c == -1  then      
                                (a+1,b,c)
                                else
                                        let f _ = Just(pop c)--fucntion that returns v
                                        in (a+1,alter f v b,pop2 c)         
                else
                        (a+1,b,c)   

iexec (JMP i)   (a,b,c) = (a+i+1,b,c) --for all jump condtion validate that a+i is within the bounds of list length (also not negative)
iexec (JMPLESS i) (a,b,c)
               | comparevalues c = (a+i+1,b,c)
               | otherwise = (a+1,b,c)
 
iexec (JMPGE i) (a,b,c) 
              | not (comparevalues c) = (a+i+1,b,c)
              | otherwise = (a,b,c)
             
exec :: [Instr] -> Config -> Config--lists of instrsuctions
exec [] _ = (0,empty,[]) --DEAFULT RETRUN CONFIG FILE ,handle jmp instruction when its illegal values 
exec list (a,b,c)
        | length list <= a = (a,b,c)--check if the pc has been incrememtned if not then halt and output the state that is atm
        | otherwise = exec list (iexec (list !! a) (a,b,c))

--thoughts:
--if there is somethign that the interpreter doesnt like it still carries on with execution by incremementing the pc
--check exec function if it runs list of instructions properly again        





