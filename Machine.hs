{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Machine--change this later cba to do it now
(      
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec,
	push,
        pop,
        pop2,
        returntwo,
        add,
        findState,
        grabState,
        comparevalues  
) where

import Data.Map
import Data.Maybe

type Vname = String

type Val = Int 

type State = Map Vname Val --modelled as : (map k a)

data Instr = LOADI Int --the full instruction set 
           | LOAD String 
           | ADD
           | STORE String
           | JMP Int
           | JMPLESS Int
           | JMPGE Int
        deriving (Eq, Read, Show)

type Stack = [Int]

type Pc = Int

type Config = (Pc,State,Stack)

push :: Int -> Stack -> Stack  
push value xs = value : xs  --pushes items to the "top" of the stack

pop :: Stack -> Int
pop [] = -1 --means the stack is empty
pop list = head list

pop2 :: Stack  -> Stack  
pop2 [] = []
pop2 (x:xs) = xs --returns the tail of the stack excluding the top item

returntwo :: Int -> Stack -> Stack -- returns the first two item's from a stack
returntwo _ [] = []
returntwo a (x:xs)
        | a < 3 = x : returntwo (a + 1) xs
        | otherwise = []

add :: Stack -> Stack -- adds the frist two items in the stack and then returns the altered stack 
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

iexec :: Instr -> Config -> Config 
iexec (LOADI x) (a,b,c) = (a+1,b,push x c)
iexec (LOAD v)  (a,b,c) 
                    | not(Data.Map.null b) = if  Data.Map.valid b then
                                if isNothing(grabState v b) then
                                    (a+1,b,c)
                                else
                                    let value = fromJust(grabState v b)
                                    in (a+1,b,push value c)      --places the value on the stack   
                            else
                            (a,b,c)   
                    | otherwise = (a+1,b,c)
                                            
iexec  ADD   (a,b,c) 
                | length c < 2 = (a+1,b,c)
                | otherwise = (a+1,b,add c)

iexec (STORE v) (a,b,c) 
                = if  Data.Map.valid b then
                        if isNothing(grabState v b) then
                           if Prelude.null c || pop c == -1  then      --checks if the stack is not empty so it can assign the top item to the specified variabel name (state)
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

iexec (JMP i)   (a,b,c) = (a+i+1,b,c) 
iexec (JMPLESS i) (a,b,c)
               | comparevalues c = (a+i+1,b,pop2(pop2 (c)))--checks if the condition is met
               | otherwise = (a+1,b,pop2(pop2 (c)))
 
iexec (JMPGE i) (a,b,c) 
              | not (comparevalues c) = (a+i+1,b,pop2(pop2 (c)))
              | otherwise = (a,b,pop2(pop2 (c)))

exec :: [Instr] -> Config -> Config
exec [] _ = (0,empty,[]) 
exec list (a,b,c)
        | length list <= a = (a,b,c) --check if the program counter exceeds the length of instructions
        | otherwise = exec list (iexec (list !! a) (a,b,c)) --if not the the call is passed recursively to execute firther instructions in the list





