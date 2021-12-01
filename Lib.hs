
module Machine
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

--import Data.Map

type Vname = String

type Val = Int 

type State = (Vname,Val)-- maps variable names to values
--zip can pair two things into a tupule like variable and its value

--TODO Task 1.4
data Instr a = LOADI Int -- i think its like this
           | LOAD String 
           | ADD
           | STORE String
           | JMP Int
           | JMPLESS Int
           | JMPGE Int
        --IUndefined (check wtf is thats shit)
        deriving (Eq, Read, Show)--check what this does 

type Stack  = [Int]--staack can be a list that grow right from left 

type Pc = Int--for now i'll leave it like this

type Config = (Pc,[State],Stack)---chnage this later but this is the rougth idea
--program counter,(var name,var value),stack contents

push :: Int -> Stack -> Stack  -- we need to add a item to the stack from  the right hand side ?(check the convention with the guy)
push value xs = value : xs  --adds value to start of list ye?

pop :: Stack -> Int
pop list = head list--add validation for if the list is empty ect

pop2 :: Stack  -> Stack  --not a clean way of doing this
pop2 [] = []
pop2 (x:xs) = xs

returntwo :: Int -> Stack -> Stack --chnage those to stack types
returntwo _ [] = []
returntwo a (x:xs)
        | a < 3 = [x] ++ returntwo (a + 1) xs  
        | otherwise = [] 

add :: [Int] -> [Int]
add [] =[]
add (x:xs) = sum (returntwo (1) (x:xs)) : (drop 1 xs)   

--thing to find,left list,right list ,return full updated list
updateState :: State -> [State] -> [State] -> [State]--original state array + one new state 
updateState _ _ [] = []
updateState st left (x:xs) -- need to ttake into account the bondary condrtitions
        | (fst x == fst st) = left ++ [st] ++ xs 
        | otherwise = updateState st (x:left) xs

grabState :: String -> [State] -> Int--return the value of the state in the state array
grabState _ [] = -1--change this m8
grabState st (x:xs) -- need to ttake into account the bondary condrtitions
        | fst x == st = snd x
        | otherwise = grabState st xs
        

comparevalues :: Stack  -> Bool
comparevalues [] = False
comparevalues stack
        | head (returntwo 1 stack) > last (returntwo 1 stack) = True --if y<x
        | otherwise = False -- if y >= x

--addState :: String -> [State] -> [State]
--addState a b = a:b
        
iexec :: Instr a -> Config -> Config --probably wrong 
iexec (LOADI x) (a,b,c) = (a+1,b,push x [])    
iexec (LOAD v)   (a,b,c) = (a+1,b,push (grabState v b ) c)--need to retrieve data from the array and place on to the stack do later
iexec ADD  (a,b,c) = (a+1,b,add c)
iexec (STORE v) (a,b,c) = (a+1,updateState (v,pop c) [] b,pop2 c)
iexec (JMP i)  (a,b,c) = (a+i,b,c)
iexec (JMPLESS i) (a,b,c)
                | comparevalues c == True = (a+i,b,c)
                | otherwise = (a,b,c)
 
iexec (JMPGE i) (a,b,c) 
                | comparevalues c == False = (a+i,b,c)
                | otherwise = (a,b,c)
             
                
--we need to find the length of the list each iteration
length' :: (Num b) => [a] -> b 
length' [] = 0 
length' xs = sum [1 | _ <- xs] 
                                                

--TODO Task 1.8
exec :: [Instr a] -> Config -> Config--lists of instrsuctions 
--exec [] _ = Config --this is either when the fucntion terminates so it has emptied its list constents and processed them or if the list of instructions is empty and the user provided a config (deal with validation later with maybe) 
exec (x:xs) c = exec xs (iexec x c)
exec [] c = c  

--exec _ 
--exec a b  = iexec head [x|x <- a]--yhink abotu this iteratively




