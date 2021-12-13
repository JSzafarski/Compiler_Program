{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Lib

acomp :: AExp -> [Instr] --MAKE SURE YOU ARE RETURNING THE LIST OF INSTRUCTIONS IN THE ORDER THAT THE EXAMINE WANT ME TO EVEN THO THEIS DOES NOT INFLUENECE RESULT
acomp (Plus a b)  = case a of 
                    (N c) -> case b of
                            (V d) -> [LOADI c, LOAD d,ADD]
                            (N e) -> [LOADI c, LOADI e,ADD]
                            (Plus f g) -> [LOADI c] ++ acomp (Plus f g) ++ [ADD]
                    (V f) -> case b of
                            (V g) -> [LOAD f,LOAD g,ADD]
                            (N h) -> [LOAD f,LOADI h,ADD]
                            (Plus i j)-> [LOAD f] ++ acomp (Plus i j) ++ [ADD]
                    (Plus k l) -> case b of
                            (V g) -> acomp (Plus k l) ++ [LOAD g,ADD]
                            (N h) -> acomp (Plus k l) ++ [LOADI h,ADD]
                            (Plus i j)-> acomp (Plus k l) ++ acomp (Plus i j) ++ [ADD]
acomp (N a) = [LOADI a] 
acomp (V a) = [LOAD a]                            


-- > bcomp (And ( Bc False ) (Bc True)) True 3
-- >[ JMP 1 , JMP 3]

bcomp :: BExp -> Bool -> Int -> [Instr]

bcomp (Not a) b c = case a of 
                         (Bc d) -> if not d /= b then [] else [JMP c]
                        --(And e f) -> if  not (bcomp (And e f) b c ) /= b then [] else [JMP c]
                         {- (Less g h) -> if b then 
                                            if bcomp (Less g h) b c)!!3 == (JMPGE n)  then 
                                                let (JMPGE jump) = (bcomp (Less g h) b c) !! 3  
                                                in (drop 3 (bcomp (Less g h) b c)) ++ [JMPLESS jump]  
                                            else  
                                                let (JMPLESS jump) = (bcomp (Less g h) b c) !! 3  
                                                in (drop 3 (bcomp (Less g h) b c)) ++ [JMPGE jump]
                                        else 
                                            if bcomp (Less g h) b c == [_,_,JMPGE n]  then 
                                                let (JMPGE jump) = (bcomp (Less g h) b c) !! 3  
                                                in (drop 3 (bcomp (Less g h) b c)) ++ [JMPLESS jump]  
                                            else  
                                                let (JMPLESS jump) = (bcomp (Less g h) b c) !! 3  
                                                in (drop 3 (bcomp (Less g h) b c)) ++ [JMPGE jump]      
                          (Not i) -> if b then
                                        if (bcomp (Not i) b c) == [JMP c] then [] else [JMP c]
                                    else
                                        if (bcomp (Not i) b c) == [JMP c] then [JMP c] else []
  -}
--bcomp (And a b) c d = case a of
--bcomp (Less (V "x" ) (N 5)) True 3
--[LOAD "x" , LOADI 5 ,JMPLESS 3]

bcomp (Less a b) c d = let left = case a of
                                            (N b) -> acomp (N b) 
                                            (V c) -> acomp (V c) 
                                            (Plus d e) -> acomp (Plus d e) 
                                        in
                                        case b of
                                            (N f) -> if c then left ++ acomp (N f) ++ [JMPLESS d] else left ++ acomp (N f) ++ [JMPGE d]
                                            (V g) -> if c then left ++ acomp (V g) ++ [JMPLESS d]  else left ++ acomp (V g) ++ [JMPGE d]
                                            (Plus h i) -> if c then left ++ acomp (Plus h i) ++ [JMPLESS d] else left ++ acomp (Plus h i) ++ [JMPGE d]

bcomp (Bc a) b c = if a then
                        (if b == True then [JMP c] else [])
                    else
                        (if b == False then [JMP c] else []) 

ccomp :: Com -> [Instr]

ccomp (Assign a b)  = case b of
                             (N c) -> acomp (N c) ++ [STORE a]
                             (V d) -> acomp (V d) ++ [STORE a]                  
                             (Plus e g) -> acomp (Plus e g) ++ [STORE a]                    
ccomp SKIP = [JMP 1]                                     
 
ccomp (Seq a b)  = let left = case a of
                                (Assign d e) -> ccomp (Assign d e) 
                                (If f g h) -> ccomp  (If f g h)
                                (Seq i j) -> ccomp  (Seq i j) 
                                (While k l) -> ccomp (While k l) 
                                SKIP -> ccomp SKIP  
                    in case b of
                                (Assign d e) -> left ++ ccomp(Assign d e) 
                                (If f g h) -> left ++ ccomp(If f g h) 
                                (Seq i j) -> left ++ ccomp(Seq i j)   
                                (While k l) -> left ++ ccomp(While k l)         
                                SKIP -> left ++ ccomp SKIP

ccomp (If a b c)  = let y = case b of 
                            (Assign g q) -> ccomp (Assign g q)
                            (If i j k) -> ccomp (If i j k)
                            (Seq l m)  -> ccomp (Seq l m)
                            (While o p) -> ccomp (While o p)     
                            SKIP -> ccomp SKIP                       
                            in 
                                case c of 
                                (Assign g q) -> bcomp a False (length y+1) ++ y ++ [JMP (length (ccomp (Assign g q)))] ++ ccomp (Assign g q)
                                (If i j k) -> bcomp a False (length y+1) ++ y ++ [JMP (length (ccomp (If i j k)))] ++ ccomp (If i j k)
                                (Seq l m)  -> bcomp a False (length y+1) ++ y ++ [JMP (length (ccomp (Seq l m)))] ++ ccomp(Seq l m)
                                (While o p) -> bcomp a False (length y+1) ++ y ++ [JMP (length (ccomp (While o p)))] ++ ccomp (While o p)     
                                SKIP -> bcomp a False (length y+1) ++ y ++ [JMP (length (ccomp  SKIP))] ++ ccomp  SKIP
                                                                
ccomp (While c d)  = case d of
                        (Assign g q) -> let left = ccomp (Assign g q) 
                                        in bcomp c False (length left)  ++left++  [JMP (-length left)]  
                        (If i j k) -> let left = ccomp (If i j k) 
                                        in  bcomp c False (length left)  ++ left++ [JMP (-length left)] 
                        (Seq l m)  -> let left = ccomp (Seq l m) 
                                        in  bcomp c False (length left) ++ left++ [JMP (-length left)] 
                        (While o p) -> let left = ccomp (While o p) 
                                        in bcomp c False (length left)  ++ [JMP (-length left)]        
                        SKIP -> let  left = ccomp SKIP 
                                in bcomp c False (length left) ++left ++ [JMP (-length left)] 
