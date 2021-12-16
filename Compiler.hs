{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter

acomp :: AExp -> [Instr] 
acomp (Plus a b)  = case a of  -- checks all the possible cases the user may wish to enter 
                    (N c) -> case b of
                            (V d) -> [LOADI c, LOAD d,ADD]
                            (N e) -> [LOADI c, LOADI e,ADD]
                            (Plus f g) -> [LOADI c] ++ acomp (Plus f g) ++ [ADD] --recursive call if the arguments are nested more deeply
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

bcomp :: BExp -> Bool -> Int -> [Instr]

bcomp (Not a) b c = case a of {- treats the third paramater as a way of fliping the states so if the bool "b" 
                        was True the it flips it to false an re-evaluates the bolean axperession with the flipped boolean value. as that is the same as doing Not of that expression-}
                         (Bc d) -> if not d /= b then [] else [JMP c] 
                         (And e f) -> if b == True then
                                        bcomp (And e f) (False) c 
                                      else
                                        bcomp (And e f) True c       
                         (Less g h) -> if b == True then 
                                        bcomp (Less g h) False c  
                                       else
                                        bcomp (Less g h) True c                      
                         (Not i) -> if b ==True then
                                        bcomp (Not i) False c
                                    else
                                        bcomp (Not i) True c

bcomp (And a b) c d = case a of
                                (Less e f) -> case b of
                                                (Less g h) -> bcomp (Less e f) c (length (bcomp (Less g h) c d) + d) ++ bcomp (Less g h) c d  
                                                (Bc l) -> if c then 
                                                                if l then [JMP ((length (bcomp(Less e f) c d))+(d))] ++ bcomp(Less e f) c d else
                                                                [JMP d] ++bcomp(Less e f) c d
                                                            else
                                                                if l then bcomp(Less e f) c d else
                                                                    [JMP ((length (bcomp(Less e f) c d))+(d))] ++ bcomp(Less e f) c d
                                (Bc i) -> case b of
                                                (Less g h) -> if c then 
                                                                if i then [JMP ((length (bcomp(Less g h) c d))+(d))] ++ bcomp(Less g h) c d else
                                                                    [JMP d] ++ bcomp(Less g h) c d
                                                            else
                                                                if i then [JMP d] ++ bcomp(Less g h) c d else
                                                                    [JMP ((length (bcomp(Less g h) c d))+(d))] ++ bcomp(Less g h) c d    
                                                (Bc j) -> if c then if j then
                                                                    if i then [JMP d] else [JMP 1,JMP d]
                                                                else 
                                                                    if i then [] else []
                                                                else 
                                                                if j then
                                                                    if i then [] else [JMP d]
                                                                else 
                                                                    if i then [JMP d] else [JMP d] 

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
                    in case b of -- concats the first seq of instructions with the second set of instructions 
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
                                        in bcomp c False (length left+1)  ++left++  [JMP ((-1) + (-(length left)) - length (bcomp c False (length left+1)))]  
                        (If i j k) -> let left = ccomp (If i j k) 
                                        in  bcomp c False (length left+1)  ++ left++ [JMP ((-1) + (-(length left)) - length (bcomp c False (length left+1)))]  
                        (Seq l m)  -> let left = ccomp (Seq l m) 
                                        in  bcomp c False (length left+1) ++ left++ [JMP ((-1) + (-(length left)) - length (bcomp c False (length left+1)))]  
                        (While o p) -> let left = ccomp (While o p) 
                                        in bcomp c False (length left+1)  ++left++ [JMP ((-1) + (-(length left)) - length (bcomp c False (length left+1)))]        
                        SKIP -> let  left = ccomp SKIP 
                                in bcomp c False (length left+1) ++left ++ [JMP ((-1) + (-(length left)) - length (bcomp c False (length left+1)))]  
