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

-- > bcomp (And ( Bc False ) (Bc True)) True 3
-- >[ JMP 1 , JMP 3]

bcomp :: BExp -> Bool -> Int -> [Instr]

bcomp (Less a b) c d = case a of 

bcomp (Not a) b c = case a of 
                        (Bc b) -> if not (bcomp (Bc b)) != b then [] else [JMP c]
                        (And c d) -> if  not (bcomp (And c d)) != b then [] else [JMP c]
                        (Less e f) -> if not (bcomp (Less e f)) != b then [] else [JMP c] 
                        (Not g) -> if not(bcomp (Not g)) != b then [] else [JMP c]
bcomp (And a b) c d = case a of
                        (Bc b) -> let y = bcomp(Bc b) in case b of 
                                            (Bc b) -> if y && bcomp(Bc b) then else 
                                            (And c d) -> if y && bcomp(And c d) then else 
                                            (Less e f) -> if y && bcomp(Less e f) then else 
                                            (Not g) -> if y && bcomp(Not g) then else 
                        (And c d) ->
                        (Less e f) ->
                        (Not g) ->
--bcomp (Less a b) c d = case a of
bcomp (Bc a) b c = case a of 
                True -> if b == True then [JMP c] else []
                False ->if b == False then [JMP c] else []

ccomp :: Com -> [Instr]
ccomp = undefined