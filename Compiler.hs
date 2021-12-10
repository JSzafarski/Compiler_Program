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
                    (V f) -> case b of
                            (V g) -> [LOAD g,LOAD f,ADD]

                            (N h) -> [LOADI h ,LOAD f,ADD]


bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp = undefined


ccomp :: Com -> [Instr]
ccomp = undefined