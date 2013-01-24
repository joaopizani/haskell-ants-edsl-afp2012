module Game.UUAntGen.AntImperative where

import qualified Data.Map as M
import Data.Map ((!))
import Data.Maybe (fromJust)
import Data.List (delete)
import Control.Monad.Supply

import Game.UUAntGen.AntInstruction
import Game.UUAntGen.AntTransformation
import Game.UUAntGen.AntBasic

-- Composing AntStrategies. Sequencing, loop, conditionals, etc.

-- | Sequencing two AntStrategies. Means that s2 will be executed after s1
(>>-) :: AntStrategy -> AntStrategy -> AntStrategy
s1 >>- s2 = do
    s1' <- s1  -- extracting instruction blocks from within the Supply monad
    s2' <- s2
    let s1final = instructions s1' ! final s1'
        s1new   = M.insert (final s1') (replaceDefaultState (initial s2') s1final) (instructions s1')
    return $ AntStrategy' (s1new `M.union` (instructions s2')) (initial s1') (final s2')


-- | Datatype representing all the possible tests to be performed in a conditional AntStrategy
-- | TODO we want to join conditions
data AntTest
    = TrySense Direction Condition
    | TryRandomEqZero Int
    | Not AntTest
    deriving Show


-- | While block, is given a test and a strategy for the body
aWhile :: AntTest -> AntStrategy -> AntStrategy
aWhile (Not (TrySense d c))        = aMkWhile $ \t f -> Sense d f t c
aWhile (TrySense d c)                   = aMkWhile $ \t f -> Sense d t f c
aWhile (Not (TryRandomEqZero p))   = aMkWhile $ \t f -> Flip p f t
aWhile (TryRandomEqZero p)              = aMkWhile $ \t f -> Flip p t f

-- Helper function to aWhile. Produces a conditional loop block, given a conditional
-- assembly instruction and a strategies for the loop body
aMkWhile :: (AntState -> AntState -> AntInstruction) -> AntStrategy -> AntStrategy
aMkWhile condi b = do
    idx   <- supply  -- getting the unique id for the conditional instruction
    gidx  <- supply  -- getting the unique id for the ghost instruction
    b'    <- b  -- extracting the instruction blocks from the Supply monad
    let bidx         = initial b'
        testi        = condi bidx gidx
        ghosti       = Ghost gidx idx idx
        bPlusG       = M.insert gidx ghosti (instructions $ replaceFinal idx b')
    return $ AntStrategy' (M.insert idx testi bPlusG) idx gidx


-- | IfThenElse, given a test and two strategies: one for the true branch and one for the false
aIfThenElse :: AntTest -> AntStrategy -> AntStrategy -> AntStrategy
aIfThenElse (Not (TrySense d c))       = aMkIfThenElse $ \t f -> Sense d f t c
aIfThenElse (TrySense d c)                  = aMkIfThenElse $ \t f -> Sense d t f c
aIfThenElse (Not (TryRandomEqZero p))  = aMkIfThenElse $ \t f -> Flip p f t
aIfThenElse (TryRandomEqZero p)             = aMkIfThenElse $ \t f -> Flip p t f

-- Helper function to aIfThenElse. Produces a conditional strategy given an assembly instruction
-- and two strategies. Introduces a "Ghost" instruction to serve as return point from both branches
aMkIfThenElse :: (AntState -> AntState -> AntInstruction) -> AntStrategy -> AntStrategy -> AntStrategy
aMkIfThenElse condi ts fs = do
    idx <- supply  -- getting the unique id for the testing instruction
    gidx <- supply -- getting the unique id for the ghost instruction
    ts' <- ts  -- extracting the instruction blocks from the Supply monad
    fs' <- fs
    let (tidx, fidx) = (initial ts', initial fs')
        ghosti       = Ghost gidx (final ts') (final fs')
        testi        = condi tidx fidx
        fPlusT       = (instructions $ replaceFinal gidx ts') `M.union` (instructions $ replaceFinal gidx fs')
    return $ AntStrategy' (M.insert idx testi (M.insert gidx ghosti fPlusT)) idx gidx


