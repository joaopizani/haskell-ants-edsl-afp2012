module Game.UUAntGen.AntImperative where

import           Control.Monad.Supply (supply)
import           Data.Map             ((!))
import qualified Data.Map             as M

import Game.UUAntGen.AntAssembly
import Game.UUAntGen.AntDeepEmbedded
import Game.UUAntGen.AntInstruction
import Game.UUAntGen.AntTransformation



-- | Empty strategy, will be eliminated when sequeced (iSeq) with another one
iEmpty :: AntImperative
iEmpty = iList []


-- Iterative programming-like constructs for building ant strategies. Sequencing, loop,
-- conditionals, etc. Every constructs has two corresponding functions. One (prefixed with "a")
-- which builds the ant assembly itself (of type AntStrategy), and one (prefixed with "i")
-- which builds the deep-embedded EDSL (of type AntImperative).

-- Sequencing AntStrategies. Means that s2 will be executed after s1
iSeq :: AntImperative -> AntImperative -> AntImperative
iSeq (IList s1) (IList s2) = IList (s1 ++ s2)  -- first 3 patterns eliminate empty strategies
iSeq (IList s1) s2         = IList $ s1 ++ [s2]
iSeq s1         (IList s2) = IList $ s1 : s2
iSeq s1         s2         = IList [s1, s2]

iList :: [AntImperative] -> AntImperative
iList = IList

(>>-) :: AntStrategy -> AntStrategy -> AntStrategy
s1 >>- s2 = do
    s1' <- s1  -- extracting instruction blocks from within the Supply monad
    s2' <- s2
    let s1final = instructions s1' ! final s1'
        s1new   = M.insert (final s1') 
                           (replaceDefaultState (initial s2') s1final) 
                           (instructions s1')
    return $ AntStrategy' (s1new `M.union` (instructions s2')) (initial s1') (final s2')



-- While block, is given a test and a strategy for the body
iWhile :: AntTest -> AntImperative -> AntImperative
iWhile t body = While t body

aWhile :: AntTest -> AntStrategy -> AntStrategy
aWhile (Not (TrySense d c))        = aMkWhile $ \t f -> Sense d f t c
aWhile (TrySense d c)              = aMkWhile $ \t f -> Sense d t f c
aWhile (Not (TryRandomEqZero p))   = aMkWhile $ \t f -> Flip p f t
aWhile (TryRandomEqZero p)         = aMkWhile $ \t f -> Flip p t f
aWhile (Not TryForward)            = aMkWhile $ \t f -> Move f t
aWhile TryForward                  = aMkWhile $ \t f -> Move t f
aWhile (Not TryPickUp)             = aMkWhile $ \t f -> PickUp f t
aWhile TryPickUp                   = aMkWhile $ \t f -> PickUp t f

-- Helper function to aWhile. Produces a conditional loop block, given a conditional
-- assembly instruction and a strategies for the loop body
aMkWhile :: (AntState -> AntState -> AntInstruction) -> AntStrategy -> AntStrategy
aMkWhile condi b = do
    idx   <- supply  -- getting the unique id for the conditional instruction
    gidx  <- supply  -- getting the unique id for the ghost instruction
    b'    <- b  -- extracting the instruction blocks from the Supply monad
    let bidx         = initial b'
        testi        = condi bidx gidx
        ghosti       = Ghost gidx [idx]
        bPlusG       = M.insert gidx ghosti (instructions $ replaceFinal idx b')
    return $ AntStrategy' (M.insert idx testi bPlusG) idx gidx

aWhile' :: AntTest -> AntStrategy -> AntStrategy
aWhile' = aMkWhile' . processAntTest

aMkWhile' :: (AntState -> AntState -> AntStrategy) -> AntStrategy -> AntStrategy
aMkWhile' cond b = do
    gidx  <- supply  -- getting the unique id for the ghost instruction
    b'    <- b  -- extracting the instruction blocks from the Supply monad
    let bidx         = initial b'
    AntStrategy' m i f <- cond bidx gidx
    let condInstrs = M.keys m
        ghosti     = Ghost gidx condInstrs
        bPlusG     = M.insert gidx ghosti (instructions $ replaceFinal i b')
    return $ AntStrategy' (m `M.union` bPlusG) i gidx


-- IfThenElse, given a test and two strategies: one for the true branch and one for the false
iIfThenElse :: AntTest -> AntImperative -> AntImperative -> AntImperative
iIfThenElse c t f = IfThenElse c t f

aIfThenElse :: AntTest -> AntStrategy -> AntStrategy -> AntStrategy
aIfThenElse (Not (TrySense d c))      = aMkIfThenElse $ \t f -> Sense d f t c
aIfThenElse (TrySense d c)            = aMkIfThenElse $ \t f -> Sense d t f c
aIfThenElse (Not (TryRandomEqZero p)) = aMkIfThenElse $ \t f -> Flip p t f
aIfThenElse (TryRandomEqZero p)       = aMkIfThenElse $ \t f -> Flip p f t
aIfThenElse (Not TryForward)          = aMkIfThenElse $ \t f -> Move f t
aIfThenElse TryForward                = aMkIfThenElse $ \t f -> Move t f
aIfThenElse (Not TryPickUp)           = aMkIfThenElse $ \t f -> PickUp f t
aIfThenElse TryPickUp                 = aMkIfThenElse $ \t f -> PickUp t f

-- Helper function to aIfThenElse. Produces a conditional strategy given an assembly instruction
-- and two strategies. Introduces a "Ghost" instruction to serve as return point from both branches
aMkIfThenElse :: (AntState -> AntState -> AntInstruction) -> AntStrategy -> AntStrategy -> AntStrategy
aMkIfThenElse condi ts fs = do
    idx <- supply  -- getting the unique id for the testing instruction
    gidx <- supply -- getting the unique id for the ghost instruction
    ts' <- ts  -- extracting the instruction blocks from the Supply monad
    fs' <- fs
    let (tidx, fidx) = (initial ts', initial fs')
        ghosti       = Ghost gidx [(final ts'),(final fs')]
        testi        = condi tidx fidx
        fPlusT       = (instructions $ replaceFinal gidx ts') `M.union` 
                       (instructions $ replaceFinal gidx fs')
    return $ AntStrategy' (M.insert idx testi (M.insert gidx ghosti fPlusT)) idx gidx


aIfThenElse' :: AntTest -> AntStrategy -> AntStrategy -> AntStrategy
aIfThenElse' = aMkIfThenElse' . processAntTest

aMkIfThenElse' :: (AntState -> AntState -> AntStrategy) -> AntStrategy -> AntStrategy 
               -> AntStrategy
aMkIfThenElse' cond ts fs = do
    gidx <- supply -- getting the unique id for the ghost instruction
    ts' <- ts  -- extracting the instruction blocks from the Supply monad
    fs' <- fs
    let (tidx, fidx) = (initial ts', initial fs')
    AntStrategy' m i f <- cond tidx fidx
    let ghosti       = Ghost gidx [(final ts'),(final fs')]
        fPlusT       = (instructions $ replaceFinal gidx ts') `M.union` 
                       (instructions $ replaceFinal gidx fs')
    return $ AntStrategy' (m `M.union` (M.insert gidx ghosti fPlusT)) i gidx



-- IfThen, given a test and a strategy for the body
iIfThen :: AntTest -> AntImperative -> AntImperative
iIfThen t body = IfThen t body

aIfThen :: AntTest -> AntStrategy -> AntStrategy
aIfThen (Not (TrySense d c))      = aMkIfThen $ \t f -> Sense d f t c
aIfThen (TrySense d c)            = aMkIfThen $ \t f -> Sense d t f c
aIfThen (Not (TryRandomEqZero p)) = aMkIfThen $ \t f -> Flip p t f
aIfThen (TryRandomEqZero p)       = aMkIfThen $ \t f -> Flip p f t
aIfThen (Not TryForward)          = aMkIfThen $ \t f -> Move f t
aIfThen TryForward                = aMkIfThen $ \t f -> Move t f
aIfThen (Not TryPickUp)           = aMkIfThen $ \t f -> PickUp f t
aIfThen TryPickUp                 = aMkIfThen $ \t f -> PickUp t f

-- Helper function to make a IfThen block.
aMkIfThen :: (AntState -> AntState -> AntInstruction) -> AntStrategy -> AntStrategy
aMkIfThen condi body = do
    idx <- supply
    gidx <- supply
    body' <- body
    let testi  = condi (initial body') gidx
        ghosti = Ghost gidx [(final body'),idx]
        instrs = (instructions $ replaceFinal gidx body') -- body instr
    return $ AntStrategy' (M.insert idx testi $ M.insert gidx ghosti instrs) idx gidx


aIfThen' :: AntTest -> AntStrategy -> AntStrategy
aIfThen' = aMkIfThen' . processAntTest

aMkIfThen' :: (AntState -> AntState -> AntStrategy) -> AntStrategy -> AntStrategy
aMkIfThen' cond body = do
    gidx <- supply
    body' <- body
    AntStrategy' m i f <- cond (initial body') gidx
    let condInstrs = M.keys m
        ghosti     = Ghost gidx ((final body') : condInstrs)
        instrs     = (instructions $ replaceFinal gidx body') -- body instr
    return $ AntStrategy' (m `M.union` M.insert gidx ghosti instrs) i gidx


-- Produces a block containing a side-effecting test instruction, but with no body
iTest :: AntTest -> AntImperative
iTest t = SideEffect t

aTest :: AntTest -> AntStrategy
aTest (Not (TrySense d c))      = aMkTest $ \t f -> Sense d f t c
aTest (TrySense d c)            = aMkTest $ \t f -> Sense d t f c
aTest (Not (TryRandomEqZero p)) = aMkTest $ \t f -> Flip p t f
aTest (TryRandomEqZero p)       = aMkTest $ \t f -> Flip p f t
aTest (Not TryForward)          = aMkTest $ \t f -> Move f t
aTest TryForward                = aMkTest $ \t f -> Move t f
aTest (Not TryPickUp)           = aMkTest $ \t f -> PickUp f t
aTest TryPickUp                 = aMkTest $ \t f -> PickUp t f

-- Helper function to build a aTest block
aMkTest :: (AntState -> AntState -> AntInstruction) -> AntStrategy
aMkTest condi = do
    idx <- supply
    gidx <- supply
    let ghost = (gidx, Ghost gidx [idx])
        allInstrs = M.fromList [ghost, (idx, condi gidx gidx)]
    return $ AntStrategy' allInstrs idx gidx


aTest' :: AntTest -> AntStrategy
aTest' = aMkTest' . processAntTest 


aMkTest' :: (AntState -> AntState -> AntStrategy) -> AntStrategy
aMkTest' cond = do
    gidx <- supply
    AntStrategy' m i f <- cond gidx gidx
    let condInstrs = M.keys m
        ghost      = (gidx, Ghost gidx condInstrs)
        allInstrs  = m `M.union` M.fromList [ghost] 
    return $ AntStrategy' allInstrs i gidx


-- | Dealing with boolean operators

aMkAnd :: (AntState -> AntState -> AntStrategy) -- s1
       -> (AntState -> AntState -> AntStrategy) -- s2
       -> AntState                              -- true branch
       -> AntState                              -- else branch
       -> AntStrategy
aMkAnd mkS1 mkS2 st sf = do
    AntStrategy' m2 i2 f2 <- mkS2 st sf
    AntStrategy' m1 i1 f1 <- mkS1 i2 sf
    return $ AntStrategy' (m1 `M.union` m2) i1 f2 


processAntTest :: AntTest -> (AntState -> AntState -> AntStrategy)
processAntTest = foldAntTest (sense,random,forward,pickup,and,not) 
    where aMkSingletonCondStrategy f id1 id2 = do 
              idx <- supply
              return $ AntStrategy' (M.singleton idx (f id1 id2)) idx idx 
          sense d c   = aMkSingletonCondStrategy (\t f -> Sense d t f c)
          random p    = aMkSingletonCondStrategy (\t f -> Flip p f t)
          forward     = aMkSingletonCondStrategy Move
          pickup      = aMkSingletonCondStrategy PickUp
          and         = aMkAnd 
          not s ts fs = s fs ts 


-- | This functions gives the semantics of the AntImperative deep-embedded EDSL,
-- in terms of the assembly-building functions aWhile, aIfThenElse, etc.
semanticsImp :: AntImperative -> AntStrategy
semanticsImp (Single b)         = semanticsBasic b
semanticsImp (IfThenElse c t e) = aIfThenElse c (semanticsImp t) (semanticsImp e)
semanticsImp (IfThen c b)       = aIfThen c (semanticsImp b)
semanticsImp (While c b)        = aWhile c (semanticsImp b)
semanticsImp (SideEffect t)     = aTest t
semanticsImp (IList l)          = semanticsImpList l
    where
        semanticsImpList (x:[]) = semanticsImp x
        semanticsImpList (x:xs) = semanticsImp x >>- semanticsImpList xs


-- TODO: The code below should go to another module

-- Chooses a random strategy, among the ones in the given list, with uniform distribution
chooseUniformly :: [AntImperative] -> AntImperative
chooseUniformly (s:[])   = s
chooseUniformly l@(s:ss) = iIfThenElse (TryRandomEqZero (sz-1)) s (chooseUniformly ss)
    where sz = length l

doWithChance :: Int -> AntImperative -> AntImperative
doWithChance p = iIfThen (Not $ TryRandomEqZero p)

oneOfOrNothing :: [AntImperative] -> AntImperative
oneOfOrNothing ss = doWithChance (length ss) $ chooseUniformly ss



