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
        ghosti       = Ghost gidx idx idx
        bPlusG       = M.insert gidx ghosti (instructions $ replaceFinal idx b')
    return $ AntStrategy' (M.insert idx testi bPlusG) idx gidx



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
        ghosti       = Ghost gidx (final ts') (final fs')
        testi        = condi tidx fidx
        fPlusT       = (instructions $ replaceFinal gidx ts') `M.union` 
                       (instructions $ replaceFinal gidx fs')
    return $ AntStrategy' (M.insert idx testi (M.insert gidx ghosti fPlusT)) idx gidx



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
        ghosti = Ghost gidx (final body') idx
        instrs = (instructions $ replaceFinal gidx body') -- body instr
    return $ AntStrategy' (M.insert idx testi $ M.insert gidx ghosti instrs) idx gidx



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
    let ghost = (gidx, Ghost gidx idx idx)
        allInstrs = M.fromList [ghost, (idx, condi gidx gidx)]
    return $ AntStrategy' allInstrs idx gidx


aTest' :: AntTest -> AntStrategy
aTest' TryPickUp                   = do idx <- supply 
                                        aMkTest' $ \t f -> M.fromList [(idx,PickUp t f)]
aTest' (And ts)                    = do ids <- sequence $ replicate (length ts) supply
                                        aMkTest' $ aAnd undefined ids    
--aTest' (TrySense d c)            = aMkTest $ \t f -> Sense d t f c
--aTest' (Not (TryRandomEqZero p)) = aMkTest $ \t f -> Flip p t f
--aTest' (TryRandomEqZero p)       = aMkTest $ \t f -> Flip p f t
--aTest' (Not TryForward)          = aMkTest $ \t f -> Move f t
--aTest' TryForward                = aMkTest $ \t f -> Move t f
--aTest' (Not TryPickUp)           = aMkTest $ \t f -> PickUp f t


-- Helper function to build a aTest block
aMkTest' :: (AntState -> AntState -> IMap) -> AntStrategy
aMkTest' conds = do
    gidx <- supply
    let condM     = conds gidx gidx
        (idx,_)   = M.findMax condM
        ghost     = Ghost gidx idx idx
        allInstrs = M.insert gidx ghost condM 
    return $ AntStrategy' allInstrs idx gidx


-- | PRE-CONDITION: list with at least 2 elements (otherwise AND doesn't make sense)
aAnd :: [AntState -> AntState -> AntInstruction] -- Conditionals 
     -> [AntState]                               -- Labels for each conditional
     -> AntState                                 -- true label
     -> AntState                                 -- false label
     -> IMap                                     -- list of instructions
aAnd condTrueFalseL ids st sf = do
    let l          = length condL
        condFalseL = map (\(f,x) -> f x) $ 
                     zip condTrueFalseL (tail ids ++ [st]) -- link true
        condL      = map ($sf) condFalseL -- link false
     in M.fromList (zip ids condL) 
    

-- Chooses a random strategy, among the ones in the given list, with uniform distribution
chooseUniformly :: [AntImperative] -> AntImperative
chooseUniformly (s:[])   = s
chooseUniformly l@(s:ss) = iIfThenElse (TryRandomEqZero (sz-1)) s (chooseUniformly ss)
    where sz = length l

doWithChance :: Int -> AntImperative -> AntImperative
doWithChance p = iIfThen (Not $ TryRandomEqZero p)

oneOfOrNothing :: [AntImperative] -> AntImperative
oneOfOrNothing ss = doWithChance (length ss) $ chooseUniformly ss


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

