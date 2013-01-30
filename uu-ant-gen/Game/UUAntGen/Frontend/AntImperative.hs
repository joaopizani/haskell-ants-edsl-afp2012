module Game.UUAntGen.Frontend.AntImperative where

import           Control.Monad.Supply (supply)
import           Data.HashMap.Strict  ((!))
import qualified Data.HashMap.Strict  as M

import Game.UUAntGen.Backend.AntAssembly
import Game.UUAntGen.Backend.AntDeepEmbedded
import Game.UUAntGen.Backend.AntInstruction
import Game.UUAntGen.Backend.AntTransformation



-- Iterative programming-like constructs for building ant strategies. Sequencing, loop,
-- conditionals, etc. Every constructs has two corresponding functions. One (prefixed with "a")
-- which builds the ant assembly itself (of type AntStrategy), and one (prefixed with "i")
-- which builds the deep-embedded EDSL (of type AntImperative).

-- Sequencing AntStrategies. Means that s2 will be executed after s1
iSeq :: AntImperative -> AntImperative -> AntImperative
iSeq (IList s1) (IList s2) = IList $ s1 ++ s2   -- first 3 patterns eliminate empty strategies
iSeq (IList s1) s2         = IList $ s1 ++ [s2]
iSeq s1         (IList s2) = IList $ s1 : s2
iSeq s1         s2         = IList [s1, s2]

(>>-) :: AntStrategy -> AntStrategy -> AntStrategy
s1 >>- s2 = do
    s1' <- s1  -- extracting instruction blocks from within the Supply monad
    s2' <- s2
    let s1final = instructions s1' ! final s1'
        s1new   = M.insert (final s1') 
                           (replaceDefaultState (initial s2') s1final) 
                           (instructions s1')
    return $ AntStrategy' (s1new `M.union` (instructions s2')) (initial s1') (final s2')

-- Strategy made of a sequenced list of strategies
iList :: [AntImperative] -> AntImperative
iList = IList



-- While block, is given a test and a strategy for the body
iWhile :: AntTest -> AntImperative -> AntImperative
iWhile t body = While t body

aWhile :: AntTest -> AntStrategy -> AntStrategy
aWhile = aMkWhile . processAntTest

-- Helper function to aWhile. Produces a conditional loop block, given a conditional
-- assembly instruction and a strategies for the loop body
aMkWhile :: (AntState -> AntState -> AntStrategy) -> AntStrategy -> AntStrategy
aMkWhile cond b = do
    gidx  <- supply  -- getting the unique id for the ghost instruction
    b'    <- b  -- extracting the instruction blocks from the Supply monad
    let bidx         = initial b'
    AntStrategy' m i _ <- cond bidx gidx
    let condInstrs = M.keys m
        ghosti     = Ghost gidx condInstrs
        bPlusG     = M.insert gidx ghosti (instructions $ replaceFinal i b')
    return $ AntStrategy' (m `M.union` bPlusG) i gidx



-- IfThenElse, given a test and two strategies: one for the true branch and one for the false
iIfThenElse :: AntTest -> AntImperative -> AntImperative -> AntImperative
iIfThenElse c t f = IfThenElse c t f

aIfThenElse :: AntTest -> AntStrategy -> AntStrategy -> AntStrategy
aIfThenElse = aMkIfThenElse . processAntTest

-- Helper function to aIfThenElse. Produces a conditional strategy given an assembly instruction
-- and two strategies. Introduces a "Ghost" instruction to serve as return point from both branches
aMkIfThenElse :: (AntState -> AntState -> AntStrategy) -> AntStrategy -> AntStrategy -> AntStrategy
aMkIfThenElse cond ts fs = do
    g <- supply -- getting the unique id for the ghost instruction
    ts' <- ts  -- extracting the instruction blocks from the Supply monad
    fs' <- fs
    let (tidx, fidx) = (initial ts', initial fs')
    AntStrategy' m i _ <- cond tidx fidx
    let ghosti = Ghost g [(final ts'),(final fs')]
        fPlusT = (instructions $ replaceFinal g ts') `M.union` (instructions $ replaceFinal g fs')
    return $ AntStrategy' (m `M.union` (M.insert g ghosti fPlusT)) i g



-- IfThen, given a test and a strategy for the body
iIfThen :: AntTest -> AntImperative -> AntImperative
iIfThen t body = IfThen t body

aIfThen :: AntTest -> AntStrategy -> AntStrategy
aIfThen = aMkIfThen . processAntTest

-- Helper function to make a IfThen block.
aMkIfThen :: (AntState -> AntState -> AntStrategy) -> AntStrategy -> AntStrategy
aMkIfThen cond b = do
    g  <- supply
    b' <- b
    AntStrategy' m i _ <- cond (initial b') g
    let condInstrs = M.keys m
        ghosti     = Ghost g ((final b') : condInstrs)
        instrs     = (instructions $ replaceFinal g b')
    return $ AntStrategy' (m `M.union` M.insert g ghosti instrs) i g



-- Produces a block containing a side-effecting test instruction, but with no body
iTest :: AntTest -> AntImperative
iTest t = SideEffect t

aTest :: AntTest -> AntStrategy
aTest = aMkTest . processAntTest

-- Helper function to build a aTest block, given the condition for which to test
aMkTest :: (AntState -> AntState -> AntStrategy) -> AntStrategy
aMkTest cond = do
    g <- supply
    AntStrategy' m i _ <- cond g g
    let condInstrs = M.keys m
        allInstrs  = m `M.union` M.fromList [(g, Ghost g condInstrs)]
    return $ AntStrategy' allInstrs i g



-- Produces a switch-case like block, analogous to nested if-elsif statements
iCase :: [(AntTest,AntImperative)] -> AntImperative
iCase = Case

aCase :: [(AntTest,AntStrategy)] -> AntStrategy
aCase = aMkCase . mapFst processAntTest
    where mapFst f = map (\(x,y) -> (f x,y))

aMkCase :: [(AntState -> AntState -> AntStrategy,AntStrategy)] -> AntStrategy
aMkCase conds = do
    g <- supply
    ss' <- mapM (linkToGhost g . snd) conds
    let (condsF, _) = unzip $ map linkTrue $ zip (map fst conds) ss'
        -- linking conditions
        conds' = foldr (buildCondStrat g) (return $ AntStrategy' M.empty g g) condsF

    AntStrategy' mc ic f <- conds'
    let mss       = foldr M.union M.empty $ map instructions ss'
        ghosti    = Ghost g (f : M.keys mss)
        allInstrs = M.insert g ghosti (mc `M.union` mss)
    return $ AntStrategy' allInstrs ic g
   where
        buildCondStrat g mkS1 s2 = do
            AntStrategy' m2 i2 f2 <- s2
            AntStrategy' m1 i1 f1 <- mkS1 i2
            return $ AntStrategy' (m1 `M.union` m2) i1 (if f2 == g then f1 else f2)
        linkToGhost g s = s >>= return . replaceFinal g
        linkTrue (mkS,s) = (\fs -> flip mkS fs $ initial s, s)



-- | Dealing with boolean operators

-- | Produces a block of conjunction of two conditional instructions given two functions
-- that, given the reference of the true and false branches, produces blocks corresponding
-- to the inner expressions
aMkAnd :: (AntState -> AntState -> AntStrategy) -> (AntState -> AntState -> AntStrategy)
       -> AntState -> AntState -> AntStrategy
aMkAnd mkS1 mkS2 st sf = do
    AntStrategy' m2 i2 f2 <- mkS2 st sf
    AntStrategy' m1 i1 _  <- mkS1 i2 sf
    return $ AntStrategy' (m1 `M.union` m2) i1 f2


-- | Procuces a block of disjunction of two conditional instructions given two functions
-- that, given the reference of the true and false branch, produce blocks corresponding
-- to the inner expressions
aMkOr :: (AntState -> AntState -> AntStrategy) -> (AntState -> AntState -> AntStrategy)
      -> AntState -> AntState -> AntStrategy
aMkOr mkS1 mkS2 st sf = do
    AntStrategy' m2 i2 f2 <- mkS2 st sf
    AntStrategy' m1 i1 _  <- mkS1 st i2
    return $ AntStrategy' (m1 `M.union` m2) i1 f2


-- | Consumes an AntTest and returns a function that produces a block of conditional
-- code, given two parameters: the locations of the true and else branch, respectively.
processAntTest :: AntTest -> (AntState -> AntState -> AntStrategy)
processAntTest = foldAntTest (sense', random', forward', pickup', and', or', not')
    where
        aMkSingletonCondStrategy f id1 id2 = do
            idx <- supply
            return $ AntStrategy' (M.singleton idx (f id1 id2)) idx idx
        sense' d c = aMkSingletonCondStrategy (\t f -> Sense d t f c)
        random' p  = aMkSingletonCondStrategy (\t f -> Flip p t f)
        forward'   = aMkSingletonCondStrategy Move
        pickup'    = aMkSingletonCondStrategy PickUp
        and'       = aMkAnd
        or'        = aMkOr
        not'       = flip



-- | A logical condition (AntTest) which never fails, i.e, a tautology. Pearl of ant nature
tautology :: AntTest
tautology = TrySense Here Friend

-- | The empty strategy is a side-effecting test with no side effects and which never fails
iEmpty :: AntImperative
iEmpty = iTest tautology


-- | An infinite loop, executes a certain strategy forever. Implemented basically as while(true)
iForever :: AntImperative -> AntImperative
iForever = iWhile tautology




-- | This function gives the semantics of the AntImperative deep-embedded EDSL,
-- in terms of the assembly-building functions aWhile, aIfThenElse, etc.
semanticsImp :: AntImperative -> AntStrategy
semanticsImp (Single b)         = semanticsBasic b
semanticsImp (IfThenElse c t e) = aIfThenElse c (semanticsImp t) (semanticsImp e)
semanticsImp (IfThen c b)       = aIfThen c (semanticsImp b)
semanticsImp (While c b)        = aWhile c (semanticsImp b)
semanticsImp (SideEffect t)     = aTest t
semanticsImp (Case l)           = aCase (mapSnd semanticsImp l)
    where mapSnd f = map (\(x,y) -> (x,f y))
semanticsImp (IList l)          = semanticsImpList l
    where
        semanticsImpList (x:[]) = semanticsImp x
        semanticsImpList (x:xs) = semanticsImp x >>- semanticsImpList xs

