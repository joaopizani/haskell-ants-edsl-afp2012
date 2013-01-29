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
    gidx <- supply -- getting the unique id for the ghost instruction
    ts' <- ts  -- extracting the instruction blocks from the Supply monad
    fs' <- fs
    let (tidx, fidx) = (initial ts', initial fs')
    AntStrategy' m i _ <- cond tidx fidx
    let ghosti       = Ghost gidx [(final ts'),(final fs')]
        fPlusT       = (instructions $ replaceFinal gidx ts') `M.union` 
                       (instructions $ replaceFinal gidx fs')
    return $ AntStrategy' (m `M.union` (M.insert gidx ghosti fPlusT)) i gidx



-- IfThen, given a test and a strategy for the body
iIfThen :: AntTest -> AntImperative -> AntImperative
iIfThen t body = IfThen t body

aIfThen :: AntTest -> AntStrategy -> AntStrategy
aIfThen = aMkIfThen . processAntTest

-- Helper function to make a IfThen block.
aMkIfThen :: (AntState -> AntState -> AntStrategy) -> AntStrategy -> AntStrategy
aMkIfThen cond body = do
    gidx <- supply
    body' <- body
    AntStrategy' m i _ <- cond (initial body') gidx
    let condInstrs = M.keys m
        ghosti     = Ghost gidx ((final body') : condInstrs)
        instrs     = (instructions $ replaceFinal gidx body') -- body instr
    return $ AntStrategy' (m `M.union` M.insert gidx ghosti instrs) i gidx


-- Produces a block containing a side-effecting test instruction, but with no body
iTest :: AntTest -> AntImperative
iTest t = SideEffect t

aTest :: AntTest -> AntStrategy
aTest = aMkTest . processAntTest

-- Helper function to build a aTest block
aMkTest :: (AntState -> AntState -> AntStrategy) -> AntStrategy
aMkTest cond = do
    gidx <- supply
    AntStrategy' m i _ <- cond gidx gidx
    let condInstrs = M.keys m
        ghost      = (gidx, Ghost gidx condInstrs)
        allInstrs  = m `M.union` M.fromList [ghost] 
    return $ AntStrategy' allInstrs i gidx


-- Produces a switch-case like block, analogous to nested if-elsif statements
iCase :: [(AntTest,AntImperative)] -> AntImperative
iCase = Case

aCase :: [(AntTest,AntStrategy)] -> AntStrategy
aCase = aMkCase . mapFst processAntTest
    where mapFst f = map (\(x,y) -> (f x,y))

aMkCase :: [(AntState -> AntState -> AntStrategy,AntStrategy)] -> AntStrategy
aMkCase condL = do
    gidx <- supply
    let ss = map (linkGhost gidx . snd) condL
    ss' <- sequence ss
    let (condsF, _) = unzip $ map linkTrue $ zip (map fst condL) ss'
        -- linking conditions
        conds' = foldr (\mkS1 s2 -> do AntStrategy' m2 i2 f2 <- s2 
                                       AntStrategy' m1 i1 f1 <- mkS1 i2
                                       return $ AntStrategy' (m1 `M.union` m2) 
                                                             i1 
                                                             (if f2 == gidx
                                                              then f1
                                                              else f2))
                       (return $ AntStrategy' M.empty gidx gidx) 
                       condsF
    AntStrategy' mc ic f <- conds'
    let mss = foldr M.union M.empty $ map instructions ss'
        ghosti = Ghost gidx (f : M.keys mss) 
        allInstrs = M.insert gidx ghosti (mc `M.union` mss) 
    return $ AntStrategy' allInstrs ic gidx 
   where
        linkGhost :: AntState -> AntStrategy -> AntStrategy
        linkGhost gidx s = s >>= return . replaceFinal gidx

        linkTrue :: (AntState -> AntState -> AntStrategy, AntStrategy') 
                 -> (AntState -> AntStrategy, AntStrategy')
        linkTrue (mkS,s) = (\fs -> flip mkS fs $ initial s, s)



-- | Dealing with boolean operators

-- | Produces a block of conjunction of two conditional instructions given two functions
-- that, given the reference of the true and false branches, produces blocks corresponding
-- to the inner expressions
aMkAnd :: (AntState -> AntState -> AntStrategy)  -- s1
       -> (AntState -> AntState -> AntStrategy)  -- s2
       -> AntState  -- true branch
       -> AntState  -- else branch
       -> AntStrategy
aMkAnd mkS1 mkS2 st sf = do
    AntStrategy' m2 i2 f2 <- mkS2 st sf
    AntStrategy' m1 i1 _  <- mkS1 i2 sf
    return $ AntStrategy' (m1 `M.union` m2) i1 f2


-- | Procuces a block of disjunction of two conditional instructions given two functions
-- that, given the reference of the true and false branch, produce blocks corresponding
-- to the inner expressions
aMkOr :: (AntState -> AntState -> AntStrategy)  -- s1
      -> (AntState -> AntState -> AntStrategy)  -- s2
      -> AntState  -- true branch
      -> AntState  -- else branch
      -> AntStrategy
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

