module Game.UUAntGen.Frontend.AntImperative where

import           Control.Monad.Supply 
import           Control.Monad        
import           Control.Monad.Trans  
import           Control.Monad.Trans.State  
import           Control.Monad.Reader 
import           Data.Map              ((!))
import qualified Data.Map              as M

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
    (i2,f2,m2) <- s2 
    (i1,_ ,m1) <- local (const i2) s1
    return $ (i1, f2, m1 `M.union` m2) 


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
aMkWhile :: (AntState -> AntState -> AntStrategy) 
         -> AntStrategy 
         -> AntStrategy
aMkWhile cond b = do
    nextState <- ask
    (ib,fb,mb) <- b -- b is being linked to the next state 
    (ic,fc,mc) <- cond ib nextState 
    -- correcly linking b (TODO: could it be improved using fc?)
    let mb' = M.map (replaceMatchingStates nextState ic) mb 
    return $ (ic,fc,mc `M.union` mb') 

-- IfThenElse, given a test and two strategies: one for the true branch and one for the false
iIfThenElse :: AntTest -> AntImperative -> AntImperative -> AntImperative
iIfThenElse c t f = IfThenElse c t f

aIfThenElse :: AntTest -> AntStrategy -> AntStrategy -> AntStrategy
aIfThenElse = aMkIfThenElse . processAntTest

-- Helper function to aIfThenElse. Produces a conditional strategy given an assembly instruction
-- and two strategies. Introduces a "Ghost" instruction to serve as return point from both branches
aMkIfThenElse :: (AntState -> AntState -> AntStrategy) -> AntStrategy -> AntStrategy 
              -> AntStrategy
aMkIfThenElse cond ts fs = do
    (iT,fT,mT) <- ts  
    (iF,fF,mF) <- fs
    (ic,fc,mc) <- cond iT iF
    return $ (ic, fT ++ fF, mc `M.union` mT `M.union` mF)  


-- IfThen, given a test and a strategy for the body
iIfThen :: AntTest -> AntImperative -> AntImperative
iIfThen t body = IfThen t body

aIfThen :: AntTest -> AntStrategy -> AntStrategy
aIfThen = aMkIfThen . processAntTest

-- Helper function to make a IfThen block.
aMkIfThen :: (AntState -> AntState -> AntStrategy) -> AntStrategy -> AntStrategy
aMkIfThen cond b = do
    nextState <- ask 
    (ib,fb,mb) <- b
    (ic,fc,mc) <- cond ib nextState
    return $ (ic, fc ++ fb , (mc `M.union` mb))


-- Produces a block containing a side-effecting test instruction, but with no body
iTest :: AntTest -> AntImperative
iTest t = SideEffect t

aTest :: AntTest -> AntStrategy
aTest = aMkTest . processAntTest

-- Helper function to build a aTest block, given the condition for which to test
aMkTest :: (AntState -> AntState -> AntStrategy) -> AntStrategy
aMkTest cond = ask >>= \nextState -> cond nextState nextState


-- Produces a switch-case like block, analogous to nested if-elsif statements
iCase :: [(AntTest,AntImperative)] -> AntImperative
iCase = Case

aCase :: [(AntTest,AntStrategy)] -> AntStrategy
aCase = aMkCase . mapFst processAntTest
    where mapFst f = map (\(x,y) -> (f x,y))


aMkCase :: [(AntState -> AntState -> AntStrategy,AntStrategy)] -> AntStrategy
aMkCase conds = do
    nextState <- ask
    ss <- sequence $ map snd conds
    let condsF = map linkTrue $ zip (map fst conds) ss
        -- linking conditions
        conds' = foldr (buildCondStrat nextState) 
                       (return (nextState,[nextState],M.empty)) 
                       condsF
    (ic,fc,mc) <- conds'
    let mss = foldr M.union M.empty (map third ss)
        fss = concat (map second ss)
    return $ (ic, fss, mss `M.union` mc)
   where
        first  (x,_,_) = x
        second (_,x,_) = x
        third  (_,_,x) = x
        buildCondStrat g mkS1 s2 = do
            (i2,f2,m2) <- s2
            (i1,f1,m1) <- mkS1 i2
            return $ (i1, if f2 == [g] then f1 else f2, (m1 `M.union` m2)) 
        linkTrue (mkS,s) = \fs -> flip mkS fs $ first s 


-- | Dealing with boolean operators

-- | Produces a block of conjunction of two conditional instructions given two functions
-- that, given the reference of the true and false branches, produces blocks corresponding
-- to the inner expressions
aMkAnd :: (AntState -> AntState -> AntStrategy) -> (AntState -> AntState -> AntStrategy)
       -> AntState -> AntState -> AntStrategy
aMkAnd mkS1 mkS2 st sf = do
    (i2,f2,m2) <- mkS2 st sf
    (i1,_ ,m1) <- mkS1 i2 sf
    return $ (i1, f2, (m1 `M.union` m2)) 


-- | Procuces a block of disjunction of two conditional instructions given two functions
-- that, given the reference of the true and false branch, produce blocks corresponding
-- to the inner expressions
aMkOr :: (AntState -> AntState -> AntStrategy) -> (AntState -> AntState -> AntStrategy)
      -> AntState -> AntState -> AntStrategy
aMkOr mkS1 mkS2 st sf = do
    (i2,f2,m2) <- mkS2 st sf
    (i1,_ ,m1) <- mkS1 st i2
    return $ (i1,f2,(m1 `M.union` m2))


-- | Consumes an AntTest and returns a function that produces a block of conditional
-- code, given two parameters: the locations of the true and else branch, respectively.
processAntTest :: AntTest -> (AntState -> AntState -> AntStrategy)
processAntTest = foldAntTest (sense', random', forward', pickup', and', or', not')
    where
        aMkSingletonCondStrategy f id1 id2 = do
            idx <- lift $ supply
            return $ (idx, [idx], M.singleton idx (f id1 id2)) 
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

-- | Delays execution n rounds by doing nothing repeatedly
iDelay :: Int -> AntImperative
iDelay n = iList $ replicate n iEmpty

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

