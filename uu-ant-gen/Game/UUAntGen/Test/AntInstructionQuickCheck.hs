{-# LANGUAGE FlexibleInstances #-}
module Game.UUAntGen.Test.AntInstructionQuickCheck where

import Test.QuickCheck.Monadic
import Test.QuickCheck   (Gen(..),Arbitrary(..),arbitrary,Property,oneof,forAll
                         ,listOf,choose,sized,frequency,vectorOf)
import Control.Monad     (liftM,liftM2,liftM3)
import Data.Map          ((!),elems,size)
import Data.Set          (fromList,isSubsetOf)


import Control.Monad.Supply

import Game.UUAntGen.Test.AntInstructionArbitrary
import Game.UUAntGen.AntMap 
import Game.UUAntGen.AntInstruction

import Debug.Trace

data Action 
    = Single Command
    | If AntTest [Action] [Action] 
    | While AntTest [Action] 
    deriving Show

data Command 
    = CMark Pheromone
    | CUnMark Pheromone
    | CDrop
    | CTurn Dexterity 
    deriving Show


semantics :: AntStrategy -> [Action] -> AntStrategy
semantics as []     = as
semantics as (a:tl) =
    case a of
      Single (CMark p)   -> as >>- semantics (aMark p) tl 
      Single (CUnMark p) -> as >>- semantics (aUnMark p) tl 
      Single CDrop       -> as >>- semantics aDrop tl
      Single (CTurn d)   -> as >>- semantics (aTurn d) tl
      If c t e           -> as >>- semantics (aIfThenElse c (perform t) (perform e)) 
                                             tl 
      While c b          -> as >>- semantics (aWhile c (perform b)) tl  

perform :: [Action] -> AntStrategy
perform []     = aDrop -- this should be working on non-empty lists...
perform (a:tl) = 
    case a of
      Single (CMark p)   -> semantics (aMark p) tl 
      Single (CUnMark p) -> semantics (aUnMark p) tl 
      Single CDrop       -> semantics aDrop tl
      Single (CTurn d)   -> semantics (aTurn d) tl
      If c t e           -> aIfThenElse c (perform t) (perform e) 
      While c b          -> aWhile c (perform b) 


-- Generators

actions :: Gen [Action]
actions = liftM2 (:) arbitrary (listOf arbitrary)

singleAction :: Gen [Action]
singleAction = liftM (\x -> [Single x]) arbitrary

instance Arbitrary Action where
    arbitrary = sized genAction 
        where 
              genAction 0 = liftM Single arbitrary 
              genAction n = frequency $ 
                                [ (5, liftM  Single arbitrary)
                                , (1, liftM3 If arbitrary (genActs n) (genActs n))
                                , (1, liftM2 While arbitrary (genActs n)) ]
              genActs n = choose (1,4) >>= \l -> vectorOf l (genAction (n `div` 2))

instance Arbitrary Command where
    arbitrary = oneof [ liftM CMark arbitrary 
                      , liftM CUnMark arbitrary 
                      , return CDrop
                      , liftM CTurn arbitrary ]
 
instance Arbitrary AntTest where
    arbitrary = oneof [ return TryForward
                      , return TryPickup
                      , liftM2 TrySense arbitrary arbitrary
                      , liftM  TryRandomEqZero (choose (0,maxBound)) ]

-- The actual properties

supplyProp :: PropertyM (Supply AntState) a -> Property
supplyProp p = monadic f p
    where f :: Supply AntState Property -> Property
          f s = fst $ runSupply s [AntState 0..]

-- | Given a predicate on AntStrategy' and a generator for a list of actions,
--   test the predicate AFTER running the computation in the supply monad and 
--   applying the function to it
mkSupplyEndProp :: (AntStrategy' -> Bool) -> Gen [Action] -> Property
mkSupplyEndProp p gen = supplyProp p'
    where p' = forAllM gen $ \as -> do
                   b <- run (perform as) 
                   assert (p b)


finalRefsItselfProp :: Property
finalRefsItselfProp = mkSupplyEndProp lastRefsItself actions 

singleInstructionProp :: Property
singleInstructionProp = mkSupplyEndProp refsItself singleAction 

noBrokenRefsProp :: Property 
noBrokenRefsProp = mkSupplyEndProp noBrokenRefs actions 


-- AntStrategy' predicates

refsItself :: AntStrategy' -> Bool
refsItself as = let ins  = instructions as
                    init = initial as
                    fin  = final as
                 in init == fin &&
                    getDefaultState (ins ! init) == init

lastRefsItself :: AntStrategy' -> Bool
lastRefsItself as = let ins  = instructions as
                        fin  = final as
                     in getDefaultState (ins ! fin) == fin

-- FIXME: last ref may be broken 
noBrokenRefs :: AntStrategy' -> Bool
noBrokenRefs as = 
    let (AntStrategy' m i f) = fromKeysToLineNumbers $ ghostBuster as
        k    = size m 
        setM = fromList $ map toInt $ concatMap getAntStates $ elems m
        set  = fromList $ [0..(k-1)]
     in trace ("Original map"
               ++ show m ++ " ") $ setM `isSubsetOf` set 
    where toInt (AntState s) = s


