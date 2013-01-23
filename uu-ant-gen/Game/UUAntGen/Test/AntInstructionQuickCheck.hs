{-# LANGUAGE FlexibleInstances #-}
module Game.UUAntGen.Test.AntInstructionQuickCheck where

import Test.QuickCheck.Monadic
import Test.QuickCheck   (Gen(..),Arbitrary(..),arbitrary,Property,oneof,forAll
                         ,listOf,choose,sized,frequency,vectorOf)
import Control.Monad     (liftM,liftM2,liftM3)
import Data.Map          ((!))

import Control.Monad.Supply

import Game.UUAntGen.Test.AntInstructionArbitrary
import Game.UUAntGen.AntMap
import Game.UUAntGen.AntInstruction


data Action 
    = Single Command
    | If AntTest [Action] [Action] 
    | While AntTest [Action] Action
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
      If c t e           -> as >>- aIfThenElse c (perform t) (perform e) 
      While c b a'       -> as >>- aWhile c (perform b) (perform (a':tl)) 

perform :: [Action] -> AntStrategy
perform []     = aDrop -- this should be working on non-empty lists...
perform (a:tl) = 
    case a of
      Single (CMark p)   -> semantics (aMark p) tl 
      Single (CUnMark p) -> semantics (aUnMark p) tl 
      Single CDrop       -> semantics aDrop tl
      Single (CTurn d)   -> semantics (aTurn d) tl
      If c t e           -> aIfThenElse c (perform t) (perform e) 
      While c b a'       -> aWhile c (perform b) (perform (a':tl)) 


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
                                , (1, liftM3 While arbitrary (genActs n) 
                                                             (genAction 1)) ]
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

finalRefsItselfProp :: Property
finalRefsItselfProp = supplyProp finalRefsItselfPropM

finalRefsItselfPropM :: PropertyM (Supply AntState) ()
finalRefsItselfPropM = forAllM actions $ \as -> do
    b <- run (perform as)
    assert (lastRefsItself b)

singleInstructionProp :: Property
singleInstructionProp = supplyProp singleInstructionPropM 

singleInstructionPropM :: PropertyM (Supply AntState) () 
singleInstructionPropM = forAllM singleAction $ \a -> do
    b <- run (perform a) 
    assert (refsItself b)

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


