module Game.UUAntGen.Test.AntInstructionArbitrary where

import Control.Monad   (liftM, liftM2, liftM3, liftM4)
import Test.QuickCheck (Arbitrary(..), arbitrary, choose, oneof)

import Game.UUAntGen.AntAssembly


instance Arbitrary Pheromone where
    arbitrary = oneof $ map return [P0, P1, P2, P3, P4, P5]

instance Arbitrary Direction where
    arbitrary = oneof $ map return [Here, Ahead, LeftAhead, RightAhead]

instance Arbitrary Dexterity where
    arbitrary = oneof $ map return [L, R]

instance Arbitrary Condition where
    arbitrary =
        oneof $
            liftM Marker arbitrary :
            map return [Friend, Foe, FriendWithFood, FoeWithFood, Food, Rock, FoeMarker, Home, FoeHome]

instance Arbitrary AntState where
    arbitrary = liftM AntState $ choose (0,9999)

instance Arbitrary AntInstruction where
    arbitrary =
        oneof $
            [ liftM4 Sense arbitrary arbitrary arbitrary arbitrary
            , liftM2 Mark arbitrary arbitrary
            , liftM2 UnMark arbitrary arbitrary
            , liftM2 PickUp arbitrary arbitrary
            , liftM  Drop arbitrary
            , liftM2 Turn arbitrary arbitrary
            , liftM2 Move arbitrary arbitrary
            , liftM3 Flip (choose (1,100)) arbitrary arbitrary]

