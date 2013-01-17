
module AntGen where

import Test.QuickCheck
import Control.Monad

main = undefined --print randomAntAssembly

newtype AntState = AntState Int

incStateBy :: Int -> AntState -> AntState
incStateBy i (AntState a) = (AntState (a+i))

instance Show AntState where
    show (AntState s) = show s

data AntInstruction
    = Sense Direction AntState AntState Condition
    | Mark Pheromone AntState
    | UnMark Pheromone AntState
    | PickUp AntState AntState
    | Drop AntState
    | Turn Dexterity AntState
    | Move AntState AntState
    | Flip Int AntState AntState
    deriving Show

data Pheromone = P0 | P1 | P2 | P3 | P4 | P5
    deriving Enum

data Direction = Here | Ahead | LeftAhead | RightAhead 
    deriving (Enum, Show)

data Dexterity = L | R
    deriving Enum

data Condition = Friend | Foe | FriendWithFood | FoeWithFood 
               | Food | Rock | Marker Pheromone | FoeMarker | Home | FoeHome

instance Show Condition where
    show Friend         = "Friend"
    show Foe            = "Foe"
    show FriendWithFood = "FriendWithFood"
    show FoeWithFood    = "FoeWithFood"
    show Food           = "Food"
    show Rock           = "Rock"
    show (Marker p)     = "Marker " ++ show p
    show FoeMarker      = "FoeMarker"
    show Home           = "Home"
    show FoeHome        = "FoeHome"
    
instance Show Pheromone where
    show = show . fromEnum

instance Show Dexterity where
    show L = "Left"
    show R = "Right"

instance Arbitrary Pheromone where
    arbitrary = oneof $ map return [ P0 , P1 , P2 , P3 , P4 , P5 ]

instance Arbitrary Direction where
    arbitrary = oneof $ map return [ Here , Ahead , LeftAhead , RightAhead ]

instance Arbitrary Dexterity where
    arbitrary = oneof $ map return [ L , R ]

instance Arbitrary Condition where
    arbitrary = oneof $ liftM Marker arbitrary : 
                        map return [ Friend , Foe , FriendWithFood , FoeWithFood
                                   , Food , Rock , FoeMarker , Home , FoeHome ]

instance Arbitrary AntState where
    arbitrary = liftM AntState $ choose (0,9999)

instance Arbitrary AntInstruction where
    arbitrary = oneof [ liftM4 Sense arbitrary arbitrary arbitrary arbitrary 
                      , liftM2 Mark arbitrary arbitrary
                      , liftM2 UnMark arbitrary arbitrary
                      , liftM2 PickUp arbitrary arbitrary
                      , liftM  Drop arbitrary
                      , liftM2 Turn arbitrary arbitrary
                      , liftM2 Move arbitrary arbitrary
                      , liftM3 Flip (choose (1,100)) arbitrary arbitrary ]
    
