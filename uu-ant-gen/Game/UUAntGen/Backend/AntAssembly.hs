{-# LANGUAGE DeriveGeneric #-}
module Game.UUAntGen.Backend.AntAssembly where

import Data.Hashable (Hashable)
import GHC.Generics  (Generic)


newtype AntState = AntState Int
    deriving (Eq, Ord, Generic)  -- have to derive Generic to make auto-instance of Hashable work

-- | We might want to use a hash table. Empty instance decl, GHC Generics will derive it
instance Hashable AntState where

instance Show AntState where
    show (AntState i) = show i

instance Enum AntState where
    fromEnum (AntState i) = i
    toEnum i              = (AntState i)


data Pheromone = P0 | P1 | P2 | P3 | P4 | P5
    deriving (Eq, Enum)

instance Show Pheromone where
    show = show . fromEnum


data Direction = Here | Ahead | LeftAhead | RightAhead
    deriving (Eq, Enum, Show)


data Dexterity = L | R
    deriving (Eq, Enum)

instance Show Dexterity where
    show L = "Left"
    show R = "Right"


data Condition
    = Friend | Foe | FriendWithFood | FoeWithFood
    | Food | Rock | Marker Pheromone | FoeMarker | Home | FoeHome
    deriving (Eq)

-- | We avoid the extra parentheses that GHC's derived instance would add
instance Show Condition where
    show (Marker p) = "Marker " ++ (show p)
    show Friend = "Friend"
    show Foe = "Foe"
    show FriendWithFood = "FriendWithFood"
    show FoeWithFood = "FoeWithFood"
    show Food = "Food"
    show Rock = "Rock"
    show FoeMarker = "FoeMarker"
    show Home = "Home"
    show FoeHome = "FoeHome"



-- | Elements of this datatype are Ant assembly instructions.
data AntInstruction
    = Sense Direction AntState AntState Condition
    | Mark Pheromone AntState
    | UnMark Pheromone AntState
    | PickUp AntState AntState
    | Drop AntState
    | Turn Dexterity AntState
    | Move AntState AntState
    | Flip Int AntState AntState
    | Ghost AntState [AntState] 
    deriving (Eq, Show)

