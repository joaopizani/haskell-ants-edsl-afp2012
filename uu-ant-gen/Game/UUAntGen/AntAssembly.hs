{-# LANGUAGE DeriveGeneric #-}
module Game.UUAntGen.AntAssembly where

import Data.Hashable (Hashable)
import GHC.Generics  (Generic)


newtype AntState = AntState Int
    deriving (Eq, Ord, Generic)  -- have to derive Generic to make auto-instance of Hashable work

instance Hashable AntState where  -- empty instance decl, GHC Generics will derive it

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
    deriving (Eq, Show)


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

