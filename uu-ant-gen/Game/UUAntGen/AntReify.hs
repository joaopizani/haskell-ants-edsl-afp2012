module Game.UUAntGen.AntReify where

import GHC.Vacuum
import Data.IntMap as M


program :: AntInstruction
program = Drop (Flip 25 program program)

runAnt :: a -> IntMap ([HNodeId], String)
runAnt = M.map inspect . vacuum

inspect :: HNode -> ([HNodeId],String)
inspect hnode = (nodePtrs hnode, f (nodeInfo hnode))
    where f :: InfoTab -> String 
          f (ConInfo pkg mod con ptrs 
                     lits ty slen code) | con == "I#" = show $ head $ nodeLits hnode
                                        | otherwise   = con
          f _                                         = "fun"
 

data AntInstruction
    = Sense Direction AntInstruction Condition
    | Mark Pheromone AntInstruction
    | UnMark Pheromone AntInstruction
    | PickUp AntInstruction AntInstruction
    | Drop AntInstruction
    | Turn Dexterity AntInstruction
    | Move AntInstruction AntInstruction
    | Flip Int AntInstruction AntInstruction
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


