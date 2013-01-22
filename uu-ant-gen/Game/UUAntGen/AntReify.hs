module Game.UUAntGen.AntReify where

import GHC.Vacuum
import Data.IntMap as M
import GHC.Vacuum.GraphViz

main = vacuumToPng "foo" exWhile >> return ()

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


(>>-) :: AntInstruction -> AntInstruction -> AntInstruction
Hole              >>- a  = a
(Sense d a1 a2 c) >>- a3 = Sense d a1 (a2 >>- a3) c
(Mark p a1)       >>- a2 = Mark p (a1 >>- a2)
(UnMark p a1)     >>- a2 = UnMark p (a1 >>- a2)
(PickUp a1 a2)    >>- a3 = PickUp a1 (a2 >>- a3)
(Drop a1)         >>- a2 = Drop (a1 >>- a2)
(Turn d a1)       >>- a2 = Turn d (a1 >>- a2)
(Move a1 a2)      >>- a3 = Move a1 (a2 >>- a1)

-- | Creates a while-style loop
-- Condition -> Body -> Continuation
aWhile :: AntTest -> AntInstruction -> AntInstruction -> AntInstruction
aWhile TryForward          = aMkWhile Move 
aWhile TryPickup           = aMkWhile PickUp 
aWhile (TrySense d c)      = aMkWhile (\t f -> Sense d t f c)
aWhile (TryRandomEqZero i) = aMkWhile (flip (Flip i))

aMkWhile :: (AntInstruction -> AntInstruction -> AntInstruction)
         -> AntInstruction -> AntInstruction -> AntInstruction 
aMkWhile g t f = let t'    = t >>- while
                     while = g t' f 
                  in while 

while :: AntInstruction -> AntInstruction -> AntInstruction
while body rest = let while = Move (body >>- while) rest
                   in while

aDrop = Drop Hole
aTurnL = Turn L Hole

exWhile = aWhile TryForward Hole aTurnL

data AntTest
    = TryForward
    | TryPickup
    | TrySense Direction Condition
    | TryRandomEqZero Int

data AntInstruction
    = Sense Direction AntInstruction AntInstruction Condition
    | Mark Pheromone AntInstruction
    | UnMark Pheromone AntInstruction
    | PickUp AntInstruction AntInstruction
    | Drop AntInstruction
    | Turn Dexterity AntInstruction
    | Move AntInstruction AntInstruction
    | Flip Int AntInstruction AntInstruction
    | Hole
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


