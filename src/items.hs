module Items
  ( Item(Item)
  , Module(..)
  , moduleFold
  , describe
  , newItem) where

import System.Random
import Control.Monad.State

tests::[Item]
tests = [
    Item [toWalk [Smooth 3, Rough 7]],
    Item [toWalk [Smooth 10, Rough 4]],
    Item [toWalk [Smooth 14, Rough 6]],
    Item [Magnify 2, Rad, toWalk [Smooth 4, Rough 6]],
    Item [Fire, toWalk [Smooth 65, Rough 15]],
    Item [Magnify 245, Rad, Fire, toWalk [Smooth 13, Rough 50]],
    Item [Magnify 46, toExamine [World, Player]],
    Item [Magnify 23, Grasp, Magnify 30, toExamine [History, Player]],
    Item [Magnify 100, toExamine [History, Player]]
  ]

newtype Item = Item [Module]

data Module = ModNull
            | Magnify Int           -- Make things better
            | Walk LWalkProp        -- Move
            | Rad                   -- How awesome is it?
            | Fly                   -- Move
            | Examine LExamineProp
            | Grasp                 -- Pick things up.
            | Carry                 -- Inventory, etc.
            | Explode    
            | Fire
            | Illuminate
            | Upgrade               -- Item can be upgraded
            | Expend Int            -- Item can be expended
            | Break                 -- Indicates a breakpoint
            | System                -- System commands, e.g. "exit"
            deriving Show

-- Folds (ish) a function over the modules. This function should take
-- a tuple and return a list. In the tuple (p, m, ms):
--    p : list of previous modules (most recent is the head)
--    m : current module
--    ms: list of remaining modules (next is the head)
moduleFold::(([Module], Module, [Module]) -> [a])->[Module]->[a]
moduleFold f = moduleFold' f [] where
  moduleFold' f p [] = []
  moduleFold' f p (m:ms) = (f (p, m, ms)) ++ (moduleFold' f (m:p) ms)

class PropertyList t where
  combineProp :: t -> t -> t

-- Walk properties
newtype LWalkProp = LWalkProp {wprop :: [WalkProp]} deriving Show
data WalkProp
  = Smooth Int  -- Can walk over smooth terrain.
  | Rough  Int  -- Can walk over rough terrain.
  deriving Show

instance PropertyList LWalkProp where
  combineProp p1 p2 = LWalkProp [Smooth (walkSmooth p1), Rough (walkRough p2)]

-- For convenience:
toWalk props = Walk (LWalkProp props)

-- Get the smoothness/roughness values.
walkSmooth :: LWalkProp -> Int
walkSmooth prop = foldr (\x s -> case x of
  (Smooth y) -> s + y
  (Rough  _) -> s) 0 (wprop prop)
walkRough  :: LWalkProp -> Int
walkRough  prop = foldr (\x r -> case x of
  (Rough  y) -> r + y
  (Smooth _) -> r) 0 (wprop prop)

-- Examine properties
data ExamineProp
  = Material
  | World
  | Player
  | History
  | Whatever
  deriving (Show, Eq)

newtype LExamineProp = LExamineProp {eprop :: [ExamineProp]} deriving Show
instance PropertyList LExamineProp where
  -- Ensure each element in the list is unique
  combineProp p1 p2 = LExamineProp $
    foldr f [] ((eprop p1) ++ (eprop p2)) where
      f prop ls = if (hasExamine prop ls) then ls else prop:ls

-- For convenience:
toExamine props = Examine (LExamineProp props)
hasExamine prop = foldr (\p v -> v || (p == prop)) False

--------------------------------------------------------------------------------
-- Description generation:
--------------------------------------------------------------------------------
-- Generate a description of an item.
-- Invisible items have empty descriptions.
describe::Item->Maybe String
describe (Item mods) = case nouns of
  [] -> Nothing
  _  -> Just $ (format adjectives) ++ (format' nouns)
  where adjectives = moduleFold adjectify mods
        nouns      = moduleFold nounify   mods
        format = foldr (\word words -> case words of
          [] -> word ++ " "
          _  -> word ++ ", " ++ words) []
        format' = foldr (\word words -> case word of
          [] -> []
          w  -> case words of [] -> w; _ -> w ++ " " ++ words) []

-- Produce adjectives to describe an item.
adjectify::([Module], Module, [Module])->[String]

-- Rad things:
adjectify (Magnify n : _, Rad, _)
  | n > 100   = ["extremely rad"]
  | n > 80    = ["sick"]
  | n > 50    = ["fashionable"]
  | n < 5     = ["clean"]
  | otherwise = []
adjectify (_, Rad, _) = ["embarassing"]

-- Fire things:
adjectify (_, Fire, (Walk prop : _))
  = ["combustible"]

-- Walking things:
adjectify (_, (Walk prop), _)
  | smooth > 10 && rough > 10 = ["well-cobbled"]
  | smooth > 6  && rough > 4  = ["quick"]
  | smooth > 4  && rough > 3  = ["shoddy"]
  | otherwise                 = []
  where smooth = walkSmooth prop
        rough  = walkRough  prop

adjectify _ = []

-- Produce nouns to name an item.
nounify::([Module], Module, [Module])->[String]

-- Walking items:
nounify (_, (Walk prop), _)
  | smooth > 5 && rough > 5 = ["utility boots"]
  | otherwise               = ["shoes"]
  where smooth = walkSmooth prop
        rough  = walkRough  prop

-- Examining items:
nounify (_, Examine props, _)
  | world && material = ["ultra-sensor", ""]
  | player && history = ["magic eye", ""]
  | world = ["camera", ""]
  | material = ["scanner", ""]
  | player = ["memory chip", ""]
  | history = ["void combobulator", ""]
  where world = hasExamine World prop
        material = hasExamine Material prop
        player = hasExamine Player prop
        history = hasExamine History prop
        prop = eprop props

nounify (Magnify n : _, Grasp, _)
  | n > 20 = ["vice grip", ""]
  | n > 15 = ["claw", ""]
  | n > 10 = ["hand", ""]
  | otherwise = ["grabber", ""]
nounify (_, Grasp, _) = ["grabber", ""]

nounify (_, Magnify n, _)
  | n > 100 = ["carbon-fiber"]
  | n > 50  = ["titanium"]
  | otherwise = []

nounify _ = []


--------------------------------------------------------------------------------
-- Item generation:
--------------------------------------------------------------------------------

randomSt::(Random a) => State StdGen a
randomSt = state random

newItem::State StdGen Item
newItem = do
  numMods <- randomSt
  return $ Item [Magnify numMods, Rad]

-- "Normalize" a list of modules (e.g. condense magnitudes).
normalize::[Module]->[Module]
normalize = normalizeMagnify

normalizeMagnify = foldr (\m ms -> case m of
  Magnify x -> case ms of
    [] -> [Magnify x]
    (Magnify y : ns) -> Magnify (x + y) : ns
  _ -> ms) []

lgen::Int->[Module]->[Module]
lgen 0 m = m
lgen n m = lgen (n-1) $ moduleFold lgen_rules m

lgen_rules::([Module], Module, [Module])->[Module]
lgen_rules (p, m, ms) = case (p, m, ms) of
  (_, Expend b, Expend a : _) -> [Expend (a + b)]
  (Expend a : _, Expend _, _) -> []
  (Expend 0 : _, _, _)      -> [Expend 0]

  (_, Walk a, (Walk b : _)) -> [Walk (combineProp a b)]
  (Walk a : _ , Walk _, _)  -> []

  -- Eliminate consecutive Grasps
  (_, Grasp, Grasp : _) -> [Magnify 2, Grasp]
  (Grasp : _, Grasp, _) -> []

  (Magnify a : _, Grasp, Magnify b : Grasp : _) -> [Magnify (a + b), Grasp]
  (Grasp : _, Magnify a, Grasp : _) -> []
  (Magnify a : Grasp : _, Grasp, _) -> []

  (_, ModNull, _) -> []
  _ -> [m]
