module Items
  ( Item
  , Module(..)
  , LExamineProp(..)
  , LWalkProp(..)
  , moduleFold
  , combine
  , describe
  , descriptions
  , newItem) where

import System.Random
import Control.Monad.State
import Control.Monad (replicateM, mapM)
import Data.Set (Set, insert, empty, toList)
import Data.List as List
import Utils

type Item = [Module]

data Module = Magnify Int           -- Make things better
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
            deriving (Show)

num_modules = 13 :: Int

-- Folds (ish) a function over the modules. This function should take
-- a tuple and return a list. In the tuple (p, m, ms):
--    p : list of previous modules (most recent is the head)
--    m : current module
--    ms: list of remaining modules (next is the head)
moduleFold::(([Module], Module, [Module]) -> [a])->[Module]->[a]
moduleFold f = moduleFold' f [] where
  moduleFold' f p [] = []
  moduleFold' f p (m:ms) = (f (p, m, ms)) ++ (moduleFold' f (m:p) ms)

moduleFoldS::(Ord a)=>(([Module], Module, [Module]) -> [a])->[Module]->Set a
moduleFoldS f = moduleFold' f [] where
  moduleFold' f p [] = empty
  moduleFold' f p (m:ms) = foldr
    (\x s -> Data.Set.insert x s)
    (moduleFold' f (m:p) ms)
    (f (p, m, ms))

class PropertyList t where
  combineProp :: t -> t -> t

-- Walk properties -------------------------------------------------------------
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

-- Examine properties ----------------------------------------------------------
data ExamineProp
  = Material
  | World
  | Player
  | History
  | Whatever
  deriving (Show, Eq, Enum)

num_examineprops = 5

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

descriptions::[Item]->[String]
descriptions items = mlist (map describe items)

-- Generate a description of an item.
-- Invisible items have empty descriptions.
describe::Item->Maybe String
describe mods = case nouns of
  [] -> Nothing
  _  -> Just $ (format adjectives) ++ (format' nouns)
  where adjectives = take 5 $ toList $ moduleFoldS adjectify mods
        nouns      = moduleFold nounify mods
        format = foldr (\word words -> case words of
          [] -> word ++ " "
          _  -> word ++ ", " ++ words) []
        format' = foldr (\word words -> case word of
          [] -> []
          w  -> case words of [] -> w; _ -> w ++ " " ++ words) []

--------------------------------------------------------------------------------
-- ADJECTIFY
-- Produce adjectives to describe an item.
--------------------------------------------------------------------------------
adjectify::([Module], Module, [Module])->[String]

-- Magnify

-- Walking
adjectify (_, (Walk prop), _)
  | smooth > 10 && rough > 10 = ["well-cobbled"]
  | smooth > 6  && rough > 4  = ["quick"]
  | smooth > 4  && rough > 3  = ["shoddy"]
  | otherwise                 = []
  where smooth = walkSmooth prop
        rough  = walkRough  prop

-- Rad
adjectify (Magnify n : _, Rad, _)
  | n > 100   = ["extremely rad"]
  | n > 80    = ["sick"]
  | n > 50    = ["fashionable"]
  | n > 5     = ["clean"]
  | otherwise = ["embarrassing"]
adjectify (_, Rad, _) = ["embarrassing"]

-- Fly
adjectify (_, Fly, _) = ["lighter-than-air"]

-- Examine

-- Grasp

-- Carry

-- Explode

-- Fire
adjectify (_, Fire, (Walk prop : _)) = ["combustible"]

-- Illuminate
adjectify (Magnify n : _, Illuminate, _)
  | n > 10    = ["luminous"]
  | otherwise = []

-- Upgrade
adjectify (_, Upgrade, _) = ["unfinished"]

-- Expend
adjectify (_, Expend n, _)
  | n < 3     = ["feeble"]
  | n < 10    = ["disposable"]
  | otherwise = ["destructible"]

-- Break
adjectify (_, Break, _) = ["destructible"]

-- System
adjectify (_, System, _) = ["self-aware"]

adjectify _ = []


--------------------------------------------------------------------------------
-- NOUNIFY
-- Produce nouns to name an item.
--------------------------------------------------------------------------------
nounify::([Module], Module, [Module])->[String]

-- Magnify
nounify (_, Magnify n, _)
  | n > 100 = ["carbon-fiber"]
  | n > 50  = ["titanium"]
  | otherwise = []

-- Walk
nounify (_, (Walk prop), _)
  | smooth > 5 && rough > 5 = ["utility boots", ""]
  | otherwise               = ["shoes", ""]
  where smooth = walkSmooth prop
        rough  = walkRough  prop

-- Rad

-- Fly
nounify (Magnify n : _, Fly, _)
  | n > 50    = ["rocket pods", ""]
  | n > 30    = ["wings", ""]
  | n > 10    = ["jetpack", ""]
  | otherwise = ["stabilizers", ""]

nounify (_, Fly, _) = ["fins", ""]

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

-- Grasp
nounify (Magnify n : _, Grasp, _)
  | n > 20 = ["vice grip", ""]
  | n > 15 = ["claw", ""]
  | n > 10 = ["hand", ""]
  | otherwise = ["grabber", ""]
nounify (_, Grasp, _) = ["grabber", ""]

-- Carry
nounify (Magnify n : _, Carry, _)
  | n > 80  = ["TARDIS", ""]
  | n > 30  = ["storage unit", ""]
  | n > 10  = ["backpack", ""]
  | otherwise = ["satchel", ""]

nounify (_, Carry, _) = ["pouch", ""]

-- Explode
nounify (Magnify n : _, Explode, _)
  | n > 50    = ["bomb", ""]
  | n > 25    = ["stick of dynamite", ""]
  | otherwise = ["firecracker", ""]

-- Fire

-- Illuminate
nounify (Magnify n : _, Illuminate, _)
  | n > 20    = ["beacon", ""]
  | n > 10    = ["flashlight", ""]
  | otherwise = ["candle", ""]

-- Upgrade

-- Expend

-- Break

-- System


nounify _ = []

--------------------------------------------------------------------------------
-- Item generation:
--------------------------------------------------------------------------------

combine::Item->Item->Item
combine ms ns = normalize $ lgen 1 $ alternate ms ns

alternate::[a]->[a]->[a]
alternate [] xs = xs
alternate (y:ys) xs = y : (alternate xs ys)

-- Random item generation ------------------------------------------------------
getRandom::(Random a) => (a, a) -> State StdGen a
getRandom (low, hi) = state (randomR (low, hi))

getRandoms::(Random a) => (a, a) -> Int -> State StdGen [a]
getRandoms (low, hi) n = replicateM n (getRandom (low, hi))

newItem::State StdGen Item
newItem = do
  numMods <- getRandom (0, 100)
  nums <- getRandoms (0, num_modules - 1) numMods
  modules <- mapM generateModule nums
  rounds <- getRandom (1, 50)
  let item = normalize $ lgen rounds modules
  return $ item

generateModule::Int->State StdGen Module
generateModule 0 = do
  n <- getRandom (1, 30)
  return $ Magnify n

generateModule 1 = do
  r <- getRandom (1, 5)
  s <- getRandom (1, 10)
  return $ toWalk [Smooth s, Rough r]

generateModule 2 = return Rad

generateModule 3 = return Fly

generateModule 4 = do
  n <- getRandom (0, num_examineprops - 1)
  return $ toExamine [toEnum n]

generateModule 5 = return Grasp

generateModule 6 = return Carry

generateModule 7 = return Explode

generateModule 8 = return Fire

generateModule 9 = return Illuminate

generateModule 10 = return Upgrade

generateModule 11 = do
  n <- getRandom (1, 10)
  return $ Expend n

generateModule 12 = return Break

-- "Normalize" a list of modules (e.g. condense magnitudes).
normalize::[Module]->[Module]
normalize = \x -> x

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
  -- Magnifications
  (_, Magnify a, Magnify b : _) -> [Magnify (a + b)]
  (Magnify _ : _, Magnify _, _) -> []

  -- Expendable things
  (_, Expend b, Expend a : _) -> [Expend (a + b)]
  (Expend a : _, Expend _, _) -> []

  (Expend 0 : _, _, _)      -> [Expend 0] -- Expend a property

  -- Combine walks
  (_, Walk a, (Walk b : _)) -> [Walk (combineProp a b)]
  (Walk a : _ , Walk _, _)  -> []

  -- Eliminate consecutive Grasps
  (_, Grasp, Grasp : _) -> [Magnify 2, Grasp]
  (Grasp : _, Grasp, _) -> []

  (Magnify a : _, Grasp, Magnify b : Grasp : _) -> [Magnify (a + b), Grasp]
  (Grasp : _, Magnify a, Grasp : _) -> []
  (Magnify a : Grasp : _, Grasp, _) -> []

  _ -> [m]
