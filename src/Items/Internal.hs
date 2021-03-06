module Items.Internal where

import System.Random
import Control.Monad.State
import Control.Monad (replicateM, mapM)
import Data.Set as Set (Set, insert, empty, toList, fromList)
import Data.List as List
import Format

-- Constants
max_mods = 100 :: Int     -- Number of modules in a generated item.
max_rounds = 100 :: Int   -- Number of rounds to run L-System.
max_magnify = 30 :: Int   -- Maximum magnitude of generated modules.
max_expend = 50 :: Int    -- Maximum expend parameter.

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
            deriving (Show, Eq)

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

-- Class for lists of properties, which must have a way to combine them.
class PropertyList t where
  combineProp :: t -> t -> t

-- Walk properties -------------------------------------------------------------
newtype LWalkProp = LWalkProp {wprop :: [WalkProp]} deriving (Show, Eq)
data WalkProp
  = Smooth Int  -- Can walk over smooth terrain.
  | Rough  Int  -- Can walk over rough terrain.
  deriving (Show, Eq)

instance PropertyList LWalkProp where
  combineProp p1 p2 = LWalkProp [Smooth (walkSmooth p1), Rough (walkRough p2)]

-- For convenience:
toWalk::[WalkProp] -> Module
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

newtype LExamineProp = LExamineProp {eprop :: [ExamineProp]} deriving (Show, Eq)
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

-- Take a random n unique elements from a list.
takeRandom::Int->[a]->State StdGen [a]
takeRandom 0 _ = return []
takeRandom _ [] = return []
takeRandom n ls = do
  i <- getRandom (0, (length ls) - 1)
  let e = ls!!i
  next <- takeRandom (n-1) ((take (i-1) ls) ++ (drop i ls))
  return $ e:next
  
-- Generate a description of an item.
-- Invisible items have empty descriptions.
describeRandom::Item->State StdGen (Maybe String)
describeRandom mods = case nouns of
  [] -> return Nothing
  _  -> do
    adjectives <- (takeRandom 5) . toList . fromList $ moduleFold adjectify mods
    return $ Just $ Format.item adjectives nouns
  where nouns = takeWhile (/= "") (moduleFold nounify mods)

describe::Item->Maybe String
describe mods = case nouns of
   -- No nouns, don't describe this.
  [] -> Nothing

  -- Combine the adjectives and nouns.
  _  -> Just $ Format.item adjectives nouns
  where -- Take the first 5 adjectives generated.
        adjectives = (take 5) . toList . fromList $ moduleFold adjectify mods
        -- Take all the nouns, but stop at a blank.
        nouns      = takeWhile (/= "") (moduleFold nounify mods)

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
  | n > 30    = ["sturdy"]
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
-- Returns a list of nouns. A null string means to terminate the noun at that
-- point; only some nouns can be combined.
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

-- Alternate items in lists.
alternate::[a]->[a]->[a]
alternate [] xs = xs
alternate (y:ys) xs = y : (alternate xs ys)

-- Random item generation ------------------------------------------------------

-- Helper functions:
getRandom::(Random a) => (a, a) -> State StdGen a
getRandom (low, hi) = state (randomR (low, hi))

getRandoms::(Random a) => (a, a) -> Int -> State StdGen [a]
getRandoms (low, hi) n = replicateM n (getRandom (low, hi))

-- Generate a new random item.
newItem::State StdGen Item
newItem = do
  numMods <- getRandom (0, max_mods)
  nums <- getRandoms (0, num_modules - 1) numMods
  modules <- mapM generateModule nums
  rounds <- getRandom (1, max_rounds)
  let item = normalize $ lgen rounds modules
  return $ item

-- Generate random modules.
generateModule::Int->State StdGen Module
generateModule 0 = getRandom (1, max_magnify) >>= return . Magnify

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

generateModule 11 = getRandom (1, max_expend) >>= return . Expend

generateModule 12 = return Break

-- "Normalize" a list of modules (e.g. condense magnitudes).
normalize::[Module]->[Module]
normalize = foldr normalize' []

normalize'::Module->[Module]->[Module]
normalize' (Magnify n) (Magnify m : ms) = Magnify (n+m) : ms

normalize' mod ms = mod:ms

lgen::Int->[Module]->[Module]
lgen 0 m = m
lgen n m = lgen (n-1) $ moduleFold lgen_rules m

--------------------------------------------------------------------------------
-- L-System generation.
--------------------------------------------------------------------------------
lgen_rules::([Module], Module, [Module])->[Module]
lgen_rules (p, m, ms) = case (p, m, ms) of
  -- Magnifications
  (_, Magnify a, Magnify b : _) -> [Magnify (a + b)]
  (Magnify _ : _, Magnify _, _) -> []

  -- Expend
  (_, Expend b, Expend a : _) -> [Expend (a + b)]
  (Expend a : _, Expend _, _) -> []

  (Expend 0 : _, _, _)      -> [Expend 0] -- Expend a property

  -- Combine walks
  (_, Walk a, (Walk b : _)) -> [Walk (combineProp a b)]
  (Walk a : _ , Walk _, _)  -> []

  -- Eliminate consecutive Grasps
  (_, Grasp, Grasp : _) -> [Magnify 2, Grasp]
  (Grasp : _, Grasp, _) -> []

  -- Carry
  (_, Carry, Carry : _) -> [Magnify 2, Carry]

  -- Fly
  (_, Fly, Fly : _) -> [Magnify 2, Fly]

  -- Illuminate
  (_, Illuminate, Illuminate : _) -> [Magnify 2, Illuminate]

  (Magnify n : _, Illuminate, _) -> if n > 20
    then [Illuminate, Fire]
    else [Illuminate]

  (m1 : _, Magnify a, m2 : _) -> if m1 == m2
    then [Magnify (a + 1), m1]
    else [Magnify a]
  
  (_, m1, Magnify _ : m2 : _) -> if m1 == m2
    then []
    else [m1]

  _ -> [m]
