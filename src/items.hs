module Items where

tests::[Item]
tests = [
    Item [toWalk [Smooth 3, Rough 7]],
    Item [toWalk [Smooth 10, Rough 4]],
    Item [toWalk [Smooth 14, Rough 6]],
    Item [Rad 2, toWalk [Smooth 4, Rough 6]],
    Item [Fire, toWalk [Smooth 65, Rough 15]],
    Item [Rad 245, Fire, toWalk [Smooth 13, Rough 50]]
  ]

newtype Item = Item [Module]

data Module = ModNull
            | Walk LWalkProp
            | Rad Int
            | Fly Int
            | Examine LExamineProp
            | Grasp Int
            | Carry Int
            | Explode Int
            | Fire
            | Illuminate
            | Expendable Int
            | Break
            | System
            deriving Show

-- For convenience:
toExamine props = Examine (LExamineProp props)
toWalk props = Walk (LWalkProp props)

-- Generate a description of an item.
-- Invisible items have empty descriptions.
describe::Item->Maybe String
describe (Item mods) = case nouns of
  [] -> Nothing
  _  -> Just $ (sep (commas adjectives)) ++ (head nouns)
  where adjectives = moduleFold adjectify mods
        nouns      = moduleFold nounify   mods
        sep []     = []
        sep ss     = ss ++ " "

commas = foldr (\word words -> case words of
  [] -> word
  _  -> word ++ ", " ++ words) []

-- Produce adjectives to describe an item.
adjectify::([Module], Module, [Module])->[String]
adjectify (_, Rad n, _)
  | n > 100   = ["extremely rad"]
  | n > 80    = ["sick"]
  | n > 50    = ["fashionable"]
  | n < 5     = ["embarassing"]
  | otherwise = []

adjectify (_, Fire, (Walk prop : _))
  = ["combustible"]

adjectify (_, (Walk prop), _)
  | smooth > 10 && rough > 10 = ["well-cobbled"]
  | smooth > 6  && rough > 4  = ["quick"]
  | smooth > 4  && rough > 3  = ["shoddy"]
  | otherwise                 = []
  where smooth = walkSmooth prop
        rough  = walkRough  prop

-- Produce nouns to name an item.
nounify::([Module], Module, [Module])->[String]
nounify (_, (Walk prop), _)
  | smooth > 5 && rough > 5 = ["utility boots"]
  | otherwise               = ["shoes"]
  where smooth = walkSmooth prop
        rough  = walkRough  prop

nounify _ = []

class PropertyList t where
  combineProp :: t -> t -> t

newtype LWalkProp = LWalkProp {wprop :: [WalkProp]} deriving Show
data WalkProp
  = Smooth Int  -- Can walk over smooth terrain.
  | Rough  Int  -- Can walk over rough terrain.
  deriving Show


instance PropertyList LWalkProp where
  combineProp p1 p2 = LWalkProp [Smooth (walkSmooth p1), Rough (walkRough p2)]

-- Get the smoothness/roughness values.
walkSmooth :: LWalkProp -> Int
walkSmooth prop = foldr (\x s -> case x of
  (Smooth y) -> s + y
  (Rough  _) -> s) 0 (wprop prop)
walkRough  :: LWalkProp -> Int
walkRough  prop = foldr (\x r -> case x of
  (Rough  y) -> r + y
  (Smooth _) -> r) 0 (wprop prop)

newtype LExamineProp = LExamineProp {eprop :: [ExamineProp]} deriving Show
instance PropertyList LExamineProp where
  -- Ensure each element in the list is unique
  combineProp p1 p2 = LExamineProp $
    foldr f [] ((eprop p1) ++ (eprop p2)) where
      f prop ls = if (find prop ls) then ls else prop:ls
      find prop = foldr (\p v -> v || (p == prop)) False

data ExamineProp
  = Material
  | Player
  | History
  | Whatever
  deriving (Show, Eq)

-- Folds (ish) a function over the modules. This function should take
-- a tuple and return a list. In the tuple (p, m, ms):
--    p : list of previous modules (most recent is the head)
--    m : current module
--    ms: list of remaining modules (next is the head)
moduleFold::(([Module], Module, [Module]) -> [a])->[Module]->[a]
moduleFold f = moduleFold' f [] where
  moduleFold' f p [] = []
  moduleFold' f p (m:ms) = (f (p, m, ms)) ++ (moduleFold' f (m:p) ms)

lgen::Int->[Module]->[Module]
lgen 0 m = m
lgen n m = lgen (n-1) $ moduleFold lgen_rules m

lgen_rules::([Module], Module, [Module])->[Module]
lgen_rules (p, m, ms) = case (p, m, ms) of
  (_, Expendable b, Expendable a : _) -> [Expendable (a + b)]
  (Expendable a : _, Expendable _, _) -> []
  (Expendable 0 : _, _, _)      -> [Expendable 0]

  (_, Walk a, (Walk b : _)) -> [Walk (combineProp a b)]
  (Walk a : _ , Walk _, _)  -> []

  (_, Carry a, _)               -> [Carry a, Grasp 1]
  (_, Grasp a, (Carry b : _))   -> [Grasp (a+1), Carry (b+1)]
  (Grasp a : _, Grasp b, _)     -> [Grasp (a + b)]

  (_, ModNull, _) -> []
  _ -> [m]
