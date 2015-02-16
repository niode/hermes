data Module = ModNull
            | Walk Int Int
            | Examine [ExamineProp]
            | Grasp Int
            | Carry Int
            | Explode Int
            deriving Show

data ExamineProp = Material | Player | Whatever deriving Show

w::[Module]
w = [Walk 1 1, Examine [Material], Grasp 1, Carry 1]

lgen::Int->[Module]->[Module]
lgen 0 m = m
lgen n m = lgen (n-1) $ lgen_rules [ModNull] (m ++ [ModNull])

lgen_rules::[Module]->[Module]->[Module]
lgen_rules _ [] = []
lgen_rules p (m:ms) = (case (p, m, ms) of
  (_, Walk a b, (Walk c d : _)) -> [Walk (a + c) (b + d)]
  (Walk a b : _ , Walk _ _, _)  -> []
  (_, Carry a, _)               -> [Carry a, Grasp 1]
  (_, Grasp a, (Carry b : _))   -> [Grasp (a+1), Carry (b+1)]
  (Grasp a : _, Grasp b, _)     -> [Grasp (a + b)]
  (_, ModNull, _) -> []
  _ -> [m]
  ) ++ (lgen_rules (m:p) ms)
