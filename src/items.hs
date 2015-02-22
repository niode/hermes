module Items where

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


data Kernel = KA | KB | KC | KD | KE
instance Show Kernel where
  show KA = "a"
  show KB = "b"
  show KC = "c"
  show KD = "d"
  show KE = "e"

parseKernel::String->[Kernel]
parseKernel [] = []
parseKernel ('a':k) = (KA :) $ parseKernel k
parseKernel ('b':k) = (KB :) $ parseKernel k
parseKernel ('c':k) = (KC :) $ parseKernel k
parseKernel ('d':k) = (KD :) $ parseKernel k
parseKernel ('e':k) = (KE :) $ parseKernel k

kgen::Int->[Kernel]->[Kernel]
kgen 0 k = k
kgen n k = kgen (n - 1) $ kgen_rules [] k

kgen_rules::[Kernel]->[Kernel]->[Kernel]
kgen_rules _ [] = []
kgen_rules p (k:ks) = (case (p, k, ks) of
    (_, KA, KB : _) -> [KA, KC, KB]
    (_, KA, KC : _) -> [KA, KB, KC]
    (_, KA, KA : _) -> [KB, KB, KB]
    (KA : _, KB, KC: _) -> [KD]
    (KB : _, KC, KA : _) -> []
    (KD : _, _, _) -> []
    _ -> [k]
  ) ++ (kgen_rules (k:p) ks)
