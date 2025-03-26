-- Quantum benchmark: Fusion (fuse gate operations)
-- Desc: Fuse a sequence of gate operations within a repeat construct

import "dqfut"
open gates

def fuse (n:i64) : f64 =
  let s = fromKet (replicate n 0)
          |*> repeat (n-1) (cntrlZ 1)
	  |*> repeat n (\q ->
			  gateH q >* gateZ q >* gateT q)
  in dist s |> distmax |> (.1)

def main (n:i64) : f64 =
  fuse n

-- ==
-- entry: main
-- input { 2i64 } output { 0.25f64 }
