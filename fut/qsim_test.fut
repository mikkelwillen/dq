import "qsim"

module g = gates

def test_circuit [n] (f: *g.st[n] -> *g.st[n]) (k:[n]i64) : [n]i64 =
  let v = g.fromKet k
  let v = f v
  let d = g.dist v
  let m = g.distmax d
  in m.0

entry test_gateX [n] (q:i64) (k:[n]i64) : [n]i64 =
  test_circuit (g.gateX q) k

-- ==
-- entry: test_gateX
-- input { 0i64 [0i64] }
-- output { [1i64] }
-- input { 0i64 [0i64,0i64] }
-- output { [1i64,0i64] }
-- input { 1i64 [0i64,0i64] }
-- output { [0i64,1i64] }
-- input { 2i64 [0i64,0i64,0i64,0i64] }
-- output { [0i64,0i64,1i64,0i64] }

entry test_cntrlX10 [n] (k:[n]i64) : [n]i64 =
  test_circuit (g.cntrlX 1 0) k

-- ==
-- entry: test_cntrlX10
-- input { [0i64,0i64] }
-- output { [0i64,0i64] }
-- input { [1i64,0i64] }
-- output { [1i64,1i64] }
-- input { [1i64,1i64] }
-- output { [1i64,0i64] }

entry test_swap [n] (q:i64) (k:[n]i64) : [n]i64 =
  test_circuit (g.swap q) k

-- ==
-- entry: test_swap
-- input { 0i64 [0i64,0i64] }
-- output { [0i64,0i64] }
-- input { 0i64 [0i64,1i64] }
-- output { [1i64,0i64] }
-- input { 0i64 [1i64,0i64] }
-- output { [0i64,1i64] }
-- input { 0i64 [0i64,1i64,1i64] }
-- output { [1i64,0i64,1i64] }
-- input { 1i64 [0i64,0i64,1i64] }
-- output { [0i64,1i64,0i64] }
-- input { 1i64 [1i64,0i64,1i64] }
-- output { [1i64,1i64,0i64] }
-- input { 2i64 [1i64,0i64,1i64,0i64,0i64] }
-- output { [1i64,0i64,0i64,1i64,0i64] }

entry test_cntrlX2 [n] (k:[n]i64) : [n]i64 =
  test_circuit (g.cntrlX 2 0) k

-- ==
-- entry: test_cntrlX2
-- input { [0i64,0i64,0i64] }
-- output { [0i64,0i64,0i64] }
-- input { [0i64,1i64,0i64] }
-- output { [0i64,1i64,0i64] }
-- input { [1i64,1i64,0i64] }
-- output { [1i64,1i64,1i64] }

entry test_cntrlX11 [n] (k:[n]i64) : [n]i64 =
  test_circuit (g.cntrlX 1 1) k

-- ==
-- entry: test_cntrlX11
-- input { [0i64,0i64,0i64] }
-- output { [0i64,0i64,0i64] }
-- input { [0i64,1i64,0i64] }
-- output { [0i64,1i64,1i64] }
-- input { [1i64,1i64,0i64] }
-- output { [1i64,1i64,1i64] }
-- input { [1i64,0i64,0i64] }
-- output { [1i64,0i64,0i64] }

entry test_cntrlX [n] (c:i64) (q:i64) (k:[n]i64) : [n]i64 =
  test_circuit (g.cntrlX c q) k

-- ==
-- entry: test_cntrlX
-- input { 1i64 1i64 [1i64,1i64,0i64,0i64] }
-- output { [1i64,1i64,1i64,0i64] }
-- input { 1i64 2i64 [1i64,1i64,0i64,0i64] }
-- output { [1i64,1i64,0i64,0i64] }
-- input { 1i64 1i64 [1i64,0i64,1i64,0i64] }
-- output { [1i64,0i64,1i64,0i64] }
-- input { 2i64 0i64 [1i64,1i64,0i64,0i64] }
-- output { [1i64,1i64,1i64,0i64] }
-- input { 2i64 1i64 [1i64,1i64,0i64,0i64] }
-- output { [1i64,1i64,0i64,0i64] }
-- input { 2i64 1i64 [1i64,1i64,1i64,0i64] }
-- output { [1i64,1i64,1i64,1i64] }
-- input { 3i64 0i64 [1i64,1i64,1i64,0i64] }
-- output { [1i64,1i64,1i64,1i64] }
