import "dqfut"

open mk_gates(f64)

def test_circuit [n] (f: *st[n] -> *st[n]) (k:[n]i64) : [n]i64 =
  let v = fromKet k
  let v = f v
  let d = dist v
  let m = distmax d
  in m.0

entry test_gateX [n] (q:i64) (k:[n]i64) : [n]i64 =
  test_circuit (gateX q) k

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
-- input { 2i64 [1i64,0i64,1i64,1i64] }
-- output { [1i64,0i64,0i64,1i64] }

entry test_gateTTT [n] (q:i64) (k:[n]i64) : [n]i64 =
  test_circuit ((gateT >*> gateT >*> gateT) q) k

-- ==
-- entry: test_gateTTT
-- input { 0i64 [0i64] }
-- output { [0i64] }
-- input { 2i64 [0i64,0i64,0i64,1i64,1i64,0i64] }
-- output { [0i64,0i64,0i64,1i64,1i64,0i64] }

entry test_cntrlX10 [n] (k:[n]i64) : [n]i64 =
  test_circuit (cntrlX 1 0) k

-- ==
-- entry: test_cntrlX10
-- input { [0i64,0i64] }
-- output { [0i64,0i64] }
-- input { [1i64,0i64] }
-- output { [1i64,1i64] }
-- input { [1i64,1i64] }
-- output { [1i64,0i64] }

entry test_swap [n] (q:i64) (k:[n]i64) : [n]i64 =
  test_circuit (swap q) k

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
  test_circuit (cntrlX 2 0) k

-- ==
-- entry: test_cntrlX2
-- input { [0i64,0i64,0i64] }
-- output { [0i64,0i64,0i64] }
-- input { [0i64,1i64,0i64] }
-- output { [0i64,1i64,0i64] }
-- input { [1i64,1i64,0i64] }
-- output { [1i64,1i64,1i64] }

entry test_cntrlX11 [n] (k:[n]i64) : [n]i64 =
  test_circuit (cntrlX 1 1) k

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
  test_circuit (cntrlX c q) k

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

entry test_gate1_ids (n:i64) (q:i64) : bool =
  let k = replicate n 0
  let r = test_circuit ((gateX >*> gateX
  			       >*> gateH >*> gateX >*> gateH >*> gateZ) q) k
  in reduce (&&) true (map2 (==) k r)

-- ==
-- entry: test_gate1_ids
-- input { 3i64 1i64 }
-- output { true }
-- input { 3i64 1i64 }
-- output { true }

entry test_swap2 [n] (q:i64) (r:i64) (k:[n]i64) : [n]i64 =
  test_circuit (swap2 q r) k

-- ==
-- entry: test_swap2
-- input { 0i64 1i64 [0i64,0i64] }
-- output { [0i64,0i64] }
-- input { 0i64 1i64 [0i64,1i64] }
-- output { [1i64,0i64] }
-- input { 0i64 1i64 [1i64,0i64] }
-- output { [0i64,1i64] }
-- input { 0i64 1i64 [0i64,1i64,1i64] }
-- output { [1i64,0i64,1i64] }
-- input { 1i64 2i64 [0i64,0i64,1i64] }
-- output { [0i64,1i64,0i64] }
-- input { 1i64 2i64 [1i64,0i64,1i64] }
-- output { [1i64,1i64,0i64] }
-- input { 2i64 3i64 [1i64,0i64,1i64,0i64,0i64] }
-- output {          [1i64,0i64,0i64,1i64,0i64] }
-- input { 1i64 2i64 [1i64,0i64,1i64,0i64,0i64] }
-- output {          [1i64,1i64,0i64,0i64,0i64] }
-- input { 0i64 3i64 [1i64,0i64,1i64,0i64,0i64] }
-- output {          [0i64,0i64,1i64,1i64,0i64] }
-- input { 1i64 3i64 [1i64,0i64,1i64,0i64,0i64] }
-- output {          [1i64,0i64,1i64,0i64,0i64] }
-- input { 2i64 4i64 [1i64,0i64,1i64,0i64,0i64] }
-- output {          [1i64,0i64,0i64,0i64,1i64] }
