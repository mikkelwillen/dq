-- Quantum benchmark: Grover
-- Desc: Data-parallel simulation of Grover's algorithm in Futhark
-- Spec: main(n): For a number of qubits n > 4, search for the value 12.

import "dqfut"
open gates

def grover_diff [n] : stT[n] =
     repeat n (gateH >*> gateX)
  >* gateH (n-1)
  >* cntrlX (n-1) 0
  >* gateH (n-1)
  >* repeat n (gateX >*> gateH)

def encNum [n] (i:i64) (s:*st[n]) : *st[n] =
  (loop (s,i) = (s,i) for n in n..>0 do
     if i % 2 == 0
     then (gateX (n-1) s, i/2)
     else (s,i/2)
  ).0

def oracle [n] i : stT[n] =
  encNum i >* cntrlZ (n-1) 0 >* encNum i

def grover (n:i64) (i:i64) : (ket[n], f64) =
  let k = 2**n |> f64.i64 |> (*(f64.pi/4))
          |> f64.sqrt |> f64.ceil |> i64.f64
  let s = fromKet (replicate n 0)
      |*> repeat n gateH
      |*> repeat k (\_ ->
		      oracle i >* grover_diff)
  in dist s |> distmax

-- Grover's algorithm searches for the index where the oracle
-- returns 1, which it does for the binary encoding of the
-- integer argument 12 (< 2**n). See tests below...

def main n = grover n 12

entry test_grover n i = (grover n i).0

-- ==
-- entry: test_grover
-- nobench input { 8i64 12i64 }
-- output { [0i64, 0i64, 0i64, 0i64, 1i64, 1i64, 0i64, 0i64] }
-- nobench input { 9i64 13i64 }
-- output { [0i64, 0i64, 0i64, 0i64, 0i64, 1i64, 1i64, 0i64, 1i64] }

entry bench_grover n =
  let v = (grover n 12).0
  in (reverse v)[0:5] == [0,0,1,1,0]

-- ==
-- entry: bench_grover
-- input { 5i64 } output { true }
-- input { 6i64 } output { true }
-- input { 7i64 } output { true }
-- notest input { 8i64 } output { true }
-- notest input { 9i64 } output { true }
-- notest input { 10i64 } output { true }
-- notest input { 11i64 } output { true }
-- notest input { 12i64 } output { true }
-- notest input { 13i64 } output { true }
-- notest input { 14i64 } output { true }
-- notest input { 15i64 } output { true }
