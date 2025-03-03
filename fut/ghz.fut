-- Quantum benchmark: GHZ (
-- Desc: Data-parallel simulation of Grover's algorithm in Futhark
-- Spec: main(n): For a number of qubits perform an extangled state
--       between zero-state and 1-state.
-- Origin: Ported from Supermarq
import "qsim"

open gates

-- return probabilities of states |0..0> and |1..1>
def ghz n : (f64, f64) =
  let s = fromKet (replicate n 0)
  let s = gateH 0 s
  let s = repeat (n-1)
		 (\q (s:*st[n]) : *st[n] -> cntrlX 1 q s) s
  let d = dist s
  in (d[0].1, d[2**n-1].1)

def approx a b = f64.abs(a-b) < 0.01

entry ghz_test (n:i64) : (f64,f64) =
  ghz n

def main n =
  let (a,b) = ghz n
  in approx a 0.5 && approx b 0.5

-- ==
-- entry: ghz_test
-- nobench input { 8i64 }
-- output { 0.5f64 0.5f64 }
-- nobench input { 12i64 }
-- output { 0.5f64 0.5f64 }

-- ==
-- entry: main
-- input { 8i64 } output { true }
-- input { 12i64 } output { true }
-- disable input { 13i64 } output { true }
-- disable input { 14i64 } output { true }
-- disable input { 15i64 } output { true }
-- disable input { 17i64 } output { true }
-- disable input { 18i64 } output { true }
-- disable input { 19i64 } output { true }
-- disable input { 20i64 } output { true }
-- disable input { 21i64 } output { true }
-- disable input { 22i64 } output { true }
-- disable input { 23i64 } output { true }
-- disable input { 24i64 } output { true }
-- disable input { 25i64 } output { true }
-- disable input { 26i64 } output { true }
-- disable input { 27i64 } output { true }
-- disable input { 30i64 } output { true }

-- disable input { 28i64 } output { true }
-- disable input { 29i64 } output { true }
