-- Quantum benchmark: QFT (Quantum Fourier Transform)
-- Desc: Data-parallel simulation of the Quantum Fourier Transform in Futhark
-- Spec: main(n): Perform the QFT on n qubits for the value 12 (00...001100).
-- Origin: Based on https://medium.com/@marcell.ujlaki/exploring-quantum-computing-demystifying-quantum-fourier-transformations-unveiling-the-math-with-5d74f3f8025f

import "dqfut"
open mk_gates(f64)

def init [k] (n:i64) (s: *st[k]) : *st[k] =
  (loop (s,n) = (s,n) for i < k do
     if n % 2 != 0 then (gateX i s, n / 2)
     else (s, n / 2)
  ).0

def sw [k] (p:i64) (q:i64) (s: *st[k]) : *st[k] =
  if p < q then swap2 p q s
  else if p > q then swap2 q p s
  else s

def qft_rots [k] (s: *st[k]) : *st[k] =
  loop s = s for n in k-1..>-1 do
    gateH n s |*>
    repeat n (\q -> sw n (q+1) >*
    	            cntrlR 1 (f64.pi/(f64.i64(2**(n-q)))) q >*
	            sw n (q+1)
	     )

def qft_swaps [k] (s: *st[k]) : *st[k] =
  repeat (k/2) (\q -> sw q (k-q-1)) s

-- qft on k qubits for number n
def qft (k:i64) (n:i64) : *st[k] =
  let s = fromKet (replicate k 0)
  let s = init n s
  in qft_rots s |*> qft_swaps

def unc c = (complex.re c, complex.im c)

entry bench_qft (k:i64) : (f64, f64) =
  let s = qft k 12 |*> lsb_toggle
  in unc (s[0])

-- ==
-- entry: bench_qft
-- input { 14i64 } output { 0.007812500000000f64 0f64 }
-- notest input { 15i64 } output { 0.005524271728020f64 0f64 }
-- notest input { 16i64 } output { 0.003906250000000f64 0f64 }
-- notest input { 17i64 } output { 0.002762135864010f64 0f64 }
-- notest input { 18i64 } output { 0.001953125000000f64 0f64 }
-- notest input { 19i64 } output { 0.001381067932005f64 0f64 }
-- notest input { 20i64 } output { 0.000976562500000f64 0f64 }
