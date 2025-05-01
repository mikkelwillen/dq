import "lib/github.com/diku-dk/complex/complex"

module type gates = {

  module complex : complex
  type r = complex.real     -- type of real numbers
  type c = complex.complex  -- type of complex numbers
  type q = i64              -- qubit index

  type^ gate_snd = c -> c                            -- C -> C; gate on the form [[1,0],[0,a]]
  type^ gate = c -> c -> (c,c)                       -- C^2 -> C^2
  type^ gate2 = c -> c -> c -> c -> (c,c,c,c)        -- C^4 -> C^4

  type st[n] = [2**n]c                               -- state vector
  type^ stT[n] = *st[n] -> *st[n]	             -- state transformer

  val gate_sndC [n] : (m:i64) -> q -> gate_snd -> stT[n]  -- S, ...
  val gateC     [n] : (m:i64) -> q -> gate -> stT[n]      -- X, H, ...
  val gate2     [n] : q -> gate2 -> stT[n]                -- e.g., swap

  val gateX   [n] : q -> stT[n]                      -- 0 <= q < n
  val gateY   [n] : q -> stT[n]                      -- 0 <= q < n
  val gateZ   [n] : q -> stT[n]                      -- 0 <= q < n
  val gateH   [n] : q -> stT[n]                      -- 0 <= q < n
  val gateT   [n] : q -> stT[n]                      -- 0 <= q < n
  val gateS   [n] : q -> stT[n]                      -- 0 <= q < n
  val gateSd  [n] : q -> stT[n]                      -- 0 <= q < n
  val gateR   [n] : r -> q -> stT[n]                 -- 0 <= q < n
  val gateRx  [n] : r -> q -> stT[n]                 -- 0 <= q < n
  val gateRy  [n] : r -> q -> stT[n]                 -- 0 <= q < n
  val gateRz  [n] : r -> q -> stT[n]                 -- 0 <= q < n
  val gateSX  [n] : q -> stT[n]                      -- 0 <= q < n
  val gateSY  [n] : q -> stT[n]                      -- 0 <= q < n

  val cntrlX  [n] : (m:i64) -> q -> stT[n]           -- 0 <= q < n - m
  val cntrlY  [n] : (m:i64) -> q -> stT[n]           -- 0 <= q < n - m
  val cntrlZ  [n] : (m:i64) -> q -> stT[n]           -- 0 <= q < n - m
  val cntrlH  [n] : (m:i64) -> q -> stT[n]           -- 0 <= q < n - m
  val cntrlT  [n] : (m:i64) -> q -> stT[n]           -- 0 <= q < n - m
  val cntrlS  [n] : (m:i64) -> q -> stT[n]           -- 0 <= q < n - m
  val cntrlSd [n] : (m:i64) -> q -> stT[n]           -- 0 <= q < n - m
  val cntrlR  [n] : (m:i64) -> r -> q -> stT[n]      -- 0 <= q < n - m
  val cntrlRx [n] : (m:i64) -> r -> q -> stT[n]      -- 0 <= q < n - m
  val cntrlRy [n] : (m:i64) -> r -> q -> stT[n]      -- 0 <= q < n - m
  val cntrlRz [n] : (m:i64) -> r -> q -> stT[n]      -- 0 <= q < n - m
  val cntrlSX [n] : (m:i64) -> q -> stT[n]           -- 0 <= q < n - m
  val cntrlSY [n] : (m:i64) -> q -> stT[n]           -- 0 <= q < n - m

  val swap    [n] : q -> stT[n]                      -- 0 <= q < n - 1
  val swap2   [n] : (q:q) -> (r:q) -> stT[n]         -- 0 <= q < n - 1, q < r < n

  val lsb_toggle [n] : stT[n]

  type ket[n] = [n]i64
  val fromKet [n] : ket[n] -> *st[n]
  val toKet       : (n:i64) -> (i:i64) -> ket[n]

  type dist[n] = [2**n](ket[n],r)

  val dist       [n] : st[n] -> dist[n]
  val distmax    [n] : dist[n] -> (ket[n],r)

  -- Some utility functions
  val <*<      'a : (q -> *a -> *a) -> (q -> *a -> *a) -> (q -> *a -> *a)
  val >*>      'a : (q -> *a -> *a) -> (q -> *a -> *a) -> (q -> *a -> *a)
  val |*>   'a 'b : *a -> (*a -> *b) -> *b
  val >* 'a 'b 'c : (*a -> *b) -> (*b -> *c) -> (*a -> *c)
  val repeat   'a : i64 -> (i64 -> *a -> *a) -> *a -> *a
}


module mk_gates (W:real) : gates with r = W.t = {

  module complex = mk_complex(W)
  type r = W.t
  type c = complex.complex
  type q = i64

  type^ gate_snd = c -> c
  type^ gate = c -> c -> (c,c)
  type^ gate2 = c -> c -> c -> c -> (c,c,c,c)

  type st[n] = [2**n]c
  type^ stT[n] = *st[n] -> *st[n]

  def i = complex.mk_im (W.i64 1)
  def ni = complex.mk_im (W.i64 (-1))
  def rsqrt2 = complex.mk_re (W.(i64 1 / sqrt (i64 2)))
  def ipi4 = complex.mk_im (W.(pi / i64 4))
  def eipi4 = complex.(exp ipi4)
  def ipi2 = complex.mk_im (W.(pi / i64 2))
  def nipi2 = complex.mk_im (W.(pi / (i64 (-2))))
  def eipi2 = complex.(exp ipi2)
  def enipi2 = complex.(exp nipi2)
  def c1 = complex.mk_re (W.i64 1)
  def c1i = complex.(c1+i)
  def c1ni = complex.(c1-i)
  def c1ihalf = complex.(c1i / mk_re (W.i64 2))
  def X a b = (b, a)
  def Y a b = complex.((ni*b, i*a))
  def Z a b = complex.((a, neg b))
  def H a b = complex.((a*rsqrt2+b*rsqrt2, a*rsqrt2-b*rsqrt2))
  def T a b = complex.((a, b*eipi4))
  def S a b = complex.((a, b*i))
  def Sd a b = complex.((a, b*ni))

  def R p a b = complex.((a, b*exp(mk_im p)))

  def Rx (p:r) (a:c) (b:c) : (c,c) =
    let p2 = W.(p / i64 2)
    let cosp2 = complex.mk_re (W.cos p2)
    let isinp2 = complex.(ni * mk_re(W.sin p2))
    in complex.((a*cosp2 - b*isinp2, b*cosp2 - a*isinp2))

  def Ry p a b =
    let p2 = W.(p / i64 2)
    let cosp2 = complex.mk_re (W.cos p2)
    let sinp2 = complex.mk_re (W.sin p2)
    in complex.((a*cosp2 - b*sinp2, b*cosp2 + a*sinp2))

  def Rz p a b =
    let p2 = W.(p / i64 2)
    let np2 = W.(p / i64 (-2))
    in complex.((a*exp(mk_im np2),
		 b*exp(mk_im p2)))

  def SX a b = complex.((c1i*a+c1ni*b, c1ni*a+c1i*b))
  def SY a b = complex.((c1ihalf*a-c1ihalf*b,c1ihalf*a+c1ihalf*b))

  def vec 'a [m][n] (x:*[m][n]a) : *[n*m]a =
    flatten(transpose x)

  def unvec 'a [m][n] (x:*[m*n]a) : *[n][m]a =
    transpose(unflatten x)

  def umap 'a 'b [n] (f : *a -> *b) (v:*[n]a) : *[n]b =
    map (\u : *b -> f (copy u)) v

  -- this version is currently problematic - using futhark test -i works, also with
  -- Futhark 0.25.28
  def gate_vecn [k] (n:i64) (q:q) (g:[2**n]c -> [2**n]c) (v: *st[k]) : *st[k] =
    assert (0 <= q && q < k-n+1)
    (let v = v :> *[(2**q*2**n)*2**(k-q-n)]c
     let f u = flatten(map g (unflatten u))
     in vec(map f (unvec v)) :> *st[k])

  def gate_vec [k] (q:q) (g:*[2]c -> *[2]c) (v: *st[k]) : *st[k] =
     assert (0 <= q && q < k)
     (let v = v :> *[(2**q*2)*2**(k-q-1)]c
      let f (u:*[2**q*2]c) : *[2**q*2]c = flatten(umap g (unflatten u))
      in vec(umap f (unvec v)) :> *st[k])

-- Replace the above with the below to see the problem with 0.25.28 when
-- running futhark test dqfut_test.fut
  --def gate_vec [k] (q:q) (g:[2]c -> [2]c) (v: *st[k]) : *st[k] =
  --  gate_vecn 1 q (g :> [2**1]c -> [2**1]c) v

  def gate [k] (q:q) (g:gate) (v: *st[k]) : *st[k] =
    let g p : *[2]c = let p = p :> *[2]c
		      let (x,y) = g p[0] p[1]
		      in [x,y] :> *[2]c
    in gate_vec q g v

  def gate_snd [k] (q:q) (g:gate_snd) (v: *st[k]) : *st[k] = -- e.g., for Z
    gate_vec q (\p : *[2]c -> [p[0],g p[1]]) v

  def dst (n:i64) (q:i64) : i64 = 2**(n-q-1)

  def gateC [k] (c:i64) (q:q) (g:gate) (v: *st[k]) : *st[k] =
    let d = dst k (q+c)
    let xs = map (\i -> map (\j -> 2*d*((i+1) * 2**c - 1) + j) (iota d)
                 ) (iota (2**q)) |> flatten
    let ys = map (\x -> x+d) xs
    let ccs = map2 (\x y -> g v[x] v[y]) xs ys
    in scatter v (xs++ys) (map (.0) ccs ++ map (.1) ccs)

  def gate_sndC [k] (c:i64) (q:q) (g:gate_snd) (v: *st[k]) : *st[k] =
    let d = dst k (q+c)
    let xs = map (\i -> map (\j -> 2*d*((i+1) * 2**c - 1) + j + d) (iota d)
                 ) (iota (2**q)) |> flatten
    let ccs = map (\x -> g v[x]) xs
    in scatter v xs ccs

  def gate2 [k] (q:q) (g:gate2) (v: *st[k]) : *st[k] =
--    let g p =
--      let p = p :> [4]c
--      let (a,b,c,d) = g p[0] p[1] p[2] p[3]
--      in [a,b,c,d] :> [2**2]c
--    in gate_vecn 2 q g v
    assert (0 <= q && q < k-1)
    (let v = v :> *[(2**q*4)*2**(k-q-2)]c
     let f u = flatten(map (\p -> let (a,b,c,d) = g p[0] p[1] p[2] p[3]
                                  in [a,b,c,d]) (unflatten u))
     in vec(map f (unvec v)) :> *st[k])

  -- def gate2 [n] (q:q) (g:gate2) (v: *st[n]) : *st[n] =
  --   let d = dst n (q+1)
  --   let xs = map (\i -> map (\j -> 4*d*i+j) (iota d)
  --                ) (iota (2**q)) |> flatten
  --   let ys = map (\x -> (x+d,x+d+d,x+d+d+d)) xs
  --   let ccs = map2 (\x y -> g v[x] v[y.0] v[y.1] v[y.2]) xs ys
  --   in scatter v (xs++map(.0)ys++map(.1)ys++map(.2)ys)
  --              (map(.0)ccs++map(.1)ccs++map(.2)ccs++map(.3)ccs)

  def swap [n] (q:q) (v: *st[n]) : *st[n] =
    gate2 q (\a b c d -> (a,c,b,d)) v

  -- def swap [n] (q:q) (v: *st[n]) : *st[n] =
  --   let v = v :> *[(2**q*4)*2**(n-q-2)]c
  --   let g p = let x = p[1]
  --             in copy p with [2] = x
  --                       with [1] = p[2]
  --   let f u = flatten(map g (unflatten u))
  --   in vec(map f (unvec v)) :> *st[n]

  def up [k] (q:q) (r:q) (v:*st[k]) : *st[k] =         -- 0 <= q < n - 1, q < r < n
    let n = r-q
    let v = v :> *[2**(r+1)*2**(k-r-1)]c
    let up (u : *[2**(n+1)]c) : *[2*2**n]c =
      let u = u :> *[2**n*2]c
      in flatten(unvec u)
    let f (u : *[2**(r+1)]c) : *[2**q*(2*2**n)]c =
      let u = u :> *[2**q*2**(n+1)]c
      in flatten(umap up (unflatten u))
    in vec(umap f (unvec v)) :> *st[k]

  def down [k] (q:q) (r:q) (v:*st[k]) : *st[k] =         -- 0 <= q < n - 1, q < r < n
    let n = r-q
    let v = v :> *[2**(r+1)*2**(k-r-1)]c
    let dn (u : *[2**(n+1)]c) : *[2**n*2]c =
      let u = u :> *[2*2**n]c
      in flatten(unvec u)
    let f (u : *[2**(r+1)]c) : *[2**q*(2**n*2)]c =
      let u = u :> *[2**q*2**(n+1)]c
      in flatten(umap dn (unflatten u))
    in vec(umap f (unvec v)) :> *st[k]

  #[inline]
  def swap2 [k] (q:q) (r:q) (v:*st[k]) : *st[k] =
    assert (r > q && 0 <= q && q < k && 0 < r && r < k)
	   (down (q+1) r (up q r v))

  -- Non-scatter optimal version - however, the copy is very bad if c is large
  -- I would like map to support that if p is unique then the updates can be
  -- implemented in-place...
  --
  -- Here Futhark suffers from the rule that "A function that consumes one of
  -- its arguments may not be passed as a higher-order argument to another
  -- function..."

  def gateC_new [n] (c:i64) (q:q) (g:gate) (v: *st[n]) : *st[n] =
    let v = v :> *[(2**q*2**(c+1))*2**(n-q-c-1)]c
    let f u = flatten(map (\(p:[2**(c+1)]c) ->
                             let i = 2**(c+1)-2
                             let (x,y) = g p[i] p[i+1]
                             in copy p with [i] = x
                                       with [i+1] = y)
                          (unflatten u))
    in vec(map f (unvec v)) :> *st[n]

  def gateC_new' [n] (c:i64) (q:q) (g:gate) (v: *st[n]) : *st[n] =
    let v = v :> *[(2**q*2**(c+1))*2**(n-q-c-1)]c
    let f (u : [2**q*2**(c+1)]c) =
      let u2 : [2**q][2**(c+1)]c = unflatten (copy u)
      let p2 : [2**q][2]c = map (\p -> let i = 2**(c+1)-2
                                       let (x,y) = g p[i] p[i+1]
                                       in [x,y])
                                u2
      let u2[:,:-2] = p2
      in flatten u2
    in vec(map f (unvec v)) :> *st[n]

  -- The following version is the same as the above - but it results in a buggy
  -- execution if the function is used as a replacement of gateC. Executing
  -- "futhark test qsim_test.fut" issues errors whereas "futhark test -i
  -- qsim_test.fut" does not.

  def gateC_newinternalbug [n] (c:i64) (q:q) (g:gate) (v: *st[n]) : *st[n] =
    let v = v :> *[(2**q*2**(c+1))*2**(n-q-c-1)]c
    let f u = flatten(map (\p -> let i = 2**(c+1)-2
                                 let (x,y) = g p[i] p[i+1]
                                 in copy p with [i] = x
                                           with [i+1] = y)
                          (unflatten u))
    in vec(map f (unvec v)) :> *st[n]

  def cntrlX [n] (m:i64) (q:q) (v: *st[n]) : *st[n] =
    gateC m q X v

  def cntrlY [n] (m:i64) (q:q) (v: *st[n]) : *st[n] =
    gateC m q Y v

  def cntrlZ [n] (m:i64) (q:q) (v: *st[n]) : *st[n] =
    gateC m q Z v

  def cntrlH [n] (m:i64) (q:q) (v: *st[n]) : *st[n] =
    gateC m q H v

  def cntrlT [n] (m:i64) (q:q) (v: *st[n]) : *st[n] =
    gateC m q T v

  def cntrlS [n] (m:i64) (q:q) (v: *st[n]) : *st[n] =
    gateC m q S v

  def cntrlSd [n] (m:i64) (q:q) (v: *st[n]) : *st[n] =
    gateC m q Sd v

  def cntrlR [n] (m:i64) (p:r) (q:q) (v: *st[n]) : *st[n] =
    gateC m q (R p) v

  def cntrlRx [n] (m:i64) (p:r) (q:q) (v: *st[n]) : *st[n] =
    gateC m q (Rx p) v

  def cntrlRy [n] (m:i64) (p:r) (q:q) (v: *st[n]) : *st[n] =
    gateC m q (Ry p) v

  def cntrlRz [n] (m:i64) (p:r) (q:q) (v: *st[n]) : *st[n] =
    gateC m q (Rz p) v

  def cntrlSX [n] (m:i64) (q:q) (v: *st[n]) : *st[n] =
    gateC m q SX v

  def cntrlSY [n] (m:i64) (q:q) (v: *st[n]) : *st[n] =
    gateC m q SY v

  def gateX [n] (q:q) (v: *st[n]) : *st[n] =
    gate q X v

  def gateY [n] (q:q) (v: *st[n]) : *st[n] =
    gate q Y v

  def gateZ [n] (q:q) (v: *st[n]) : *st[n] =
    gate_snd q (complex.neg) v

  def gateH [n] (q:q) (v: *st[n]) : *st[n] =
    gate q H v

  def gateT [n] (q:q) (v: *st[n]) : *st[n] =
    gate_snd q (\c -> complex.(c*eipi4)) v

  def gateS [n] (q:q) (v: *st[n]) : *st[n] =
    gate_snd q (\c -> complex.(c*i)) v

  def gateSd [n] (q:q) (v: *st[n]) : *st[n] =
    gate_snd q (\c -> complex.(c*ni)) v

  def gateR [n] (p:r) (q:q) (v: *st[n]) : *st[n] =
    gate_snd q (\c -> complex.(c*exp(mk_im p))) v

  def gateRx [n] (p:r) (q:q) (v: *st[n]) : *st[n] =
    gate q (Rx p) v

  def gateRy [n] (p:r) (q:q) (v: *st[n]) : *st[n] =
    gate q (Ry p) v

  def gateRz [n] (p:r) (q:q) (v: *st[n]) : *st[n] =
    gate q (Rz p) v

  def gateSX [n] (q:q) (v: *st[n]) : *st[n] =
    gate q SX v

  def gateSY [n] (q:q) (v: *st[n]) : *st[n] =
    gate q SY v

  type ket [n] = [n]i64

  def fromKet [n] (xs:[n]i64) : *st[n] =
    let j = map2 (\x i -> x * 2**i) xs (reverse(iota n))
            |> reduce (+) 0
    in map (\i -> complex.mk_re(if i==j then W.i64 1 else W.i64 0))
           (iota (2**n))

  def toKet (n:i64) (i:i64) : ket[n] =
    let res : *[n]i64 = replicate n 0
    let (res,_) = loop (r,k) = (res,i) for j < n do
                    if k % 2 == 0
                    then (r with [n-j-1] = 0, k / 2)
                    else (r with [n-j-1] = 1, k / 2)
    in res

  def sq x : r = W.(x*x)

  def dist0 [m] (v:[m]c) : [m]r =
    map (sq <-< complex.mag) v

  type dist[n] = [2**n](ket[n],r)

  def dist [n] (v:st[n]) : dist[n] =
    map2 (\i p -> (toKet n i, p)) (iota (2**n)) (dist0 v)

  def toggle (n:i64) (v:u64) : u64 =
    loop r = 0u64 for i < n do ((r << 1) | ((v >> u64.i64 i) & 1))

  def lsb_toggle [n] (s: *st[n]) : *st[n] =
    let (is, vs) = map2 (\i c ->
			   let i' = toggle n (u64.i64 i)
				    |> i64.u64
			   in (i',c)
			) (iota (2**n)) s
		   |> unzip
    in scatter s is vs

  def distmax [n] (d:dist[n]) =
    reduce (\x y -> if W.(x.1 > y.1) then x else y) d[0] d


  def unroll_factor : i64 = 8

  def repeat 'a n (f : i64 -> *a -> *a) (s:*a) : *a =
    let s = loop s for i < n/unroll_factor do
              #[unroll]
              loop s for j < unroll_factor do f (i*unroll_factor+j) s
    in loop s for i in n/unroll_factor*unroll_factor..<n do f i s

  def (<*<) 'a (f:q -> *a -> *a) (g:q -> *a -> *a) : q -> *a -> *a =
    \q (v:*a) : *a -> f q (g q v)

  def (>*>) 'a (f:q -> *a -> *a) (g:q -> *a -> *a) : q -> *a -> *a =
    \q (v:*a) : *a -> g q (f q v)

  def (|*>) 'a 'b (a:*a) (f:*a -> *b) : *b = f a

  def (>*) 'a 'b 'c (g:*a -> *b) (f:*b -> *c) : *a -> *c =
    \(s:*a) : *c -> f (g s)
}
