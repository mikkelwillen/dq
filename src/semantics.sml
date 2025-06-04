structure Semantics :> SEMANTICS = struct

  infix |> fun x |> f = f x

  structure C = Complex
  structure M = Matrix
  structure R = Random

  type complex = C.complex
  type mat = C.complex M.t

  fun pp_c (c:complex) : string =
      C.fmtBrief (StringCvt.GEN(SOME 4)) c

  fun pp_r (r:real) =
      let val s = Real.toString r
      in if String.isSuffix ".0" s then String.extract(s,0,SOME(size s-2))
         else s
      end

  fun pp_mat (m:mat) : string =
      let val m = M.map pp_c m |> M.memoize
        val sz = foldl (fn (e,a) => Int.max(a,size e)) 1
                       (List.concat (M.listlist m))
      in M.pp sz (fn x => x) m
      end

  (* Semantics *)
  fun matmul (t1:mat,t2:mat) : mat =
      M.matmul_gen C.* C.+ (C.fromInt 0) t1 t2 |> M.memoize

  (* See https://en.wikipedia.org/wiki/Kronecker_product *)
  fun tensor (a: mat,b:mat) : mat =
      let val (m,n) = M.dimensions a
        val (p,q) = M.dimensions b
      in M.tabulate(m * p, n * q,
                    fn (i,j) =>
                      C.* (M.sub(a,i div p, j div q),
                           M.sub(b,i mod p, j mod q))
                   ) |> M.memoize
      end

  (* Generalised control - see Feynman '59:
     https://iontrap.umd.edu/wp-content/uploads/2016/01/Quantum-Gates-c2.pdf,
     section 2.5.7
   *)
  fun control (m: mat) : mat =
      let val n = M.nRows m
      in M.tabulate(2*n,2*n,
                    fn (r,c) =>                        (* 1 0 0 0 *)
                      if r >= n andalso c >= n        (* 0 1 0 0 *)
                      then M.sub(m,r-n,c-n)           (* 0 0 a b *)
                      else if r = c then C.fromInt 1  (* 0 0 c d *)
                      else C.fromInt 0)
      end

  fun fromIntM iss : mat =
      M.fromListList (map (map C.fromInt) iss)

  fun sem (t:Circuit.t) : mat =
      let open Circuit
        val c0 = C.fromInt 0
        val c1 = C.fromInt 1
        val ci = C.fromIm 1.0
        val rsqrt2 = C.fromRe (1.0 / Math.sqrt 2.0)
        val eipi4 = C.exp(C.fromIm(Math.pi/4.0))
        fun negetheta2 theta = C.exp(C.fromIm(~1.0*theta/2.0))
        fun etheta2 theta = C.exp(C.fromIm(1.0*theta/2.0))
        fun add x y = C.+(x,y)
        fun sub x y = C.-(x,y)
        fun half x = C./(x,C.fromInt 2)
      in case t of
           I => fromIntM [[1,0],
                          [0,1]]
         | X => fromIntM [[0,1],
                          [1,0]]
         | Y => M.fromListList [[c0,C.~ ci],
                                [ci,c0]]
         | Z => fromIntM [[1,0],
                          [0,~1]]
         | H => M.fromListList [[rsqrt2,rsqrt2],
                                [rsqrt2,C.~ rsqrt2]]
         | T => M.fromListList [[c1,c0],
                                [c0,eipi4]]
         | S => M.fromListList [[c1,c0],
                                [c0,ci]]
         | SX => M.fromListList [[add c1 ci,sub c1 ci],
                                 [sub c1 ci,add c1 ci]]
         | SY => let val a = half(add c1 ci)
           in M.fromListList [[a,C.~a],[a,a]]
           end
         | SZ => sem S
         | SW => fromIntM [[1,0,0,0],
                           [0,0,1,0],
                           [0,1,0,0],
                           [0,0,0,1]]
         | RZ theta => M.fromListList [[negetheta2 theta, c0],
                                        [c0, etheta2 theta]]
         | Seq(t1,t2) => matmul(sem t2,sem t1)
         | Tensor(t1,t2) => tensor(sem t1,sem t2)
         | C t => control (sem t)
         | Repeat(n, t) => let val m = sem t
           in foldl (fn (_,a) => matmul(m,a)) m (List.tabulate(n-1,fn _ => m))
           end
      end

  type ket = int list (* list of 0's and 1's *)
  fun ket xs = xs
  fun pp_ket (v:ket) : string =
      "|" ^ implode (map (fn i => if i > 0 then #"1" else #"0") v) ^ ">"

  type state = complex vector

  fun log2 n = if n <= 1 then 0 else 1 + log2(n div 2)
  fun pow2 n = if n <= 0 then 1 else 2 * pow2(n-1)

  fun init (is: ket) : state =
      let val i = foldl (fn (x,a) => 2 * a + x) 0 is
      in Vector.tabulate(pow2 (length is),
                         fn j => if i = j then C.fromInt 1 else C.fromInt 0)
      end

  fun pp_state (v:state) : string =
      let val v = Vector.map pp_c v
        val sz = Vector.foldl (fn (e,a) => Int.max(a,size e)) 1 v
      in M.ppv sz (fn x => x) v
      end

  fun eval (x:Circuit.t) (v:state) : state =
      M.matvecmul_gen C.* C.+ (C.fromInt 0) (sem x) v


  (* Helper functions *)
  fun bitAt (n, i) = (n div (IntInf.pow (2, i))) mod 2 = 1

  fun complexDivReal (z, r) = C./ (z, C.fromRe r)

  (* Calculate the probability of measuring 0 and 1 *)
  (* in the given state for the specified qubit index *)
  fun calcProbability stateSize state qubitIndex =
      let
        fun loop (idx, (p0, p1)) =
            if idx >= stateSize then
              (p0, p1)
            else
              let
                val amplitude = Vector.sub (state, idx)
                val prob = Math.pow (Complex.re (Complex.abs amplitude), 2.0)
                val bit = bitAt (IntInf.fromInt idx, qubitIndex)
                val (newP0, newP1) =
                  if bit then (p0, p1 + prob) else (p0 + prob, p1)
              in
                loop (idx + 1, (newP0, newP1))
              end
      in
        loop (0, (0.0, 0.0))
      end

  (* return list of bits chosen at random *)
  (* `true` if random number is less than `prob`, `false` otherwise *)
  fun sampleBits n prob rng : bool list =
      let fun loop list i =
          if i >= n then
            list
          else
            let val randVal = R.random rng
              val randValShifted = randVal * 100000.0 - Real.fromInt (Real.trunc (randVal * 100000.0))
              val result = randValShifted < prob
            in
              loop (list @ [result]) (i + 1)
            end
      in
        loop [] 0
      end

  (* Collapse the state based on the measured bit *)
  fun collapseState stateSize state qubitIndex measuredBit =
      let
        fun collapseIndex idx =
            let
              val amplitude = Vector.sub (state, idx)
              val bit = bitAt (IntInf.fromInt idx, qubitIndex)
            in
              if bit = measuredBit then
                amplitude
              else
                C.fromRe 0.0
            end
      in
        Vector.tabulate (stateSize, collapseIndex)
      end

  (* Normalize the collapsed state *)
  fun normalizeState state =
      let
        val normSquared = Vector.foldl (fn (amp, acc) => acc + Math.pow (C.re (C.abs amp), 2.0)) 0.0 state
        val norm = Math.sqrt normSquared
      in
        if Real.==(norm, 0.0) then state
        else Vector.map (fn amp => complexDivReal (amp, norm)) state
      end

  (* Measure i'th qubit in state s, return (result, new state) *)
  (* results is chosen at random, with probability from the state *)
  fun measureQubit (state: state) (qubitIndex: int) : bool * state =
      let
        val stateSize = Vector.length state

        (* Calculate probability of measuring 0 and 1 *)
        val (_, prob1) = calcProbability stateSize state qubitIndex

        (* Generate random result based on probability *)
        val rng = R.newgen ()
        val list = sampleBits 1 prob1 rng
        val measuredBit = List.hd list

        val collapsed = collapseState stateSize state qubitIndex measuredBit

        val normalized = normalizeState collapsed

      in
        (* Print debug information *)
        print("measuredBit: " ^ Bool.toString measuredBit ^ "\n");
        print("prob1: " ^ Real.toString prob1 ^ "\n");

        (measuredBit, normalized)
      end

  (* Measure i'th qubit in the given state `n` times and return *)
  (* the result measured the most times and the new state *)
  fun measureNQubits (state: state) (qubitIndex: int) (n: int) : bool * state =
      let
        val stateSize = Vector.length state

        (* Calculate probability of measuring 0 and 1 *)
        val (_, prob1) = calcProbability stateSize state qubitIndex

        (* Generate random result based on probability n times and return *)
        (* the number found the most times *)
        val rng = R.newgen ()
        val list = sampleBits n prob1 rng
        fun countBits (list: bool list) =
            List.foldl (fn (bit, (count0, count1)) =>
                if bit then (count0, count1 + 1)
                else (count0 + 1, count1)) (0, 0) list

        val (count0, count1) = countBits list
        val measuredBit = count0 < count1

        (* Collapse the state based on the measured bit *)
        val collapsed = collapseState stateSize state qubitIndex measuredBit

        (* Normalize the collapsed state *)
        val normalized = normalizeState collapsed

      in
        (* Print debug information *)
        print("count0: " ^ Int.toString count0 ^ "\n");
        print("count1: " ^ Int.toString count1 ^ "\n");
        print("prob1: " ^ Real.toString prob1 ^ "\n");

        (measuredBit, normalized)
      end

  (* Measure i'th qubit in the given state `n`times and return the *)
  (* distribution of the results *)
  fun measureNQubitsDist (state: state) (qubitIndex: int) (n: int) : bool list =
      let
        val stateSize = Vector.length state

        (* Calculate probability of measuring 0 and 1 *)
        val (_, prob1) = calcProbability stateSize state qubitIndex

        (* Generate random result based on probability n times and return *)
        (* the distribution of the results *)
        val rng = R.newgen ()
        val list = sampleBits n prob1 rng
      in
        (* Print debug information *)
        print("prob1: " ^ Real.toString prob1 ^ "\n");

        list
      end

  (* Probability distributions *)

  type dist = (ket*real) vector

  fun pp_dist (d:dist) : string =
      Vector.foldr (fn ((k,r),a) =>
                       (pp_ket k ^ " : " ^ pp_r r) :: a) nil d
                   |> String.concatWith "\n"

  fun toKet (n:int, i:int) : ket =
      (* state i \in [0;2^n-1] among total states 2^n, in binary *)
      let val s = StringCvt.padLeft #"0" n (Int.fmt StringCvt.BIN i)
      in CharVector.foldr (fn (#"1",a) => 1::a | (_,a) => 0::a) nil s
      end

  fun dist (s:state) : real vector =
      let fun square r = r*r
      in Vector.map (square o Complex.mag) s
      end

  fun measure_dist (s:state) : dist =
      let val v = dist s
          val n = log2 (Vector.length s)
      in Vector.mapi (fn (i,p) => (toKet(n,i), p)) v
      end

  (* Interpreter *)
  infix 7 quot rem
  val op quot = Int.quot
  val op rem = Int.rem

  type vec = state

  fun flatten (a:mat) : vec =
      let val (rows,cols) = M.dimensions a
      in Vector.tabulate(rows * cols,
                         fn i => M.sub(a,i quot cols,i rem cols))
      end

  fun unflatten (r,c) (v:vec) : mat =
      if Vector.length v <> r * c then raise Fail ("unflatten(" ^ Int.toString r ^ "," ^
                                                   Int.toString c ^ ",[" ^ pp_state v ^ "])")
      else M.tabulate(r,c,fn (i,j) => Vector.sub(v,i*c+j)) (* row-major *)

  fun unvec (r:int,c:int) (v:vec) : mat =  (* create NxN matrix from vector with stacked column vectors *)
      unflatten (r,c) v |> M.transpose

  fun vec (a:mat) : vec =
      M.transpose a |> flatten

  fun vecSplit (v:vec) : vec * vec =
      let val n = Vector.length v quot 2
      in (VectorSlice.vector(VectorSlice.slice(v,0,SOME n)),
          VectorSlice.vector(VectorSlice.slice(v,n,NONE)))
      end

  fun mapRows f (a:mat) : mat =
      List.tabulate(M.nRows a,
                    fn i => f(M.row i a))
                   |> M.fromVectorList

  fun interp (t:Circuit.t) (v:vec) : vec =
      case t of
          Circuit.Seq(t1,t2) => interp t2 (interp t1 v)
        | Circuit.Tensor(A,B) =>
          let val V = unvec (pow2(Circuit.height A),pow2(Circuit.height B)) v
              val W = mapRows (interp B) (M.transpose V)
              val Y = mapRows (interp A) (M.transpose W)
          in vec Y
          end
        | Circuit.C A =>
          let val (v1,v2) = vecSplit v
          in Vector.concat[v1,interp A v2]
          end
        | Circuit.I => v
        | _ => eval t v
end
