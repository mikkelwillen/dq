structure Execute2 :> EXECUTE = struct

  infix |> fun x |> f = f x

  structure C = Complex
  structure CA = ComplexArray

  type complex = C.complex

  type vec = complex vector

  fun pow2 n = if n <= 0 then 1 else 2 * pow2(n-1)

  infix 7 quot
  val op quot = Int.quot

  (* Execution2 (standard imperative interpreter) *)

  val rsqrt2 = C.fromRe (1.0 / Math.sqrt 2.0)
  val rsqrt2eipi4 = C.*(rsqrt2,C.exp(C.fromIm(Math.pi/4.0)))

  open Circuit

  fun allI t =
      case t of
          I => true
        | Tensor(t1,t2) => allI t1 andalso allI t2
        | Seq(t1,t2) => allI t1 andalso allI t2
        | _ => false

  fun iter b e f =
      if b >= e then ()
      else (f b ; iter (b+1) e f)

  fun exec t (v:vec) : vec =
      let val n = height t
          fun dst q = pow2 (n-q-1)
          val arr = CA.tabulate(Vector.length v, fn i => Vector.sub(v,i))
          fun get i = CA.sub(arr,i)
          fun upd i v = CA.update(arr,i,v)
          fun gate q c f =
              let val d = dst (q+c)
              in iter 0 (pow2 q)
                      (fn i => iter 0 d
                                    (fn j => let val x = 2*d*((i+1) * pow2 c - 1) + j
                                             in f(x,x+d)
                                             end))
              end
          fun gate2 q 0 f =
              let val d = dst (q+1)
              in iter 0 (pow2 q)
                      (fn i => iter 0 d
                                    (fn j =>
                                        let val x = 4*d*i+j
                                        in f(x,x+d,x+d+d,x+d+d+d)
                                        end))
              end
            | gate2 q c f = raise Fail "conditioned swap not supported"
          fun exec0 q c t : unit =
              if allI t then () else
              case t of
                  Seq(t1,t2) => (exec0 q c t1; exec0 q c t2)
                | Tensor(t1,t2) => (exec0 q c t1; exec0 (q + height t1) c t2)
                | C t' => exec0 q (c+1) t'
                | I => ()
                | X => gate q c (fn (x,y) => let val xv = get x
                                             in upd x (get y) ; upd y xv
                                             end)
                | Y => gate q c (fn (x,y) => let val xv = get x
                                             in upd x (C.*(C.fromIm ~1.0, get y))
                                              ; upd y (C.*(C.fromIm 1.0, xv))
                                             end)
                | Z => gate q c (fn (x,y) => upd y (C.~(get y)))
                | H => gate q c (fn (x,y) =>
                                    let val xv = get x
                                        val yv = get y
                                    in upd x (C.+(C.*(rsqrt2,xv),C.*(rsqrt2,yv)))
                                     ; upd y (C.-(C.*(rsqrt2,xv),C.*(rsqrt2,yv)))
                                    end)
                | T => gate q c (fn (x,y) =>
                                    let val xv = get x
                                        val yv = get y
                                    in upd x (C.*(rsqrt2,xv))
                                     ; upd y (C.*(rsqrt2eipi4,yv))
                                    end)
                | SW => gate2 q c (fn (_,x,y,_) =>
                                      let val xv = get x
                                          val yv = get y
                                      in upd x yv
                                       ; upd y xv
                                      end)
      in exec0 0 0 t
       ; Vector.tabulate(Vector.length v,fn i => CA.sub(arr,i))
      end

end
