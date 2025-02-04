signature EXECUTE = sig
  type vec = Complex.complex vector
  val exec : Circuit.t -> vec -> vec
end

structure Execute :> EXECUTE = struct

  infix |> fun x |> f = f x

  structure C = Complex
  structure CA = ComplexArray
  structure PA = ComplexPullArray
  structure PA2 = ComplexPullArray2

  type complex = C.complex

  type vec = complex vector

  fun pow2 n = if n <= 0 then 1 else 2 * pow2(n-1)

  (* Execution (imperative interpreter) *)
  infix 7 quot rem
  val op quot = Int.quot
  val op rem = Int.rem

  type parray = PA.parray
  type parray2 = PA2.parray2

  val rsqrt2 = C.fromRe (1.0 / Math.sqrt 2.0)
  val rsqrt2eipi4 = C.*(rsqrt2,C.exp(C.fromIm(Math.pi/4.0)))

  open Circuit

  fun allI t =
      case t of
          I => true
        | Tensor(t1,t2) => allI t1 andalso allI t2
        | Seq(t1,t2) => allI t1 andalso allI t2
        | _ => false

  fun exec0 t (a:parray) : unit =
      if allI t then () else
      case t of
          Seq(t1,t2) => (exec0 t1 a; exec0 t2 a)
        | Tensor(A,B) =>
          let val V = PA2.unflatten (pow2(height A),pow2(height B),a)
          in PA2.appRows (exec0 B) V
           ; PA2.appRows (exec0 A) (PA2.transpose V)
          end
        | C A => exec0 A (PA.drop (PA.length a quot 2,a))
        | I => ()
        | X => let val x = PA.sub(a,0)
               in PA.update(a,0,PA.sub(a,1)); PA.update(a,1,x)
               end
        | Y => let val x = PA.sub(a,0)
               in PA.update(a,0,C.*(C.fromIm ~1.0, PA.sub(a,1)));
                  PA.update(a,1,C.*(C.fromIm 1.0, x))
               end
        | Z => PA.update(a,1,C.~(PA.sub(a,1)))
        | H => let val x = PA.sub(a,0)
                   val y = PA.sub(a,1)
                   val x' = C.+(C.*(rsqrt2,x),C.*(rsqrt2,y))
                   val y' = C.-(C.*(rsqrt2,x),C.*(rsqrt2,y))
               in PA.update(a,0,x')
                ; PA.update(a,1,y')
               end
        | T => let val x = PA.sub(a,0)
                   val y = PA.sub(a,1)
                   val x' = C.*(rsqrt2,x)
                   val y' = C.*(rsqrt2eipi4,y)
               in PA.update(a,0,x')
                ; PA.update(a,1,y')
               end
        | SW => let val x = PA.sub(a,1)
                    val y = PA.sub(a,2)
                in PA.update(a,1,y)
                 ; PA.update(a,2,x)
                end

  fun exec (t:Circuit.t) (v:vec) : vec =
      let val a = CA.tabulate(Vector.length v, fn i => Vector.sub(v,i))
      in exec0 t (PA.full a)
       ; Vector.tabulate(Vector.length v,fn i => CA.sub(a,i))
      end

end
