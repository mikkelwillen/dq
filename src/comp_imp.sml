(* This module compiles circuits into a Futhark function that applies gates to a
   state vector in an imperative fashion. The Futhark function utilises
   functions in the accompanying Futhark library qsimutil.fut.
 *)

signature COMP_IMP = sig
  val circuitToFutFunBind : string -> Circuit.t -> string
end

structure CompImp :> COMP_IMP = struct

  fun die s = raise Fail ("CompImp: " ^ s)

  infix |> fun x |> f = f x

  structure F = Futhark

  fun i2s i = if i < 0 then "-" ^ Int.toString (~i)
              else Int.toString i

  fun stTyFromHeight h = "st[" ^ i2s h ^ "]"

  local
    open F infix >>=

    fun iConst i = CONST (i2s i)

    fun swapF q v = APP("swap", [iConst q,VAR v])

    fun checkNoC s 0 = ()
      | checkNoC s c = die (s ^ ": non-zero control")

    fun gate g 0 q v = APP("gate" ^ g, [iConst q,VAR v])
      | gate g c q v = APP("cntrl" ^ g, [iConst c,iConst q,VAR v])
  in
    fun icomp c q (t:Circuit.t) (v:exp) : exp M =
        case t of
            Circuit.I => ret v
          | Circuit.Seq(t1,t2) => icomp c q t1 v >>= (icomp c q t2)
          | Circuit.C t' => icomp (c+1) q t' v
          | Circuit.Tensor(A,B) => icomp c q A v >>= (icomp c (q+Circuit.height A) B)
          | Circuit.X => Let v >>= (ret o gate "X" c q)
          | Circuit.Y => Let v >>= (ret o gate "Y" c q)
          | Circuit.Z => Let v >>= (ret o gate "Z" c q)
          | Circuit.H => Let v >>= (ret o gate "H" c q)
          | Circuit.T => Let v >>= (ret o gate "T" c q)
          | Circuit.S => Let v >>= (ret o gate "S" c q)
          | Circuit.SX => Let v >>= (ret o gate "SX" c q)
          | Circuit.SY => Let v >>= (ret o gate "SY" c q)
          | Circuit.SZ => Let v >>= (ret o gate "SZ" c q)
          | Circuit.SW => (checkNoC "SW" c; Let v >>= (ret o swapF q))
  end

  fun circuitToFutFunBind (f:string) (t:Circuit.t) : string =
      let open F infix >>=
          val ty = "*" ^ stTyFromHeight (Circuit.height t)
      in runBinds (FunNamed f (icomp 0 0 t) ty ty >>= (fn _ =>
                   ret()))
      end

end
