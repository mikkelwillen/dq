
signature COMPLEX_ARRAY = sig
  type array
  type elem = Complex.complex
  val tabulate : int * (int -> elem) -> array
  val length   : array -> int
  val sub      : array * int -> elem
  val update   : array * int * elem -> unit
  val appi     : (int * elem -> unit) -> array -> unit
  val app      : (elem -> unit) -> array -> unit
end

structure ComplexArray0 :> COMPLEX_ARRAY = struct
  type elem = Complex.complex
  type array = elem array
  val tabulate = Array.tabulate
  val length = Array.length
  val sub = Array.sub
  val update = Array.update
  val appi = Array.appi
  val app = Array.app
end

(*
signature COMPLEX_ARRAY_SLICE = sig
  type array
  type slice
  type elem = Complex.complex
  val length   : slice -> int
  val sub      : slice * int -> elem
  val update   : slice * int * elem -> unit
  val full     : array -> slice
  val slice    : array * int * int option -> slice
  val subslice : slice * int * int option -> slice
  val appi     : (int * elem -> unit) -> slice -> unit
  val app      : (elem -> unit) -> slice -> unit
end

signature COMPLEX_ARRAY2_SLICE = sig
  type slice
  type array_slice
  type elem = Complex.complex
  val dimensions : slice -> int * int
  val transpose  : slice -> slice
  val sub        : slice * int * int -> elem
  val update     : slice * int * int * elem -> unit
  val tabulate   : array_slice * int * int * (int * int -> int) -> slice
  val appi       : (int * int * elem -> unit) -> slice -> unit
  val app        : (elem -> unit) -> slice -> unit
end
*)
local

  fun iter i s f =
      if i >= s then ()
      else (f i; iter (i+1) s f)

in

structure ComplexArray :> COMPLEX_ARRAY = struct
  structure RA = RealArray
  structure C = Complex
  type array = RA.array
  type elem = Complex.complex

  val rem = Int.rem and quot = Int.quot
  infix 7 rem quot
  fun un e = (C.re e, C.im e)

  fun tabulate (n,f) =
      let val v = Vector.tabulate(n,f)
      in RA.tabulate(2*n, fn i =>
                             let val c = Vector.sub(v,i quot 2)
                             in if i rem 2 = 0 then C.re c
                                else C.im c
                             end)
      end

  fun length a = RA.length a quot 2

  fun sub (a,i) =
      let val r = RA.sub(a,2*i)
          val j = RA.sub(a,2*i+1)
      in C.mk(r,j)
      end

  fun update (a,i,c) =
      let val (r,j) = un c
      in RA.update(a,2*i,r)
       ; RA.update(a,2*i+1,j)
      end

  fun appi f a =
      iter 0 (length a) (fn i => f (i,sub(a,i)))

  fun app f a =
      iter 0 (length a) (fn i => f (sub(a,i)))
end

(*
structure ComplexArraySlice :> COMPLEX_ARRAY_SLICE where type array = ComplexArray.array = struct
  structure CA = ComplexArray
  structure C = Complex
  type elem = C.complex
  type array = CA.array
  type slice = array * int * int option

  fun full a = (a,0,NONE)

  fun length (a,b,NONE) = CA.length a - b
    | length (a,b,SOME l) = l

  fun check (a,b,SOME l) = if l >= 0 andalso l <= CA.length a - b then ()
                           else raise Subscript
    | check (a,b,NONE) = if b < CA.length a then ()
                         else raise Subscript

  fun sub ((a,b,_),i) = CA.sub(a,i+b)

  fun update ((a,b,_),i,v) = CA.update(a,i+b,v)

  fun slice (a,b,opt) : slice =
      ( check (a,b,opt)
      ; (a,b,opt)
      )

  fun subslice ((a,b,NONE),b',opt) : slice =
      let val s = (a,b+b',opt)
      in check s
       ; s
      end
    | subslice ((a,b,SOME l),b',opt) : slice =
      let val s = (a,b+b',opt)
          val () = case opt of
                       SOME l' =>
                       if l' > l - b' orelse b' > l then raise Subscript
                       else ()
                     | NONE => if b' > l then raise Subscript
                               else ()
      in check s
       ; s
      end

  fun appi f (s as (a,b,opt)) =
      iter 0 (length s) (fn i => f(i,CA.sub(a,b+i)))

  fun app f (s as (a,b,opt)) =
      iter b (b+length s) (fn i => f(CA.sub(a,i)))
end


structure ComplexArray2Slice :> COMPLEX_ARRAY2_SLICE = struct

structure CAS = ComplexArraySlice
structure CA = ComplexArray
structure C = Complex

type elem = C.complex

type array_slice = CAS.slice

type slice = array_slice * int * int * (int * int -> int)

fun check (r,c,i,j) =
    if i < 0 orelse j < 0 orelse i >= r orelse j >= c then raise Subscript
    else ()

fun dimensions (_,r,c,_) = (r,c)

fun transpose (s,r,c,f) = (s,c,r,fn (i,j) => f (j,i))

fun tabulate (s,r,c,f) =
    if r < 0 orelse c < 0 orelse r*c > CAS.length s then raise Subscript
    else (s,r,c,f)

fun sub ((s,r,c,f),i,j) =
    ( check (r,c,i,j)
    ; CAS.sub(s,f(i,j))
    )

fun update ((s,r,c,f),i,j,v) =
    ( check (r,c,i,j)
    ; CAS.update(s,f(i,j),v)
    )

fun app f (s,r,c,g) =
    iter 0 r (fn i => iter 0 c (fn j => f(CAS.sub(s,g(i,j)))))

fun appi f (s,r,c,g) =
    iter 0 r (fn i => iter 0 c (fn j => f(i,j,CAS.sub(s,g(i,j)))))


end
*)
end
