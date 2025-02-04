
signature PULL_ARRAY = sig
  type parray
  type elem
  type array
  val full     : array -> parray
  val tabulate : array * int * (int -> int) -> parray
  val length   : parray -> int
  val sub      : parray * int -> elem
  val update   : parray * int * elem -> unit
  val appi     : (int * elem -> unit) -> parray -> unit
  val app      : (elem -> unit) -> parray -> unit
  val take     : int * parray -> parray
  val drop     : int * parray -> parray
end

signature PULL_ARRAY2 = sig
  type parray2
  type parray
  type elem
  type array
  val dimensions : parray2 -> int * int
  val transpose  : parray2 -> parray2
  val sub        : parray2 * int * int -> elem
  val update     : parray2 * int * int * elem -> unit
  val appi       : (int * int * elem -> unit) -> parray2 -> unit
  val app        : (elem -> unit) -> parray2 -> unit
  val unflatten  : int * int * parray -> parray2
  val flatten    : parray2 -> parray
  val vec        : parray2 -> parray
  val unvec      : int * int * parray -> parray2
  val appRows    : (parray -> unit) -> parray2 -> unit
end

local

  fun iter i s f =
      if i >= s then ()
      else (f i; iter (i+1) s f)

in

structure ComplexPullArray : PULL_ARRAY =
struct
  structure C = Complex
  structure CA = ComplexArray
  type array = CA.array
  type elem = Complex.complex
  type parray = array * int * (int -> int)

  fun length (_,l,_) = l

  fun full a = (a,CA.length a, fn i => i)

  fun tabulate (a,l,f) = (a,l,f)

  val safe = false

  fun check i l =
      if safe andalso (i < 0 orelse i >= l) then raise Subscript
      else ()

  fun sub ((a,l,f),i) =
      ( check i l
      ; CA.sub(a,f i) )

  fun update ((a,l,f),i,v) =
      ( check i l
      ; CA.update(a,f i,v) )

  fun appi g (a,l,f) =
      iter 0 l (fn i => g(i,CA.sub(a,f i)))

  fun app g (a,l,f) =
      iter 0 l (fn i => g(CA.sub(a,f i)))

  fun take (n,(a,l,f)) =
      if safe andalso (n > l orelse n < 0) then raise Subscript
      else (a,n,f)

  fun drop (n,(a,l,f)) =
      if safe andalso (n > l orelse n < 0) then raise Subscript
      else (a,l-n,fn i => f(i+n))
end

structure ComplexPullArray2 : PULL_ARRAY2 =
struct
  structure C = Complex
  structure CA = ComplexArray
  type array = CA.array
  type elem = Complex.complex
  type parray = ComplexPullArray.parray
  type parray2 = array * int * int * (int * int -> int)

  val rem = Int.rem and quot = Int.quot
  infix 7 rem quot

  val safe = false

  fun dimensions (_,r,c,_) = (r,c)

  fun transpose (a,r,c,f) = (a,c,r,fn (i,j) => f (j,i))

  fun check (r,c,i,j) =
      if safe andalso (i < 0 orelse j < 0 orelse i >= r orelse j >= c) then raise Subscript
      else ()

  fun sub ((a,r,c,f),i,j) =
      ( check (r,c,i,j)
      ; CA.sub(a,f(i,j)) )

  fun update ((a,r,c,f),i,j,v) =
      ( check (r,c,i,j)
      ; CA.update(a,f(i,j),v) )

  fun appi g (a,r,c,f) =
      iter 0 r (fn i => iter 0 c (fn j => g(i,j,CA.sub(a,f(i,j)))))

  fun app g (a,r,c,f) =
      iter 0 r (fn i => iter 0 c (fn j => g(CA.sub(a,f(i,j)))))

  fun unflatten (r,c,(a,n,f)) =
      if safe andalso r*c <> n then raise Size
      else (a,r,c,fn(i,j)=>f(i*c+j))

  fun flatten (a,r,c,f) = (a,r*c,fn i => f(i quot c,i rem c))

  fun vec m = flatten (transpose m)

  fun unvec (r,c,v) = transpose(unflatten (r,c,v))

  fun appRows g (a,r,c,f) =
      let val m = Array.array(c,~1)
          fun reset () = Array.modify (fn _ => ~1) m
          fun mem f =
              ( reset ()
              ; fn k =>
                   let val x = Array.sub(m,k)
                   in if x >= 0 then x
                      else let val y = f k
                           in Array.update(m,k,y)
                            ; y
                           end
                   end
              )
      in iter 0 r (fn i => g (a,c,mem (fn j => f(i,j))))
      end
end

end

local structure Both :> sig
         structure ComplexPullArray : PULL_ARRAY where type elem = Complex.complex
                                                 where type array = ComplexArray.array
         structure ComplexPullArray2 : PULL_ARRAY2 where type elem = Complex.complex
                                                   where type array = ComplexArray.array
         sharing type ComplexPullArray.parray = ComplexPullArray2.parray
      end = struct
        structure ComplexPullArray = ComplexPullArray
        structure ComplexPullArray2 = ComplexPullArray2
      end
in
open Both
end
