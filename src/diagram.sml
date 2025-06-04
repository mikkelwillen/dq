
signature DIAGRAM =
sig
  type t
  val box      : string -> t         (* dim 1 *)
  val line     : t                   (* dim 1 *)
  val cntrl    : string -> t         (* dim 2 *)
  val cntrln   : int -> string -> t  (* dim n+1 *)
  val swap     : t                   (* dim 2 *)
  val par      : t * t -> t          (* dim(seq(a,b)) = max(dim a, dim b) *)
  val seq      : t * t -> t          (* dim(par(a,b)) = dim a + dim b *)
  val rep      : int * int * t -> t
  val toString : t -> string
end

structure Diagram :> DIAGRAM =
struct
  type t = string list  (* lines; invariant: lines have equal size *)

  (* Set to true for compact representation of circuits *)
  val compact_p = false

  fun spaces i = CharVector.tabulate(i, fn _ => #" ")

  fun mapi2 f p =
      #2(ListPair.foldr (fn (a,b,(i,r)) => (i+1,f(i,a,b)::r)) (0,nil) p)

  fun mapi f x =
      #2(foldr (fn (a,(i,r)) => (i+1,f(i,a)::r)) (0,nil) x)

  fun width nil = 0
    | width (x::_) = size x

  fun cntrln_compact n s =
      if n <= 0 then [s]
      else ["*","|"] @ cntrln_compact (n-1) s

  fun cntrln n s =
      let fun f first n =
              if n <= 0 then [".-+-.","| " ^ s ^ " |", "'---'"]
              else [if first then "     " else "  |  ",
                    "--*--",
                    "  |  ",
                    "  |  "] @ f false (n-1)
      in f true n
      end

  val {box    : string -> t,        (* gate *)
       line   : t,                  (* line *)
       cntrl  : string -> t,        (* control-circuit *)
       cntrln : int -> string -> t, (* controln-circuit *)
       swap   : t,                  (* swap two qubits *)
       sep    : int -> string       (* index-variant separator *)
      } =
      if compact_p then
        {box=fn s => [s],
         line=["-"],
         cntrl=cntrln_compact 1,
         cntrln=cntrln_compact,
         swap=[". .", " X ", "' '"],
         sep=fn i => if i mod 2 = 0 then "-" else " "}
      else
        {box=fn s => [".---.","| " ^ s ^ " |", "'---'"],
         line=["     ","-----", "     "],
         cntrl=cntrln 1,
         cntrln=cntrln,
         swap=["     ", ".   .",
               " \\ / ", "  /  ", " / \\ ",
               "'   '", "     "],
         sep=fn i => if (i-1) mod 4 = 0 then "-" else " "}

  (* index-variant padding *)
  fun padi w (i,s) =
      if size s >= w then s
      else if size s = w-1 then s ^ sep i
      else padi w (i,sep i ^ s ^ sep i)

  fun par (a:t,b:t) : t =
      let val i = width a
          val j = width b
          val w = Int.max(i,j)
      in mapi (padi w) a @ [spaces w] @
         mapi (padi w) b
      end

  fun seq (a:t,b:t) : t = mapi2 (fn (i,a,b) => a ^ sep i ^ b) (a,b)

  fun vline 1 = ["|  ", "|--", "|  ", "|  "]
    | vline h =
      let val l = ["|  ", "|--", "|  ", "|  "]
      in l @ vline (h-1)
      end

  fun rline 1 = ["|", "|", "|", "|"]
    | rline h =
      let val l = ["|", "|", "|", "|"]
      in rline (h-1) @ l
      end

  fun lvline (h: int, n: int) : t =
      let val ml = ["|  ", "|" ^ Int.toString n ^ "x", "|  ", "|  "]
          val l = ["|  ", "|--", "|  ", "|  "]
      in if ((h mod 2) = 0) then
            vline ((h-1) div 2) @ ml @ l @ vline ((h-1) div 2)
         else
            vline (h div 2) @ ml @ vline (h div 2)
      end

  fun rep (n: int, h: int, a: t) : t =
      let val l = lvline (h, n)
          val rl = rline h
      in
        let val ll = mapi2 (fn (i, l, a) => l ^ sep i ^ a) (l, a)
        in mapi2 (fn (i, ll, rl) => ll ^ sep i ^ rl) (ll, rl)
        end
      end

  fun toString (a:t) : string =
      let val a = mapi (padi (width a + 4)) a
      in String.concatWith "\n" a
      end
end
