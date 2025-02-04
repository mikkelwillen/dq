(* Grover's algorithm *)

val () = print "Grover's algorithm\n"

open Circuit Semantics
infix 3 ++
infix 4 **

fun die s = raise Fail ("Grover: " ^ s)

(* Utilities *)

fun pow2 n = if n=0 then 1 else 2 * pow2 (n-1)

fun pow2ii (n:int) : IntInf.int = if n=0 then 1 else 2 * pow2ii (n-1)

fun repeatH n (g:t) : t =
    if n <= 0 then I
    else if n = 1 then g
    else g ++ repeatH (n-1) g

fun repeatV n (g:t) : t =
    if n <= 0 then die "repeatV"
    else if n = 1 then g
    else g ** repeatV (n-1) g

fun swapN n : t =           (* returns a circuit of height n+1 *)
    if n = 0 then I
    else if n = 1 then SW
    else repeatV (n-2) I ** SW ++ swapN (n-1) ** I

fun swap a b =              (* assumes b >= a, returns a circuit of height b+1 *)
    if a > b then die "swap"
    else if a = b then repeatV b I
    else if a = 0 then swapN b
    else repeatV a I ** swapN (b-a)

fun cntrl n g =             (* returns a circuit of height n + 1 *)
    if n <= 0 then g
    else C (cntrl (n-1) g)

val toffoli = cntrl 2 X     (* circuit of height 3 *)

fun grover_diff n : t =     (* returns a circuit of height n *)
    repeatV n H ++
    repeatV n X ++
    repeatV (n-1) I ** H ++
    cntrl (n-1) X ++
    repeatV (n-1) I ** H ++
    repeatV n X ++
    repeatV n H

(* Two-qubit search for |11> *)

val grover2 =
    let val init = repeatV 2 H
        val oracle = C Z
        val diff = repeatV 2 (H ++ Z) ++ C Z ++ repeatV 2 H
    in init ++
       oracle ++
       diff
    end

fun gates c =
    case c of
        Tensor(c1,c2) => gates c1 + gates c2
      | Seq(c1,c2) => gates c1 + gates c2
      | C c => 1 + gates c
      | SW => 0
      | I => 0
      | _ => 1

fun findBest (d:dist) : ket * real =
    Vector.foldl (fn ((k,r),(k',a)) => if r > a then (k,r) else (k',a)) (ket [0],0.0) d

fun pp_bytes (n:IntInf.int) =
    if n < 3000 then IntInf.toString n ^ " bytes"
    else if n < 3000000 then IntInf.toString (n div 1024) ^ " Kb"
    else if n < 1000 * 3000000 then IntInf.toString ((n div 1024) div 1024) ^ " Mb"
    else IntInf.toString (((n div 1024) div 1024) div 1024) ^ " Gb"

fun run ev p c =
    let val k = ket (List.tabulate (height c, fn _ => 0))
        val vsz_bytes = pow2ii (height c + 4)
    in print ("Simulating circuit with " ^ Int.toString (height c) ^ " qubits (vector size: " ^ pp_bytes vsz_bytes ^ ")\n")
     ; print ("Gate count: " ^ Int.toString (gates c) ^ "\n")
     ; if p then print ("Diagram for c = " ^ pp c ^ " :\n" ^ draw c ^ "\n") else ()
     ; print ("Evaluating circuit...\n")
     ; let val t = Timer.startRealTimer()
           val v0 = init k
           val v1 = ev c v0
           val t = Timer.checkRealTimer t
           val () = print ("Simulation time: " ^ Time.toString t ^ "s\n")
           val d = measure_dist v1
           val (k,r) = findBest d
       in print ("Found ket: " ^ pp_ket k ^ ":" ^ Real.toString r ^ "\n")
        ; (if p then
             print ("Result distribution:\n" ^ pp_dist d ^ "\n\n")
           else ())
       end
    end

(*
val () = run false (repeatV 24 H)
val () = raise Fail "DONE"
*)

val () = run Execute2.exec true grover2

(* 3-qubit search for |101> and |110> *)

val grover3 =
    let val init = repeatV 3 H
        val oracle = I ** SW ++ C Z ** I ++ I ** SW ++ I ** C Z
        val diff = grover_diff 3
    in init ++
       oracle ++
       diff
    end

val () = run Execute2.exec true grover3

(* |000> : 0 (X**X**X) *)
(* |001> : 1 (X**X**I) *)
(* |010> : 2 (X**I**X) *)
(* |011> : 3 (X**I**I) *)

fun tenOpt c NONE = c
  | tenOpt c (SOME c') = c**c'

fun encodeNum n i : t option =
    let fun enc n i (a:t option) : t option =
            if n = 0 then
              if i = 0 then a
              else die "encodeNum"
            else enc (n-1) (i div 2)
                     (SOME(if i mod 2 = 0
                           then tenOpt X a
                           else tenOpt I a))
    in enc n i NONE
    end

fun oracle n i : t =        (* returns circuit of height n *)
    case encodeNum n i of
        SOME c => c ++ cntrl (n-1) Z ++ c
      | NONE => die "oracle"

fun iterH n c =
    if n <= 0 then die "iterH"
    else if n = 1 then c
    else c ++ iterH (n-1) c

fun groverN n i =
    let val m = 1  (* m: number of candidates, n: number of qubits, i: candidate integer *)
        val k = Real.ceil (Math.sqrt(real (pow2 n) / real m) * Math.pi / 4.0)
    in repeatV n H ++
       iterH k (oracle n i ++ grover_diff n)
    end

val () = run Execute2.exec true (groverN 4 6)

val () = run Execute2.exec false (groverN 13 12)
val () = run Execute.exec false (groverN 13 12)

(*
val () = print (pp_mat (sem (X ** I)) ^ "\n")

val () = run (C X) (ket[0,0])

val () = run (C X) (ket[0,1])

val () = run (C X) (ket[1,1])

val () = run (C X ** I) (ket[0,0,1])
*)


datatype kind = EVAL | INTERP | EXEC | EXEC2
type conf = kind * int

fun pp_kind k =
    case k of EVAL => "EVAL"
            | INTERP => "INTERP"
            | EXEC => "EXEC"
            | EXEC2 => "EXEC2"

fun pp_conf (kind,qubits) =
    pp_kind kind ^ " " ^ Int.toString qubits

fun measure (conf as (kind,qubits)) =
    ( let val c = groverN qubits 5
          val k = ket (List.tabulate (height c, fn _ => 0))
          val p = false
          val p2 = false
          val count = gates c
          val f = case kind of EVAL => eval | INTERP => interp | EXEC => Execute.exec | EXEC2 => Execute2.exec
      in if p then print ("Diagram for c = " ^ pp c ^ " :\n" ^ draw c ^ "\n") else ()
       ; (if p2 then
            ( print ("Conf: " ^ pp_conf conf ^ "\n")
            ; print ("Built Grover Circuit for " ^ Int.toString qubits ^ " qubits\n")
            ; print ("Gate count: " ^ Int.toString count ^ "\n")
            ; print ("Evaluating circuit...\n")
            )
          else ())
       ; let val t = Timer.startRealTimer()
             val v = f c (init k)
             val t = Timer.checkRealTimer t
         in (if p orelse p2 then
               let val d = measure_dist v
                   val (k,r) = findBest d
               in (if p2 then
                     ( print ("Simulation time: " ^ Time.toString t ^ "s\n")
                     ; print ("Found ket: " ^ pp_ket k ^ ":" ^ Real.toString r ^ "\n")
                     )
                   else ())
                ; (if p then
                     print ("Result distribution:\n" ^ pp_dist d ^ "\n\n")
                   else ())
               end
             else ())
          ; print (pp_conf conf ^ " [gates=" ^ Int.toString count ^ "] : " ^ Time.toString t ^ "s\n")
          ; t
         end
      end)

fun iter n f =
    if n = 0 then nil
    else f()::iter (n-1) f

fun sum rs = List.foldl (op +) 0.0 rs
fun mean rs = sum rs / real (List.length rs)

fun mean_stddev rs =
    let val m = mean rs
        fun sq x = x * x
        val sqs = map (fn r => sq(m - r)) rs
        val s = Math.sqrt(sum sqs / real (List.length sqs - 1))
    in (m, s)
    end

fun r2s r =
    Real.fmt (StringCvt.FIX (SOME 2)) r

fun measures r k ns =
    List.app (fn n =>
                 let val ts = iter r (fn() => measure (k,n))
                     val ts = map Time.toReal ts
                     val (m,s) = mean_stddev ts
                     val sp = s * 100.0 / m
                 in print ("TOTAL " ^ pp_conf(k,n) ^ " : " ^ r2s m ^ "s +- " ^ r2s s ^ " (" ^ r2s sp ^ "%)\n")
                 end) ns

val K = 5

val () = measures 2 INTERP [13]
val () = measures K INTERP [14]

(*
val () = measures K EXEC2 [5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]

val () = measures K EVAL [5,6,7,8]

val () = measures K INTERP [5,6,7,8,9,10,11,12,13,14,15]

val () = measures K EXEC [5,6,7,8,9,10,11,12,13,14,15,16,17]
*)
