
open Circuit Semantics Execute2
infix 3 ++
infix 4 **

fun comment s =
    print("-- " ^
          String.translate (fn #"\n" => "\n-- " | c => String.str c) s
          ^ "\n")

fun iter b e f =
    if b >= e then ()
    else (f b ; iter (b+1) e f)

fun test c =
    ( comment ("Circuit for c = " ^ pp c ^ " (height=" ^ Int.toString (height c) ^ ") :")
    ; comment (draw c)
    ; let val v = init(ket(List.tabulate(height c, fn _ => 0)))
          val d = measure_dist (interp c v)
          val () = print ("Distribution (inter):\n" ^ pp_dist d ^ "\n")
          val d' = measure_dist (exec c v)
      in print ("Distribution (exec):\n" ^ pp_dist d' ^ "\n")
      end
    )

val () = test (X ** I ++ C X)

val () = test (X ** X ** I ++ C (C X))

val () = test (X ** X ** X ** I ++ C (C (C X)))
