open Circuit Semantics
infix 3 ++
infix 4 **

fun run c k =
     (print ("Circuit for c = " ^ pp c ^ ":\n");
      print ("Drawing circuit:\n" ^ draw c ^ "\n");
     let val v0 = init k
         val v1 = eval c v0
         val (bit, newState) = measureQubit v1 0
     in print ("state1: \n" ^ pp_state v1 ^ "\n")
       ; print ("state2: \n" ^ pp_state newState ^ "\n")
       ; print (pp_dist (measure_dist newState) ^ "\n\n")
     end)

val () = run ((H ** I) ++ (C X))  (ket[0,0])
