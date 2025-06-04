open Circuit Semantics
infix 3 ++
infix 4 **

fun run c k =
     (print ("Circuit for c = " ^ pp c ^ ":\n");
     let val v0 = init k
         val v1 = eval c v0
         val (bit, newState) = measureQubit v1 2
         val (bit2, newState2) = measureQubit newState 2
     in print ("State after first measurement:\n" ^ pp_dist (measure_dist newState) ^ "\n\n")
       ; print ("State after second measurement:\n" ^ pp_dist (measure_dist newState2) ^ "\n\n")
     end)

val () = run ((H ** H ** H ** H) ++ Repeat(2, T ** Z ** T ** I)) (ket[1,0,1,0])
