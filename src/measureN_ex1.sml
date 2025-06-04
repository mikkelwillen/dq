open Circuit Semantics
infix 3 ++
infix 4 **

fun run c k =
     (print ("Circuit for c = " ^ pp c ^ ":\n");
     let val v0 = init k
         val v1 = eval c v0
         val (bit, newState) = measureNQubits v1 2 10
     in print ("state1: \n" ^ pp_state v1 ^ "\n")
       ; print ("state2: \n" ^ pp_state newState ^ "\n")
       ; print (pp_dist (measure_dist newState) ^ "\n\n")
     end)

val () = run ((H ** H ** H ** H) ++ Repeat(2, T ** Z ** T ** I)) (ket[1,0,1,0])
