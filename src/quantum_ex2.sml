
open Circuit Semantics
infix 3 oo
infix 4 **

fun run c k =
    (print ("Circuit for c = " ^ pp c ^ ":\n");
     print (draw c ^ "\n");
     print ("Computing semantics...\n");
     let val m = sem c
      in print ("Semantics of c:\n");
         print (pp_mat m ^ "\n")
      end;
     print ("Result distribution when evaluating c on " ^ pp_ket k ^ " :\n");
     let val v0 = init k
         val v1 = eval c v0
     in print (pp_dist(measure_dist v1) ^ "\n\n")
      ; print ("V1: " ^ pp_state v1 ^ "\n")
      ; print ("V2: " ^ pp_state (interp c v0) ^ "\n")
     end)

val () = run (I ** I ** H ** Z oo SW ** SW oo C Z ** Y ** Z oo H ** SW ** X oo I
                ** X ** Y ** Z oo SW ** SW oo C X ** C Y)
             (ket[1,0,1,1])
