
open Circuit CompImp
infix 3 ++
infix 4 **

fun comment s =
    print("-- " ^
          String.translate (fn #"\n" => "\n-- " | c => String.str c) s
          ^ "\n")

fun test c =
    ( comment ("Circuit for c = " ^ pp c ^ ":")
    ; comment (draw c)
    ; comment ("Futhark simulation function for c:\n")
    ; print ("import \"qsim\"\nopen gates\n\n")
    ; print (circuitToFutFunBind "sim" c ^ "\n\n")
    )

val () = test ((I ** H ++ C X ++ Z ** Z ++ C X ++ I ** H) ** I ++ I ** SW ++ C X ** Y)
