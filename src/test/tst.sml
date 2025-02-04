
open Circuit Semantics
infix 3 oo
infix 4 **

fun id 1 = I
  | id n = I ** id (n-1)

fun swap1 k 1 = if k = 2 then SW else SW ** id (k-2)
  | swap1 k n = I ** swap1 (k-1) (n-1)

fun swap k 1 = swap1 k 1
  | swap k n = swap k (n-1) oo swap1 k n

val d = C X ** swap 4 3 ** H oo Z ** H ** C X ** Y ** Z oo H ** swap 6 3

val () = print (draw d ^ "\n")
