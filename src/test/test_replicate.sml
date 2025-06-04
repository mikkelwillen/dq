open Circuit Semantics
infix 3 ++
infix 4 **

fun run (t: t) (k: ket) : string =
	let val v0 = init k
	  	val v1 = eval t v0
	in pp_state v1
	end

fun iterH (n: int) (t: t) : t =
	if n = 1 then t
	else t ++ iterH (n - 1) t

fun iterV(n: int) (t: t) : t =
	if n = 1 then t
	else t ** iterV (n - 1) t

val c1 = (H ** H ** H) ++ Replicate(1, T ** T ** T)
val c1' = (H ** H ** H) ++ (T ** T ** T)

val c2 = (H ** H ** H) ++ Replicate(5, T ** T ** T)
val c2' = (H ** H ** H) ++ iterH 5 (T ** T ** T)

val c3 = (H ** H ** H) ++ Replicate(8, (T ** T ** T) ++ (H ** H ** H))
val c3' = (H ** H ** H) ++ iterH 8 ((T ** T ** T) ++ (H ** H ** H))

val c4 = (iterV 6 H) ++ Replicate(5, iterV 6 T)
val c4' = (iterV 6 H) ++ iterH 5 (iterV 6 T)

val ket3 = (ket[0, 1, 0])
val ket6 = (ket[1, 0, 1, 0, 1, 0])

val () = print (if (run c1 ket3) = (run c1' ket3) then "ok" ^ "\n" else "fail" ^ "\n")
val	() = print (if (run c2 ket3) = (run c2' ket3) then "ok" ^ "\n" else "fail" ^ "\n")
val () = print (if (run c3 ket3) = (run c3' ket3) then "ok" ^ "\n" else "fail" ^ "\n")
val () = print (if (run c4 ket6) = (run c4' ket6) then "ok" ^ "\n" else "fail" ^ "\n")
val () = print ("V1: " ^ (run c3 ket3) ^ "\n")
val () = print ("V2: " ^ (run c3' ket3) ^ "\n")
