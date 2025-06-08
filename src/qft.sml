(* Quantum Fourier Transform (QFT) implementation in Standard ML *)

(* Based on: https://medium.com/@marcell.ujlaki/exploring-quantum-computing-demystifying-quantum-fourier-transformations-unveiling-the-math-with-5d74f3f8025f
*)

open Circuit Semantics
infix 3 ++
infix 4 **

fun die s = raise Fail ("QFT: " ^ s)

fun pow2 n = if n = 0 then 1 else 2 * pow2 (n - 1)

fun repeatV n g =
		if n <= 0 then
			die "repeatV"
		else if n = 1 then
			g
		else
			g ** repeatV (n - 1) g

(* Wraps the circuit `c` in I *)
(* i = prepend `i` numbers of I  to `c`*)
(* j = append `j` numbers of I to `c` *)
fun wrap i c j =
		if i < 0 orelse j < 0 then
			die "wrap"
		else if i = 0 andalso j = 0 then
			c
		else if i = 0 then
			c ** repeatV j I
		else if j = 0 then
			repeatV i I ** c
		else
			repeatV i I ** c ** repeatV j I

(* Swap qubits a and b in a k-qubit circuit *)
fun swap k i =
		wrap (i - 1) SW (k - i - 1)

(* Swap qubits a and b in a k-qubit by calling swap from a to b *)
fun swapN k a b =
		let
			fun recSwap acc up =
			if acc = a then
				swap k acc
			else if acc = b - 1 then
				swap k acc ++ recSwap (acc - 1) false
			else if up then
				swap k acc ++ recSwap (acc + 1) up
			else
				swap k acc ++ recSwap (acc - 1) up
		in
			if a = b - 1 then
				swap k a
			else
				swap k a ++ recSwap (a + 1) true
		end

(* Controlled gate with n control qubits *)
fun cntrl n g =
	if n <= 0 then
		g
	else
		C (cntrl (n - 1) g)

(* Encode classical integer n on k qubits using X gates *)
fun encode_num k n =
		let
			fun enc i acc =
					if i >= k then
						acc
					else
						let
							val bit = (n div pow2 i) mod 2
							val gate = if bit = 1 then X else I
						in
							enc (i + 1) (gate ** acc)
						end
	in
		let
			val bit = n mod 2
			val gate = if bit = 1 then X else I
		in
			enc 1 gate
		end
	end

(* QFT core rotations *)
fun qft_rots k =
	let
		fun apply i acc =
				if i >= k then
					acc
				else
					let
						val had = wrap i H (k - i - 1)

						fun cph j acc2 =
								if j <= i then
									acc2
								else
									let
										val theta = Math.pi / Real.fromInt (pow2 (j - i))
										val rz = RZ(theta)
										val ctrl = wrap i (cntrl (j - i) rz) (k - j - 1)
									in
										cph (j - 1) (acc2 ++ ctrl)
									end
					in
						apply (i + 1) (acc ++ (cph (k - 1) had))
					end
	in
		let
			val i = repeatV k I
		in
			apply 0 i
		end
	end

(* Swaps to reverse qubit order *)
fun qft_swaps k =
	let
		fun flip i acc =
				if i >= k div 2 then
					acc
				else
					let
						val s = swapN k (i + 1) (k - i) ++ acc
					in
						flip (i + 1) s
					end
	in
		let
			val s = swapN k 1 k
		in
			flip 1 s
		end
	end

(* Full QFT circuit on k qubits with classical input n *)
fun qft_circuit k n =
	let
		val init = encode_num k n
		val qft = qft_rots k ++ qft_swaps k
	in
		init ++ qft
	end

(* Runner *)
fun run_qft k n =
	let
		val circ = qft_circuit k n
		val st = ket (List.tabulate (height circ, fn _ => 0))
		val () = print ("Running QFT on " ^ Int.toString k ^ " qubits with input " ^ Int.toString n ^ "\n")
		val () = print ("Circuit height: " ^ Int.toString (height circ) ^ "\n")
		val () = print ("Drawing circuit:\n" ^ draw circ ^ "\n")
		val v = eval circ (init st)
		val d = measure_dist v
	in
		print ("Full distribution:\n" ^ pp_dist d ^ "\n")
	end

(* Example: QFT with 4 qubits, encoding value 12 *)
val () = run_qft 4 12
