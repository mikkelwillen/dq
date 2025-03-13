
fun println s = print (s ^ "\n")

fun readFile f =
    let val is = TextIO.openIn f
    in let val s = TextIO.inputAll is
       in  TextIO.closeIn is
         ; s
       end handle X => (TextIO.closeIn is; raise X)
    end

fun ison f (k::rest) =
    (size k > 0 andalso CharVector.sub(k,0) = #"-" andalso k = f)
    orelse ison f rest
  | ison _ _ = false

fun isflag k =
    size k > 1 andalso CharVector.sub(k,0) = #"-"

fun usage () =
    ( println ("Usage: " ^ CommandLine.name() ^ " [OPTIONS...] filename")
    ; println ("OPTIONS:")
    ; println (" -s       : write statistics and exit")
    ; println (" -dqfut p : specify path to dqfut.fut (leave out .fut)")
    ; println (" -h       : write help and exit")
    )

exception EXIT

val () =
    (case rev(CommandLine.arguments ()) of
         f :: rflags =>
         let val flags = rev rflags
             val () = if isflag f then raise Fail "isflag" else ()
             val s =
                 readFile f
                 handle X => (println ("Failed to read file: " ^ f); raise X)
             val p as {qubits,insts} =
                 Qsim.parse {filename=f} s
                 handle X => (println ("Failed to parse file: " ^ f); raise X)
             val () =
                 if ison "-s" flags
                 then ( println ("Parsed program with " ^ Int.toString qubits
                                 ^ " qubits.., " ^ Int.toString (length insts) ^ " gates..")
                      ; raise EXIT)
                 else ()
             val fut =
                 Qsim.tofut {flags=flags} p
                 handle X => (println ("Failed to generate Futhark code"); raise X)
         in println ("-- Parsed program: " ^ f)
          ; println ("-- Qubits: " ^ Int.toString qubits)
          ; println ("-- Gate operations: " ^ Int.toString (length insts))
          ; println ("--")
          ; println fut
         end
       | _ => usage()
    ) handle EXIT => ()
           | _ => usage()
