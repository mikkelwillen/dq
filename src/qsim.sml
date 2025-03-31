signature QSIM = sig

  type gate = string
  datatype arg = I of int | R of real
  type inst = int * gate * arg list
  type prg = {qubits: int, insts: inst list}

  val parse : {filename:string} -> string -> prg
  val pp    : prg -> string
  val tofut : {flags:string list} -> prg -> string

end

structure Qsim :> QSIM = struct

  fun die s = raise Fail ("Imp: " ^ s)
  fun println s = print (s ^ "\n")

  type gate = string
  datatype arg = I of int | R of real
  type inst = int * gate * arg list

  type prg = {qubits: int, insts: inst list}

  structure Parser = Parse(CharToken)

  (* Parsing SVSIM files *)
  local

    open Parser

    infixr >>= <|> *> <*
    infix <*> <$> >>>

    fun char c = satisfy (fn c' => c = c')

    val space = many (satisfy (fn c => c = #" "))

    fun lexeme p = p <* space

    fun lChar c = lexeme (char c)

    fun lString s =
        let fun loop [] = accept ()
              | loop (c :: cs) = char c *> loop cs
        in lexeme (loop (explode s))
        end

    fun sepBy1 p sep =
        p >>=
          (fn x =>
              choice
                  [ sep *> delay (sepBy1 p) sep >>= (fn xs => accept (x :: xs))
                  , delay accept [x]
          ])

    fun sepBy p sep =
        choice [delay (sepBy1 p) sep, delay accept []]

    local
      val firstChar = satisfy Char.isAlpha
      val secondChar = satisfy (fn c => Char.isAlphaNum c orelse c = #"_")
    in
      val lName : string p =
          lexeme ((fn c => fn cs =>
                      implode (c :: cs)) <$> firstChar <*> many secondChar)
    end

    local
      val digitCharGen = satisfy (fn c => Char.isDigit c orelse c = #"-" orelse c = #".")
      val digitChar = satisfy Char.isDigit
    in
      val lArg: arg p =
          lexeme (implode <$> some digitCharGen)
                 >>= (fn s => case Int.fromString s of
                                  SOME i => accept (I i)
                                | NONE =>
                                  case Real.fromString s of
                                      SOME r => accept (R r)
                                    | NONE => reject "expecting integer or real")
      val lNum : int p =
          lexeme (implode <$> some digitChar)
                 >>= (fn s => case Int.fromString s of
                                  SOME i => accept i
                                | NONE => reject "expecting integer")
    end

    val lNL = lChar #"\n"

    val lArgs : arg list p =
        sepBy lArg space

    val lInst : inst p =
        (fn ((a,b),c) => (a,b,c)) <$> (lNum >>> lName >>> lArgs)

    val lInsts : inst list p = sepBy lInst lNL

    val lP : prg p =
        (fn (i,is) => {qubits=i,insts=is}) <$> ((lNum <* lNL) >>> lInsts)
  in

  fun parse {filename=f} (s:string) : prg =
      let val ts = CharToken.tokenise {srcname=f, input=s}
      in case Parser.parse ((lP <* (many (satisfy (fn c => c = #" " orelse c = #"\n")))) <* eof) ts of
             OK x => x
           | NO (loc,err) => ( println ("ERR at " ^ Region.ppLoc loc ^ ": " ^ err())
                             ; die "parse_svsim: error" )
      end

  end

  fun pp_arg (I i) = Int.toString i
    | pp_arg (R r) = Real.toString r

  fun pp_inst (i,n,args) =
      String.concatWith " "
                        (Int.toString i :: n ::
                         map pp_arg args)

  fun pp ({qubits,insts}:prg) : string =
      String.concatWith "\n"
                        (Int.toString qubits ::
                         map pp_inst insts)

  local
    open Futhark
    infix >>=

    fun prelude dqfut =
        "import \"" ^ dqfut ^ "\"\n\
        \open mk_gates(f64)\n"

    fun stT n = "*st[" ^ Int.toString n ^ "]"

    fun wrAPP (n,args) (v:var) : var M =
        LetNamed v (APP(n,args))

    fun gate g i s : var M =
        wrAPP ("gate" ^ g, [CONST(Int.toString i),
                            VAR s]) s

    fun swap2 i j s : var M =
        wrAPP ("swap2", [CONST(Int.toString i),
                         CONST(Int.toString j),
                         VAR s]) s

    fun cntrl0 g i s : var M =
        wrAPP ("cntrl" ^ g, [CONST "1",
                             CONST(Int.toString i),
                             VAR s]) s

    fun cntrl g i j s : var M =
        if j=i+1 then
          cntrl0 g i s
        else if i < j then
          swap2 i (j-1) s >>= (fn s =>
          cntrl0 g (j-1) s >>= (fn s =>
          swap2 i (j-1) s))
        else if j > i then
          swap2 j (i+1) s >>= (fn s =>
          cntrl0 g i s >>= (fn s =>
          swap2 j (i+1) s))
        else die ("cntrl: i = j")

    fun mkG n (l,g,is) (s:var) : var M =
        case (g,is) of
            ("x", [I i]) => gate "X" i s
          | ("y", [I i]) => gate "Y" i s
          | ("z", [I i]) => gate "Z" i s
          | ("h", [I i]) => gate "H" i s
          | ("t", [I i]) => gate "T" i s
          | ("s", [I i]) => gate "S" i s
          | ("x_1_2", [I i]) => gate "SX" i s
          | ("y_1_2", [I i]) => gate "SY" i s
          | ("cx", [I i,I j]) => cntrl "X" i j s
          | ("cy", [I i,I j]) => cntrl "Y" i j s
          | ("cz", [I i,I j]) => cntrl "Z" i j s
          | _ => die ("mkG.gate not implemented: " ^ g)

    fun mkF n nil s = ret s
      | mkF n (i::is) s =
        mkG n i s >>= (mkF n is)

    fun ketT n = "[" ^ Int.toString n ^ "]i64"

    fun look f (k::v::rest) =
        if size k > 0 andalso CharVector.sub(k,0) = #"-" andalso k = f then SOME v
        else look f (v::rest)
      | look _ _ = NONE

    fun ison f (k::rest) =
        (size k > 0 andalso CharVector.sub(k,0) = #"-" andalso k = f)
        orelse ison f rest
      | ison _ _ = false

  in
    fun tofut {flags:string list} ({qubits=n,insts}:prg) : string =
        let val dqfut = case look "-dqfut" flags of SOME p => p | _ => "dqfut"
            val t = stT n
            val t1 = ketT n
            val t2 = "(" ^ ketT n ^ ", f64)"
            val m = FunNamed "main" (fn e =>
                       LetNamed "s" (APP("fromKet", [e]))
                       >>= (mkF n insts)
                       >>= (fn v => LetNamed "d" (APP("dist",[VAR v]))
                       >>= (fn d => (ret (APP("distmax", [VAR d]))))))
                    t1 t2 >>= (fn _ => ret ())
        in prelude dqfut ^ "\n" ^ runBinds m
        end
  end

end
