(*
  Layout del frame real. Los primeros 6 argumentos son pasados por registros

    |    argn    |  
    |    ...     |
    |    arg8    |  fp+24
    |    arg7    |  fp+16
    |  retorno   |  fp+8
    |   rbp ant  |  fp
    --------------  fp
    | static link|  fp-8
    |    arg7    |  fp-16
    |    arg8    |  fp-24
    |    ...     |  
    |   local1   |
*)

structure tigerframe :> tigerframe = struct

open tigertree
open tigerassem
open tigertemp

val fp = "rbp"          (* frame pointer *)
val sp = "rsp"          (* stack pointer *)
val rv = "rax"          (* return value  *)
val rdi ="rdi"
val rsi = "rsi"
val rdx = "rdx"
val rcx = "rcx"
val r8 = "r8"
val r9 = "r9"
val wSz = 8             (* word size in bytes *)
val log2WSz = 3         (* base two logarithm of word size in bytes *)
val fpPrev = 0          (* offset (bytes) *)
val offStaticLink = ~8  (* offset (bytes) *)
val offArgs = 16        (* cuanto arriba del fp estan los argumentos pasados por pila*)

val argsInicial = 0       (* el primer argumento *)
val argsOffInicial = ~16  (* words *)
val argsGap = wSz         (* bytes *)

val regInicial = 1        (* reg *)
val localsInicial = 0     (* la primera variable local *)
val localsGap = 8         (* bytes *)

val calldefs = [rv]
val specialregs = [rv, fp, sp]
val argregs = []
val callersaves = [rv,rcx,rdx,rsi,rdi,r8,r9,"r10", "r11"] 
val calleesaves = ["rbx", fp, sp, "r12", "r13", "r14", "r15"]
val calleesaves' = ["rbx", "r12", "r13", "r14", "r15"]
val registers = [rv,"rbx",rcx,rdx,rsi,rdi,fp,sp,r8,r9,"r10","r11","r12","r13","r14","r15"]
val registersSet = Splayset.addList (Splayset.empty String.compare, registers) 
val K = length registers

val MAX_ARGS = 15
val MAX_ARGS_REG = 6
val MAX_ARGS_STACK = MAX_ARGS - MAX_ARGS_REG

fun its n =  if n<0 then "-" ^ Int.toString(~n) else Int.toString(n) 

datatype access = InFrame of int | InReg of tigertemp.label

type frame = {
  name: string,
  nameViejo: string,
  formals: bool list,
  arguments: access list ref, 
  locals: bool list,
  actualArg: int ref,
  actualLocal: int ref,
  actualReg: int ref
}

type register = string

datatype frag = PROC of {body: tigertree.stm, frame: frame}
  | STRING of tigertemp.label * string

fun newFrame{name, nameViejo,formals} = {
  name=name,
  nameViejo=nameViejo,
  formals=formals,
  arguments=ref [],
  locals=[],
  actualArg=ref argsInicial,     (*Cantidad de argumentos guardados en la pila*)
  actualLocal=ref localsInicial, (*Cantidad de variables locales guardadas en la pila*)
  actualReg=ref regInicial
}

fun name (f: frame) = #name f

fun nameViejo (f: frame) = #nameViejo f

fun string(l, s) = l^tigertemp.makeString(s)^"\n"

fun getFormals(f: frame) = #formals f

fun getLocals(f: frame) = #locals f

(* El InFrame que se agrega al inicio corresponde al static link*)
fun formals({arguments=ar, ...}: frame) = [InFrame (offStaticLink)] @ !ar 

fun maxRegFrame(f: frame) = !(#actualReg f)

fun allocArg (f: frame) b = 
  let val acc = 
    (case b of
    true =>
      let 
        val ret = (argsOffInicial-(!(#actualArg f)*wSz))
        val _ = #actualArg f := !(#actualArg f)+1
      in  InFrame ret end
    | false => (InReg(tigertemp.newtemp())))
  in (#arguments f := !(#arguments f) @ [acc];acc) end

fun allocLocal (f: frame) b = 
  case b of
  true =>
    let val ret = InFrame ((~(!(#actualLocal f))-(!(#actualArg f)))*localsGap+argsOffInicial) 
    in  #actualLocal f:=(!(#actualLocal f)+1); ret end
  | false => InReg(tigertemp.newtemp())

fun getFrame 0 = TEMP(fp)
  | getFrame n = MEM(BINOP(PLUS, (getFrame (n-1)), CONST offStaticLink))

fun exp (InFrame k) e = MEM(BINOP(PLUS, getFrame e, CONST k))
  | exp (InReg l) e = (TEMP l)  


fun externalCall(s, l) = CALL(NAME s, l)

fun seq [] = EXP (CONST 0)
  | seq [s] = s
  | seq (x::xs) = SEQ (x, seq xs)

fun procEntryExit1 (f : frame,body) =  
  let
    val isMain = (#name f) = "_tigermain"
    fun zipear [] _ = []
    | zipear (x::xs) n = [(x,n)] @ zipear xs (n+1)

    val lacc : (access * int) list = zipear (formals f) 0 

    fun natToReg 0 = rdi
    | natToReg 1 = rsi
    | natToReg 2 = rdx
    | natToReg 3 = rcx
    | natToReg 4 = r8
    | natToReg 5 = r9
    | natToReg _ = raise Fail "No deberia pasar (natToReg)"       

    fun accToMove ((InReg t),n) =
          if n<6 then (tigertree.MOVE (TEMP t,TEMP (natToReg n)))
                 else tigertree.MOVE(TEMP t,MEM(BINOP(PLUS, TEMP(fp), CONST (offArgs + (n-6)*localsGap))))
                 (*A partir del fp hay que sumar porque estamos queriendo acceder a la pila del llamante*)
        | accToMove ((InFrame k),n) =
            if n<6 then (tigertree.MOVE (MEM(BINOP(PLUS, TEMP(fp), CONST k)) ,TEMP (natToReg n)))
                   else tigertree.MOVE (MEM(BINOP(PLUS, TEMP(fp), CONST k)) ,MEM(BINOP(PLUS, TEMP(fp), CONST (offArgs + (n-6)*localsGap))))                                                     
  in  if isMain then body else SEQ (seq (map accToMove lacc),body) end

fun procEntryExit2 (f : frame,body : instr list) =  
  let
    val isMain = (name f) = "_tigermain"
  in case isMain of 
      false => (let fun store r = 
                            let 
                              val newTemp = tigertemp.newtemp()
                            in (tigerassem.MOVE {assem="movq %'s0, %'d0\n",dst=newTemp,src=r},newTemp) end
                    val (storeList,tempList) = ListPair.unzip (map store calleesaves')
                    val fetchTemps = ListPair.zip (tempList, calleesaves')
                    fun fetch (t,c) = tigerassem.MOVE {assem="movq %'s0, %'d0\n",dst=c,src=t}
                    val fetchList = map fetch fetchTemps
                in storeList@body@fetchList end) 
      | true => body end
  
fun pow2 0 = 1
  | pow2 n = 2*(pow2 (n-1))

fun pot2 n i = let val m = pow2 i
        in if n <= m then m else pot2 n (i+1) end
                                      
fun procEntryExit3 (f: frame,body : instr list) =  
  let
    val argsByStack = if length(getFormals f) > 6 then (length(getFormals f) - 6) else 0
    val space = ((argsByStack + MAX_ARGS_STACK + 1 + !(#actualLocal f)) * 8)
    val prol = [OPER {assem = "pushq %'s0\n",src=["rbp",sp],dst=[sp],jump=NONE},
          tigerassem.MOVE {assem="movq %'s0, %'d0\n",dst="rbp",src="rsp"},
          OPER {assem="subq $"^its(space)^", %'d0\n",src=["rsp"],dst=["rsp"],jump=NONE}]
    val epil = [tigerassem.MOVE {assem="movq %'s0, %'d0\n",dst="rsp",src="rbp"},
          OPER {assem = "pop %'d0\n",src=[sp],dst=["rbp",sp],jump=NONE},
          OPER {assem = "ret\n",src=[],dst=[],jump=NONE}]
  in prol@body@epil end
            
end
            
