open tigerlex
open tigergrm
open tigerescap
open tigerseman
open tigermunch
open tigersimpleregalloc
open BasicIO Nonstdio
open tigercolor

fun lexstream(is: instream) =
  Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing(lbuf) = (print("Error en parsing!("
  ^(makestring(!num_linea))^
  ")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")

fun main(args) =
  let fun arg(l, s) =
      (List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
    val (arbol, l1)      = arg(args, "-arbol")
    val (escapes, l2)    = arg(l1, "-escapes") 
    val (ir, l3)         = arg(l2, "-ir") 
    val (canon, l4)      = arg(l3, "-canon") 
    val (code, l5)       = arg(l4, "-code") 
    val (flow, l6)       = arg(l5, "-flow")     
    val (inter, l7)      = arg(l6, "-inter")      (* codigo intermedio del programa *)
    val (precolored, l8) = arg(l7, "-precolored") (* codigo assembler luego de munch *) 
    val (sregalloc, l9)  = arg(l8, "-sregalloc")  (* codigo assembler luego de regalloc (manda todo a memoria) *)
    val (asm, l10)       = arg(l9, "-asm")        (* genera un archivo (prueba.s) con el assembler del programa *)
    val entrada =
      case l10 of
      [n] => ((open_in n)
          handle _ => raise Fail (n^" no existe!"))
      | [] => std_in
      | _ => raise Fail "opcion desconocida!"
      
    val lexbuf = lexstream entrada
    
    val expr = prog Tok lexbuf handle _ => errParsing lexbuf
    
    val _ = findEscape(expr)
    
    val _ = if arbol then tigerpp.exprAst expr else ()
    
    val _ = transProg(expr)
    
    val frags = tigertrans.getResult() : tigerframe.frag list
    
    val canonizar = tigercanon.traceSchedule o tigercanon.basicBlocks o tigercanon.linearize
    
    fun makelist [] = ([],[])
      | makelist (tigerframe.PROC {body, frame} :: l) = 
      let 
        val (la,lb) = makelist l        
      in ((canonizar (body),frame) :: la, lb)
      end
      | makelist (tigerframe.STRING (lab,s) :: l) = 
      let
        val (la,lb) = makelist l
      in (la, (lab,s) :: lb)
      end
      
    val (b,c) = makelist frags
    
    val _ = if inter then (tigerinterp.inter true b c) else ()    
    
    fun id x = x    
    
    (* Escribe el codigo assembler de los strings para el programa *)
    fun concatInstr [] = "\n"
      | concatInstr (x::xs) = "\t"^x^"\n"^concatInstr(xs) 
    
    fun asmStrings [] = ""
      | asmStrings ((lab,s)::xs) =
          let
            val fstLetter = str(hd(String.explode lab))
            val options = if s = "" then (if fstLetter = "L" then 1 else 0) else 1
            val sizeInt = String.size s
            val sizeStr = Int.toString(sizeInt)                     
            val cond = (String.isSuffix ("\\x0a") s)
            (*si contiene como caracteres finales \\x0a los extraigo y en su lugar pongo \n*)
            val s' = if cond then String.substring(s,0,sizeInt-4)^"\\n" else s
            (*si contiene como caracteres finales \\x0a modifico la longitud que ahora sera #str + 1*)
            val sizeStr' = if cond then Int.toString(sizeInt-3) else sizeStr
          in case options of
              0 => (asmStrings xs)
              | 1 => (".align 16\n.type "^lab^", @object\n.size "^lab^", 16\n"^lab^
                      ":\n\t.quad "^sizeStr'^"\n\t.ascii \""^s'^"\"\n\n")^(asmStrings xs)
          end
                    
    (* funcion auxiliar, aplica el generador de codigo assembler (tigermunch)*)
      
    fun apCode (lstm,f) =
        let 
          val _ = print ("nuevo frame: "^(tigerframe.name f)^"\n")
        in (f,List.concat(map (fn s => tigermunch.codeGen f s) lstm)) end
    
    val _ = if precolored
               then
                 (let
                    val l11 = (List.map apCode b) : ((tigerframe.frame * tigerassem.instr list) list)                 
                    val l12 = List.concat (map (fn (f,il) => il) l11)                 
                    in map (fn (i) => print((tigerassem.format id i) ^ "\n")) l12 end)
               else [()]
                                
    val _ = if sregalloc
               then 
                 (let 
                    val l1 = (List.map apCode b) : ((tigerframe.frame * tigerassem.instr list) list)    
                    val l2 = List.concat (map (fn (f,lin) => tigersimpleregalloc.simpleregalloc f lin) l1)
                  in map (fn (i) => print((tigerassem.format id i) ^ "\n")) l2 end)
                else [()]        
    
    fun asmFunction [] = "\n"
        | asmFunction ((body, f):: xs) =
            let
              val nFrame = tigerframe.name f
              val prol = ".globl "^nFrame^"\n.type "^nFrame^",@function\n"^nFrame^":\n"               
              val l1 = List.concat(map (fn s => tigermunch.codeGen f s) body)
              val l2 = tigerframe.procEntryExit2(f,l1)
              val l3 = tigerframe.procEntryExit3(f,l2)  
              (* T/F - prints debug *)      
              val (pintar, l4) = tigercolor.colorear(l3,f,true)
              val l5 = map (fn i => tigerassem.format pintar i) l4                
            in prol^"\n"^concatInstr l5^"\n"^(asmFunction xs) end
                
    val _ = if asm then (let
              val outfile = open_out "../tests/TestAssm/prueba.s"
              val _ = output(outfile, ".section\t.rodata\n\n")
              val _ = output(outfile, asmStrings c)
              val _ = output(outfile, ".section\t.text.startup,\"ax\",@progbits\n\n")             
              val _ = output(outfile, asmFunction b)              
              val _ = close_out outfile
              in () end) else ()
    in 
    print "yes!!\n"
  end handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
