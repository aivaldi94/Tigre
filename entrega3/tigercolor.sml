structure tigercolor :> tigercolor =
struct
	open tigerassem
	open tigertemp
	open tigertab
	open Splayset
	open tigerbuild
	open tigerframe

	
	val empty = empty String.compare														
		
	
	val selectStack = ref ([])	
	val spillWorkSet = ref (empty)
	val simplifyWorkSet = ref(empty)
	val degree = ref (tabNueva())
	val color = ref (tabNueva())
	val precoloredList = ref([])
	val precoloredSet = ref(empty)
	val spilledNodes = ref ([])
	val registersSet = ref(empty)
	
	fun getDegree t = buscoEnTabla (t,!degree)
		
	fun fillSimplifyWorkSet () = let
									val lowDegreeList = tabClaves (tabFiltra ((fn n => n < K ),(!degree)))
									(*val nonMoveRelSet = difference (setOfAllTemps, !tMoveRel)*)
									(* agregar para coalese y spill in addList (nonMoveRelSet,lowDegreeList) end*)
								 in addList (empty,lowDegreeList) end
													  
	fun minusOneSet s x = x-1 
														
	fun decrementDegree (s) = let 								
								(* paso a lista el conjunto de temporales*)
								val listTemps = listItems s
								(* me quedo con los temps de la lista cuyo grado es K *)									
								
								val listKNeig = List.filter (fn n => (buscoEnTabla(n,!degree)) = K) listTemps
								(* a cada temp de la lista original le resto un vecino *)
								
								fun minusOne n = case tabBusca(n,!degree) of
													NONE => raise Fail "No deberia pasar minusOne"
													| SOME i => i-1
								val _ = map (fn n => tabRInserta (n,minusOne n,!degree)) listTemps
								(*elimino del conjunto spillWorkSet los elementos de listKNeig*)
								val setKNeig = addList(empty,listKNeig)
								val _ = spillWorkSet := difference (!spillWorkSet,setKNeig)
								(*llamo a la funcion*)
								(*val _ = enableMoves (setKNeig)*)
								(* para cada temp del conjunto evaluo lo que hace aux *)
								(*
								fun aux n = if isMoveRelated n then freezeWorkSet := add (!freezeWorkSet,n)
															   else simplifyWorkSet := add (!simplifyWorkSet,n)
								val _ = Splayset.app aux setKNeig
								*)
								in addList(empty,listKNeig) end 
								

	fun fillColor ([],c) = c
	  | fillColor ((x::xs),c) = tabRInserta(x,x,(fillColor (xs,c)))
	  												
	fun selectSpill () = let
							val m = hd(listItems (!spillWorkSet))
							val _ = spillWorkSet := difference (!spillWorkSet,add(empty,m))
							val _ = simplifyWorkSet := union (!simplifyWorkSet,add(empty,m))
						in repeatUntil() end
	
	and simplify () = case (numItems(!simplifyWorkSet)) of 
								0 => repeatUntil()
								| _ => (let val n = hd(listItems (!simplifyWorkSet))											
											val _ = selectStack := !selectStack @ [n]				
											val adjN = difference(buscoEnTabla (n,!interf),addList(empty,!selectStack))
											val setK = decrementDegree (adjN)
											val _ = simplifyWorkSet :=	difference (union(!simplifyWorkSet,setK),addList(empty,[n]))
											in  simplify () end)

	and repeatDo () = let
						val lsws = numItems (!simplifyWorkSet)
						val lspillws = numItems(!spillWorkSet)
					   in if (lsws <> 0) then simplify() else (if lspillws <> 0 then selectSpill() else ()) end
					   
	and repeatUntil () = let
							val lsws = numItems (!simplifyWorkSet)
							val lspillws = numItems(!spillWorkSet)
							val fin = (lsws = 0) andalso (lspillws = 0)
						  in if fin then () else repeatDo () end												
													
	fun assignColors (cNodes, stack) = case (length (stack)) of
						
								0 => (print("stack:\n");List.app (fn n => print (n^"\n")) (!selectStack);selectStack:=[];print ("Tabla colores\n");tigertab.tabPrintTempTemp(!color);cNodes)
								| _ => case (member(!precoloredSet,hd (stack))) of
									false =>
										(let 
											val n = hd (stack)
											val stack' = tl(stack)
											val adj = buscoEnTabla (n,!interf) : tigertemp.temp Splayset.set
											val uni = union (cNodes, !precoloredSet) : tigertemp.temp Splayset.set
											(*val _ = print "\nuni es\n"
											val _ = List.app print (listItems uni)*)
											fun discardColors (n : tigertemp.temp,s) = let 
																					val isMember = member (uni,n)
																					val colorUsed = if isMember then add(empty,buscoEnTabla(n,!color)) else empty
																					(*val _ = print ("Descarto\n")
																					val _ =  List.app print (listItems colorUsed)*)
																				in difference (s,colorUsed) end
											val okColors = Splayset.foldl discardColors (!registersSet) adj
											(*val _ = print "\nokColors es\n"
											val _ = List.app print (listItems okColors)*)
											val cNodes' = case length (listItems(okColors)) of
														0 => (let val _ = print ("\nEL NODO SPILL ES "^n^"\n")
																val _ = spilledNodes := n::(!spilledNodes)
															 in cNodes end)
														| _ => (let 
																	val c = hd(listItems(okColors))
																	val _ = color := tabRInserta (n,c,!color)					
																in union (cNodes, add(empty, n)) end)
										in assignColors (cNodes', stack') end)
									| true => assignColors (cNodes, tl(stack))											

	(* Tomará la lista de instrucciones, un temporal, un offset*)

	fun its n =  if n<0 then "-" ^ Int.toString(~n) else Int.toString(n) 

	fun forEachSpilled ([] : instr list, tmp : tigertemp.temp, offset : int) = ([],[]): (instr list * tigertemp.temp list)
		| forEachSpilled (i::instr, tmp, offset) = 
			case i of
			OPER {assem=a,dst=d,src=s,jump=j} => 
				(let	
					fun igual n = (n=tmp)
					val isDst = List.exists igual d
					val isSrc = List.exists igual s
				 in (case (isDst andalso isSrc) of
					true => let val _ = raise Fail "Es fuente y destino"
								val newTemp = newtemp()
								val newInstr1 = OPER {assem="movq "^its(offset)^"(%'s0), %'d0\n",dst=[newTemp],src=[fp],jump=NONE}
								val d' = map (fn n => if n = tmp then newTemp else n) d
								val s' = map (fn n => if n = tmp then newTemp else n) s
								val rewInstr = OPER {assem=a,dst=d',src=s',jump=j}
								val newInstr2 = OPER {assem="movq %'s0, "^its(offset)^"(%'s1)\n",dst=[],src=[fp,newTemp],jump=NONE}
								val (instructions, temps) = forEachSpilled(instr,tmp,offset)
							in ([newInstr1,rewInstr,newInstr2]@instructions, newTemp::temps)end
					| false => let val newTemp = newtemp()
								in (case isDst of
									true => let val _ = raise Fail "Es destino"
												val d' = map (fn n => if n = tmp then newTemp else n) d
												val rewInstr = OPER {assem=a,dst=d',src=s,jump=j}
												val newInstr = OPER {assem="movq %'s0, "^its(offset)^"(%'s1)\n",dst=[],src=[newTemp,fp],jump=NONE}
												val (instructions, temps) = forEachSpilled(instr,tmp,offset)
											in ([rewInstr,newInstr]@instructions, newTemp::temps)end
									| false => (case isSrc of
										true => let val _ = raise Fail "Es fuente"
													val s' = map (fn n => if n = tmp then newTemp else n) s
													val newInstr = OPER {assem="movq "^its(offset)^"(%'s0), %'d0\n",dst=[newTemp],src=[fp],jump=NONE}
													val rewInstr = OPER {assem=a,dst=d,src=s',jump=j}
													val (instructions, temps) = forEachSpilled(instr,tmp,offset)
												in ([newInstr,rewInstr]@instructions, newTemp::temps)end
										| false => let val (instructions, temps) = forEachSpilled(instr,tmp,offset)
													in (i::instructions,temps) end))end)end)
			 | MOVE {assem=a,dst=d,src=s} => 
				(let	val isDst = (d = tmp)
						val isSrc = (s = tmp)
				 in (case (isDst andalso isSrc) of
					true => forEachSpilled(instr,tmp,offset)
					| false => let val newTemp = newtemp()
								in (case isDst of
									true => let val rewInstr = MOVE {assem=a,dst=newTemp,src=s}
												val newInstr = OPER {assem="movq %'s0, "^its(offset)^"(%'s1)\n",dst=[],src=[newTemp,fp],jump=NONE}
												val (instructions, temps) = forEachSpilled(instr,tmp,offset)
											in ([rewInstr,newInstr]@instructions, newTemp::temps)end
									| false => (case isSrc of
										true => let val newInstr = OPER {assem="movq "^its(offset)^"(%'s0), %'d0\n",dst=[newTemp],src=[fp],jump=NONE}
													val rewInstr = MOVE {assem=a,dst=d,src=newTemp}
													val (instructions, temps) = forEachSpilled(instr,tmp,offset)
												in ([newInstr,rewInstr]@instructions, newTemp::temps)end
										| false => let val (instructions, temps) = forEachSpilled(instr,tmp,offset)
													in (i::instructions,temps) end))end)end)
				| _ => let val (instructions, temps) = forEachSpilled(instr,tmp,offset)
						in (i::instructions,temps) end																			
														
	(* La lista de instrucciones y el frame serán importados. La lista de temporales primera debe ser vacía*) 
	fun rewriteProgram(linstr : instr list, f : frame, ltemp : tigertemp.temp list) = case length(!spilledNodes) of
												0 => (print("Programa reescrito\n");printInstrList linstr;(linstr,ltemp))
												| _ => 	let 
															val n = hd(!spilledNodes) 
															val _ = spilledNodes := tl(!spilledNodes)
															val InFrame k = allocLocal f true
															val (instructions, temps) = forEachSpilled(linstr,n,k)
														in rewriteProgram(instructions,f,ltemp@temps) end

	fun initialize() = let 	val _ = selectStack := []
							val _ = spillWorkSet := empty
							val _ = simplifyWorkSet := empty
							val _ = degree := tabNueva()
							val _ = color := tabNueva()
							val _ = precoloredSet := empty
							val _ = spilledNodes := []
							val _ = registersSet := empty
						in () end

	fun pintar n = (case tabBusca(n,!color) of
						NONE => raise Fail ("Temporal sin color asignado "^n)
						| SOME c => c) 	
										
	fun colorear'(l,f,initial) = 
		let 
			val _ = tigerbuild.build(l,0)		
		
			val _ = spilledNodes := []
			
			(*makeWorkList()*)
			val _ = degree := tabAAplica (id,Splayset.numItems,!interf)
			val _ = precoloredList := ["rbx", "rsp","rdi", "rsi", "rdx", "rcx", "r8", "r9", "rbp", "rax","r10","r11","r12","r13","r14","r15"]
			
			val _ = color := fillColor(!precoloredList,!color)	
			val _ = precoloredSet := addList(empty, !precoloredList)		
			val _ = simplifyWorkSet := initial
			
			val _ = spillWorkSet := addList (empty,tabClaves (tabFiltra ((fn n => n >= K),!degree)))		
									
			(*repeat until*)		
			val _ = repeatUntil()	

			(* assign colors*)
			val coloredNodes = assignColors(empty, !selectStack)

			(* rewrite program*)
			val (instructions, temps) = rewriteProgram(l,f,[])
			
			val _ = (print("Temporales agregados\n");List.app print temps)
			
		in if temps = [] then (pintar,instructions) else colorear'(instructions,f, addList (coloredNodes, temps) ) end	
		
	fun colorear (l,f,printt) = 
		let
			(* OJOO: HAY QUE VACIAR TODAS LAS LISTAS Y CONJUNTOS CADA VEZ QUE EMPIEZO EL ALGORITMO*)
			val _ = initialize()
			
			val _ = tigerbuild.build(l,printt)		
			(*makeWorkList()*)
			val _ = degree := tabAAplica (id,Splayset.numItems,!interf)
			val _ = precoloredList := ["rbx", "rsp","rdi", "rsi", "rdx", "rcx", "r8", "r9", "rbp", "rax","r10","r11","r12","r13","r14","r15"]
			val registers = [rv,"rbx",rcx,rdx,rsi,rdi,r8,r9,"r10","r11","r12","r13","r14","r15"]
			val _ = registersSet := addList (empty, registers) 
				
			val _ = color := fillColor(!precoloredList,!color)			
			val _ = precoloredSet := addList(empty, !precoloredList)
			val _ = simplifyWorkSet := fillSimplifyWorkSet ()
			
			val _ = spillWorkSet := addList (empty,tabClaves (tabFiltra ((fn n => n >= K),!degree)))								
			(*repeat until*)		
			val _ = repeatUntil()	

			(* assign colors*)
			val coloredNodes = assignColors(empty, !selectStack)

			(* rewrite program*)
			val (instructions, temps) = rewriteProgram(l,f,[])
			
			val _ = (print("Temporales agregados\n");List.app print temps)
									 		 				
		in if temps = [] then (print("No hizo spill\n");(pintar,instructions)) else colorear'(instructions,f, addList (coloredNodes, temps) ) end	 
end

