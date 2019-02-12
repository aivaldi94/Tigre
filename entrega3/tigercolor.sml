structure tigercolor :> tigercolor =
struct
	open tigerassem
	open tigertemp
	open tigertab
	open Splayset
	open tigerbuild
	
	val empty = empty String.compare														
		
	(* selectStack: pila que contiene los temporales eliminados del grafo *)
	val selectStack = ref ([]) : tigertemp.temp list ref	
	(* moves que todavia no estan listos para unirse*)
	val activeMoves = empty	
	val spillWorkSet = ref (empty)
	val simplifyWorkSet = ref(empty)
	val degree = ref (tabNueva())
	val coloredNodes = empty
	val registers = ref(empty): tigertemp.temp Splayset.set ref
	val color = ref (tabNueva())
	val precoloredList = ref([])
	val precolored = ref(empty)
	val spilledNodes = ref ([])
	
	fun getDegree t = Splayset.numItems (buscoEnTabla t)	
		
	fun fillSimplifyWorkSet (tDegree, tMoveRel) = let
														val lowDegreeList = tabClaves (tabFiltra ((fn n => if n < K then true else false),!tDegree))
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
								
	fun Simplify () = case (numItems(!simplifyWorkSet)) of 
								0 => (print("stack:\n");List.app (fn n => print (n^"\n")) (!selectStack))
								| _ => (let val n = hd(listItems (!simplifyWorkSet))											
											val _ = selectStack := !selectStack @ [n]				
											val adjN = buscoEnTabla (n,!interf)
											val setK = decrementDegree (adjN)
											val _ = simplifyWorkSet :=	difference (union(!simplifyWorkSet,setK),addList(empty,[n]))
											in  Simplify () end)
	
	


	fun fillColor [] = tabNueva()
	  | fillColor (x::xs) = tabRInserta(x,x,(fillColor xs))	
	  												
	fun selectSpill () = let
							val m = hd(listItems (!spillWorkSet))
							val _ = spillWorkSet := difference (!spillWorkSet,add(empty,m))
							val _ = simplifyWorkSet := union (!simplifyWorkSet,add(empty,m))
						in () end
	
	fun repeatDo () = let
						val lsws = numItems (!simplifyWorkSet)
						val lspillws = numItems(!spillWorkSet)
					   in if (lsws <> 0) then Simplify() else (if lspillws <> 0 then selectSpill() else ()) end
					   
	fun repeatUntil () = let
							val lsws = numItems (!simplifyWorkSet)
							val lspillws = numItems(!spillWorkSet)
							val fin = (lsws = 0) andalso (lspillws = 0)
						  in if fin then () else repeatDo () end												
													
	fun AssignColors (cNodes, stack) = case (length (stack)) of
								
								0 => (print ("Tabla colores\n");tigertab.tabPrintTempTemp(!color))
								| _ => case (member(!precolored,hd (stack))) of
									false =>
										(let 
											val n = hd (stack)
											val stack' = tl(stack)
											val adj = buscoEnTabla (n,!interf) : tigertemp.temp Splayset.set
											val uni = union (cNodes, !precolored) : tigertemp.temp Splayset.set
											val okColors = Splayset.foldl (fn (n : tigertemp.temp,s) => if member (uni,n) then difference (s,add(empty,buscoEnTabla(n,!color))) else s) (!registers) adj
											val cNodes' = case length (listItems(okColors)) of
														0 => (let 
																val _ = spilledNodes := n::(!spilledNodes)
															 in cNodes end)
														| _ => (let 
																	val c = hd(listItems(okColors))
																	val _ = color := tabRInserta (n,c,!color)					
																in union (cNodes, add(empty, n)) end)
										in AssignColors (cNodes', stack') end)
									| true => AssignColors (cNodes, tl(stack))											

	(* Tomará la lista de instrucciones, un temporal, un offset*)
	fun rewriteProgram ([] : instr list, tmp : tigertemp.temp, offset : int) = ([],[]): (instr list * tigertemp.temp list)
		| rewriteProgram (i::instr, tmp, offset) = case i of
														OPER {assem=a,dst=d,src=s,jump=j} => let val newTemp = newtemp()
																								 val add = tigerframe.spilledAddress(offset)
																								 fun igual n = (string.compare tmp n)
																								 val isDst = case (find igual d) of
																												SOME _ => true
																												| NONE => false
																								 val isSrc = case (find igual d) of
																												SOME _ => true
																												| NONE => false
																								 val lins = case isDst of
																												true => let val rewInstr = OPER {assem=a,dst=newTemp,src=s,jump=j}
																															val newInstr = MOVE {assem=a,dst=add,src=newTemp}
																														in [rewInstr,newInstr]
																														
																								 
																								  
																								  val (intructions, temps) = rewriteProgram(instr,tmp,offset)
																								in ([rewInstr,newInstr]@instructions, newTemp::temps)end
														| LABEL {assem=_,lab=_} => let val (intructions, temps) = rewriteProgram(instr,tmp,offset)
																						in (i::instructions,temps) end
														| MOVE {assem=a,dst=tmp,src=tmp} => rewriteProgram(instr,tmp,offset)
														| MOVE {assem=a,dst=tmp,src=s} => let val newTemp = newtemp()
																							  val add = tigerframe.spilledAddress(offset)
																							  val rewrInstr = MOVE {assem=a,dst=newTemp,src=s}
																							  val newInstr = MOVE {assem=a,dst=add,src=newTemp} (* es OPER*)
																							  val (intructions, temps) = rewriteProgram(instr,tmp,offset)
																						  in ([rewInstr,newInstr]@instructions, newTemp::temps)end
														| MOVE {assem=a,dst=d,src=tmp} => let val newTemp = newtemp()
																							  val add = tigerframe.spilledAddress(offset)
																							  val newInstr = MOVE {assem=a,dst=newTemp,src=add}
																							  val rewrInstr = MOVE {assem=a,dst=d,src=newTemp}
																							  val (intructions, temps) = rewriteProgram(instr,tmp,offset)
																						  in ([newInstr,rewInstr]@instructions, newTemp::temps)end
														(*| _ => ...*)
	
	fun colorear (l,printt) = 
	let
		
		val _ = tigerbuild.build(l,printt)		
		(*makeWorkList()*)
		val _ = degree := tabAAplica (id,Splayset.numItems,!interf)
		val _ = precoloredList := ["rdi", "rsi", "rdx", "rcx", "r8", "r9", "rbp", "rax"]
		val _ = registers := addList(empty,["rax","rbx","rcx","rdx","rsi","rdi","rbp","rsp","r8","r9","r10","r11","r12","r13","r14","r15"]) 					
		val _ = color := fillColor(!precoloredList)			
		val _ = precolored := addList(empty, !precoloredList)
		val _ = simplifyWorkSet := fillSimplifyWorkSet (degree, tigerbuild.moveRelated)
		val _ = spillWorkSet := addList (empty,tabClaves (tabFiltra ((fn n => n > K),!degree)))								
		(*repeat until*)		
		val _ = repeatUntil()	
		val _ = AssignColors(coloredNodes, !selectStack)
		fun pintar n = (case tabBusca(n,!color) of
												NONE => raise Fail ("Temporal sin color asignado "^n)
												| SOME c => c) 							 		 				
	in pintar end	 
end

