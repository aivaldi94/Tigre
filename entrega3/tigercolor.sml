structure tigercolor :> tigercolor =
struct
	open tigerassem
	open tigertemp
	open tigertab
	open Splayset
	open tigerbuild
	
	val empty = empty String.compare
	
	fun colorear () = 
	let
		val _ = print ("En tigercolor\n")
	
	(*
		fun areAdj (t1,t2) = case (tabBusca (t1,!interf)) of
								NONE => raise Fail "No deberia pasar (temp no encontrado)"
								| SOME l => List.null (List.filter (fn e => ((e <= t2) andalso (e >= t2))) (Splayset.listItems l))	
		*)	
		fun getDegree t = Splayset.numItems (buscoEnTabla t)
			
		val degree = ref (tabAAplica (id,Splayset.numItems,!interf))	
			(*
		val setOfAllTemps = addList (empty, tabClaves (!degree))
		
		(* simplifyWorklist: tigertemp.temp Splayset.set - nodos no relacionados con move y de grado menor a K *)
			*)
		fun fillSimplifyWorkSet (tDegree, tMoveRel) = let
														val lowDegreeList = tabClaves (tabFiltra ((fn n => if n < K then true else false),!tDegree))
														(*val nonMoveRelSet = difference (setOfAllTemps, !tMoveRel)*)
													  (* agregar para coalese y spill in addList (nonMoveRelSet,lowDegreeList) end*)
													  in addList (empty,lowDegreeList) end
													  
		val simplifyWorkSet = ref (fillSimplifyWorkSet (degree, moveRelated))
val _ = print ("Llena simplfy\n")
		(* freezeWorklist: tigertemp.temp Splayset.set - nodos relacionados con move y de grado menor a K *)
(*
		fun fillFreezeWorkSet (tDegree, tMoveRel) = let 
														val lowDegreeList = tabClaves (tabFiltra ((fn n => if n < K then true else false),!tDegree))
														val moveRelSet = !tMoveRel
													  in addList (moveRelSet,lowDegreeList) end
													  
		val freezeWorkSet = ref (fillFreezeWorkSet (degree, moveRelated)) 										
		(* spillWorklist: tigertemp.temp Splayset.set - nodos con grado mayor a K *)
													
		val spillWorkSet = ref(addList (empty,tabClaves (tabFiltra ((fn n => if n > K then true else false),!degree))))
													  
		(* Hacer lista worklistMoves: moves de temp a temp que pueden eliminarse (o sea que dst y src no tienen que estar unidos en interf).*)
		(* me conviene que esto sea un conjunto de tuplas? o sea si (a,b) pertenece a este conjunto quiere decir que a y b pueden unirse
			en un solo nodo porque a y b no estan unidos en interf *)
		fun fillWorkSetMoves 0 = let
									val i = buscoEnTabla (0, !natToInstr)
								in case i of
									OPER {assem=_,dst=_,src=_,jump=_} => []
									| LABEL {assem=_,lab=_} => []
									| MOVE {assem=_,dst=d,src=s} => if member (buscoEnTabla(s,!interf),d) then [] else [(d,s)]
								end				
		  | fillWorkSetMoves n = let
									val i = buscoEnTabla (n, !natToInstr)
								in case i of
									OPER {assem=_,dst=_,src=_,jump=_} => fillWorkSetMoves (n-1)
									| LABEL {assem=_,lab=_} => fillWorkSetMoves (n-1)
									| MOVE {assem=_,dst=d,src=s} => if member (buscoEnTabla(s,!interf),d) then fillWorkSetMoves (n-1) else (fillWorkSetMoves (n-1) @ [(d,s)])
								end																		
															
		val WorkSetMoves = ref (fillWorkSetMoves (!longNatToInstr))
*)			
		(* selectStack: pila que contiene los temporales eliminados del grafo *)
		val selectStack = ref ([]) : tigertemp.temp list ref
		(* moves que todavia no estan listos para unirse*)
		val activeMoves = empty
		(*
		(*moveList: tabla que asocia a cada temp con el conjunto de temps mediante los cuales esta relaiconado con un move*)
		fun fillMoveList 0 = empty
		val moveList = ref (fillMoveList (!longNatToInstr))
		(*funcion noveMoves*)
		fun nodeMoves n = intersection(buscoEnTabla(n,!moveList),union(activeMoves,WorkSetMoves))
		(* Simplify algoritmo en pagina 246 *)
		*)
		fun minusOneSet s x = x-1 
														
		fun decrementDegree (s) = let 								
									(* paso a lista el conjunto de temporales*)
									val listTemps = listItems s
									(* me quedo con los temps de la lista cuyo grado es K *)
									(*
									val setKNeig = (Splayset.addList (empty, (listTemps @ tabClaves(tabFiltra (fn n => n = K,!degree))))) : tigertemp.temp Splayset.set
									*)
									val listKNeig = List.filter (fn n => (buscoEnTabla(n,!degree)) = K) listTemps
									(* a cada temp de la lista original le resto un vecino *)
									
									fun minusOne n = case tabBusca(n,!degree) of
														NONE => raise Fail "No deberia pasar minusOne"
														| SOME i => i-1
									val _ = map (fn n => tabRInserta (n,minusOne n,!degree)) listTemps
									(*elimino del conjunto spillWorkSet los elementos del conjunto listKNeig*)
									(*val _ = spillWorkSet := difference (!spillWorkSet,setKNeig)*)
									(*llamo a la funcion*)
									(*val _ = enableMoves (setKNeig)*)
									(* para cada temp del conjunto evaluo lo que hace aux *)
									(*
									fun aux n = if isMoveRelated n then freezeWorkSet := add (!freezeWorkSet,n)
																   else simplifyWorkSet := add (!simplifyWorkSet,n)
									val _ = Splayset.app aux setKNeig
									*)
									in addList(empty,listKNeig) end 
									
		fun Simplify (sws) = case (numItems(sws)) of 
									0 => List.app (fn n => print (n^"\n")) (!selectStack)
									| _ => ( let val n = hd(listItems (sws))											
												val _ = selectStack := !selectStack @ [n]				
												val adjN = buscoEnTabla (n,!interf)
												val setK = decrementDegree (adjN)							
												in  Simplify (difference (union(sws,setK),addList(empty,[n]))) end)
		
		val precoloredList = ["rdi", "rsi", "rdx", "rcx", "r8", "r9", "rbp", "rax"]								
		val precolored = addList(empty, precoloredList)
		val coloredNodes = empty
		val registers = addList(empty,["rax","rbx","rcx","rdx","rsi","rdi","rbp","rsp","r8","r9","r10","r11","r12","r13","r14","r15"]) : tigertemp.temp Splayset.set

		fun fillColor [] = tabNueva()
		  | fillColor (x::xs) = tabRInserta(x,x,(fillColor xs))
		 val color = ref (fillColor(precoloredList)) : (tigertemp.temp ,tigertemp.temp) tigertab.Tabla ref 												
		  												
		  												
		fun AssignColors (cNodes, stack) = case (length (stack)) of
									
									0 => (print ("Tabla colores\n");tigertab.tabPrintTempTemp(!color))
									| _ => case (member(precolored,hd (stack))) of
										false =>
											(let 
												val n = hd (stack)
												val stack' = tl(stack)
												val adj = buscoEnTabla (n,!interf) : tigertemp.temp Splayset.set
												val uni = union (cNodes, precolored) : tigertemp.temp Splayset.set
												val okColors = Splayset.foldl (fn (n : tigertemp.temp,s) => if member (uni,n) then difference (s,add(empty,buscoEnTabla(n,!color))) else s) registers adj
												val c = if length (listItems(okColors)) = 0 then raise Fail "lista vacia en assig colors" else hd(listItems(okColors))
												val _ = color := tabRInserta (n,c,!color)
												val cNodes' = union (cNodes, add(empty, n))
											in AssignColors (cNodes', stack') end)
										| true => AssignColors (cNodes, tl(stack))
											
																																																		 		 				
	in (Simplify(!simplifyWorkSet);AssignColors(coloredNodes, !selectStack)) end	 
end

