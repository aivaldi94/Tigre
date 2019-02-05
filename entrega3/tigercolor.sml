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
	
		fun areAdj (t1,t2) = case (tabBusca (t1,!interf)) of
								NONE => raise Fail "No deberia pasar (temp no encontrado)"
								| SOME l => List.null (List.filter (fn e => ((e <= t2) andalso (e >= t2))) (Splayset.listItems l))	
			
		fun getDegree t = Splayset.numItems (buscoEnTabla t)
			
		val degree = ref (tabAAplica (id,Splayset.numItems,!interf))	
			
		val setOfAllTemps = addList (empty, tabClaves (!degree))
		
		(* simplifyWorklist: tigertemp.temp Splayset.set - nodos no relacionados con move y de grado menor a K *)
			
		fun fillSimplifyWorkSet (tDegree, tMoveRel) = let
														val lowDegreeList = tabClaves (tabFiltra ((fn n => if n < K then true else false),!tDegree))
														val nonMoveRelSet = difference (setOfAllTemps, !tMoveRel)
													  in addList (nonMoveRelSet,lowDegreeList) end
		val simplifyWorkSet = ref (fillSimplifyWorkSet (degree, moveRelated))

		(* freezeWorklist: tigertemp.temp Splayset.set - nodos relacionados con move y de grado menor a K *)

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
			
		(* selectStack: pila que contiene los temporales eliminados del grafo *)
		val selectStack = ref ([])
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
									val setKNeig = (Splayset.addList (empty, (listTemps @ tabClaves(tabFiltra (fn n => n = K,!degree))))) : tigertemp.temp Splayset.set
									(* a cada temp de la lista original le resto un vecino *)
									fun minusOne n = case tabBusca(n,!degree) of
														NONE => raise Fail "No deberia pasar minusOne"
														| SOME i => i-1
									val _ = map (fn n => tabRInserta (n,minusOne n,!degree)) listTemps
									(*elimino del conjunto spillWorkSet los elementos del conjunto listKNeig*)
									val _ = spillWorkSet := difference (!spillWorkSet,setKNeig)
									(*llamo a la funcion*)
									(*val _ = enableMoves (setKNeig)*)
									(* para cada temp del conjunto evaluo lo que hace aux *)
									fun aux n = if isMoveRelated n then freezeWorkSet := add (!freezeWorkSet,n)
																   else simplifyWorkSet := add (!simplifyWorkSet,n)
									val _ = Splayset.app aux setKNeig
									in () end 
									
		fun Simplify () = let
							(* obtengo un elemento del conjunto simplifyWorkList, lo llamo N*) 
							val n = hd(listItems (!simplifyWorkSet))
							(* elimino N del conjunto simplifyWorkList *)						
							val _ = simplifyWorkSet := difference (!simplifyWorkSet,addList(empty,[n]))
							(* pongo N en el stack de temps seleccionados *) 						
							val _ = selectStack := !selectStack @ [n]	
							(* obtengo los temps adyacentes N *)											
							val adjN = buscoEnTabla (n,!interf)
							(* llamo a la funcion decrementDegree pasando como argumento los adyacentes a N *)
						 in (decrementDegree (adjN);()) end

	in print("ok\n") end	 
end

