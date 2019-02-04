structure tigerflow :> tigerflow =
struct
	open tigerassem
	open tigertemp
	open tigertab
	open Splayset

	val K = 3
	fun name x = x
	type interfTab = (tigertemp.temp, tigertemp.temp Splayset.set) tigertab.Tabla

	val empty = empty String.compare 
	
	fun colorear (instrList : instr list) = 
	let
	(* ---------------------------------------------------------------------------------------------------------- *)
		
		fun fillNatToInstr ([],_) = tabNueva()
			| fillNatToInstr (x::xs,n) = tabInserta (n,x,fillNatToInstr (xs,n+1))
			
		val natToInstr = ref (fillNatToInstr(instrList,0)) : (int, tigerassem.instr) tigertab.Tabla ref	
		
		val longNatToInstr = List.length(tabAList(!natToInstr)) - 1
		val _ = print("Cantidad de instrucciones "^Int.toString(longNatToInstr)^"\n")
		val _ = print ("\nImprimo natToInstr\n")
		val _ = tigertab.tabPrintIntInstr(!natToInstr)
		fun id x = x

	(* ---------------------------------------------------------------------------------------------------------- *)
		
		fun fillDefs i = case i of 
							OPER {assem=_,dst=d,src=_,jump=_} => addList (empty,d)
							| LABEL {assem=_,lab=_} => empty
							| MOVE {assem=_,dst=d,src=_} => singleton String.compare d 
								
		val defs = ref ((tabAAplica (id,fillDefs,!natToInstr)))
		val _ = print ("\nImprimo defs\n")
		val _ = tigertab.tabPrintIntTempSet(!defs)

	(* ---------------------------------------------------------------------------------------------------------- *)
		
		fun fillUses i = case i of 
							OPER {assem=_,dst=_,src=s,jump=_} => addList (empty,s)
							| LABEL {assem=_,lab=_} => empty
							| MOVE {assem=_,dst=_,src=s} => singleton String.compare s
								
		val uses = ref ((tabAAplica (id,fillUses,!natToInstr)))
		val _ = print ("\nImprimo uses\n")
		val _ = tigertab.tabPrintIntTempSet(!uses)
		
	(* ---------------------------------------------------------------------------------------------------------- *)
		
		(* Retorna los nodos del natToInstr que tienen como etiqueta a l *)
		fun findLabel l = tabClaves(tabFiltra ((fn i => case i of 
														LABEL {assem=_,lab=l1} => ((l1 <= l) andalso (l1 >= l))
														| _ => false), !natToInstr)) 
		
		(* lista de instrucciones de natToInstr*)
		(* ESTO NO TIENE SENTIDO HACERLO PORQUE ESA ES LA LISTA QUE TENEMOS QUE TENER PARA CREAR NATTOINSTR*)
		(*val instrList : tigerassem.instr list = (map (fn (x,y) => y) (tabAList (! natToInstr))) *)
								
		fun fillSuccs ([],_) = tabNueva()
			| fillSuccs ((x::xs),n) = let
											val empty = Splayset.empty Int.compare
									  in 
											case x of 
												OPER {assem=a,dst=d,src=s,jump=j} => (case j of
																				NONE => tabInserta(n,addList (empty,[n+1]),fillSuccs(xs,n+1))
																				| SOME l => tabInserta (n,addList(empty,List.concat ((List.map findLabel l) : int list list)), fillSuccs(xs,n+1)))
												| LABEL {assem=a,lab=l} => tabInserta(n,addList(empty,[n+1]),fillSuccs(xs,n+1)) 
												| MOVE {assem=a,dst=d,src=s} => tabInserta(n,addList(empty,[n+1]),fillSuccs(xs,n+1))	
									 end
									 
		val succs = ref (fillSuccs (instrList ,0))
		val _ = print ("\nImprimo succs\n")
		val _ = tigertab.tabPrintIntIntSet(!succs)
					
	(* ---------------------------------------------------------------------------------------------------------- *)
		
		(* busca la clave x en t y retorna el valor asociado
		Si no encuentra el valor, falla. Debe ser únicamente utilizado cuando el valor está en la tabla*)
		fun buscoEnTabla (x,t) = (case (tabBusca (x,t)) of 
									NONE => raise Fail "error buscoEnTabla"
									| SOME v => v)
									
		(* dado un nodo retorna si es un MOVE o no*)							
		fun isMove i = case tabBusca (i,!natToInstr) of
						SOME (MOVE {assem=a,dst=d,src=s}) => true
						| _ => false												
																																			
		fun forEachN (0,outNueva,outVieja,inNueva,inVieja) = let
															fun buscoEnTabla (x,t) = (case (tabBusca (x,t)) of 
																						NONE => empty
																						| SOME v => v)
															fun buscoEnTablaInt (x,t) = (case (tabBusca (x,t)) of 
																						NONE => Splayset.empty Int.compare
																						| SOME v => v)
															val b = buscoEnTabla (0,inVieja)
															val inNueva' = tabRInserta (0,b,inNueva)
															
															val b1 = buscoEnTabla (0,outVieja)
															val outNueva' = tabRInserta (0,b1,outNueva)
															
															val useN = buscoEnTabla (0,!uses)										
															val outN = buscoEnTabla (0,outVieja)									
															val defN = buscoEnTabla (0,!defs)									
															val inVieja' = tabRInserta(0,union(useN,difference(outN,defN)),inVieja)
															
															val succsN = listItems (buscoEnTablaInt (0,!succs))
															fun index n = listItems (buscoEnTabla (0,inVieja'))
															
															val m = Splayset.addList(empty,List.concat (List.map index succsN))
															val outVieja' = tabRInserta (0,m,outVieja)
														in
															(outNueva',outVieja',inNueva',inVieja')
														end
		| forEachN (n,outNueva,outVieja,inNueva,inVieja) = let
															fun buscoEnTabla (x,t) = (case (tabBusca (x,t)) of 
																						NONE => empty
																						| SOME v => v)
															fun buscoEnTablaInt (x,t) = (case (tabBusca (x,t)) of 
																						NONE => Splayset.empty Int.compare
																						| SOME v => v)
															val b = buscoEnTabla (n,inVieja)
															val inNueva' = tabRInserta (n,b,inNueva)
															
															val b1 = buscoEnTabla (n,outVieja)
															val outNueva' = tabRInserta (n,b1,outNueva)
															
															val useN = buscoEnTabla (n,!uses)										
															val outN = buscoEnTabla (n,outVieja)									
															val defN = buscoEnTabla (n,!defs)									
															val inVieja' = tabRInserta(n,union(useN,difference(outN,defN)),inVieja)
															
															val succsN = listItems (buscoEnTablaInt (n,!succs))
															fun index n = listItems (buscoEnTabla (n,inVieja'))
															
															val m = Splayset.addList(empty,List.concat (List.map index succsN))
															val outVieja' = tabRInserta (n,m,outVieja)
														in
															forEachN (n-1, outNueva',outVieja',inNueva',inVieja')
														end
														
		fun repeatUntil (outNueva,outVieja,inNueva,inVieja) = let 
															val (outNueva',outVieja',inNueva',inVieja') = forEachN (longNatToInstr,outNueva,outVieja,inNueva,inVieja)
															val fin = tabIgual (Splayset.equal,outNueva,outNueva') andalso tabIgual (Splayset.equal,inNueva,inNueva')
														in 
															if fin then (outNueva',outVieja',inNueva',inVieja') else  repeatUntil(outNueva',outVieja',inNueva',inVieja')
														end						
														
		fun liveness (0,(outNueva,outVieja,inNueva,inVieja)) = (outNueva,outVieja,inNueva,inVieja)
			| liveness (n,(outNueva,outVieja,inNueva,inVieja)) = let
																	val inVieja' = tabRInserta(n,empty,inVieja)
																	val outVieja' = tabRInserta(n,empty,outVieja)
																in liveness(n-1,repeatUntil(outNueva,outVieja',inNueva,inVieja'))
																end
														
		fun referenciar (a,b,c,d) =(ref a, ref b, ref c, ref d)
		
		val (liveOut, liveOutOld, liveIn, liveInOld) = referenciar (liveness(longNatToInstr,(tabNueva(),tabNueva(),tabNueva(),tabNueva())))
		
		val _ = print ("\nImprimo liveIn\n")
		val _ = tigertab.tabPrintIntTempSet(!liveIn)
		
		val _ = print ("\nImprimo liveOut\n")
		val _ = tigertab.tabPrintIntTempSet(!liveOut)
			
		(*******************************************************************************************************************************)
		
		val interf = ref (tabNueva())
				
		fun areAdj (t1,t2) = case (tabBusca (t1,!interf)) of
							NONE => raise Fail "No deberia pasar (temp no encontrado)"
							| SOME l => List.null (List.filter (fn e => ((e <= t2) andalso (e >= t2))) (Splayset.listItems l))	
		
		fun getDegree t = Splayset.numItems (buscoEnTabla t)
		
		val degree = ref (tabAAplica (id,Splayset.numItems,!interf)											)
		

		fun getTemps ([],l) = l 
			| getTemps ((x::xs,ts)) = case x of 
									OPER {assem=a,dst=d,src=s,jump=j} => let 
																			val _ = Splayset.addList (ts,s)
																			val _ = Splayset.addList (ts,d)
																		 in ts
																		 end
									| LABEL {assem=a,lab=l} => ts
									| MOVE {assem=a,dst=d,src=s} => let
																		val _ = add (ts,d)
																		val _ = add (ts,s)
																	in
																		ts
																	end
	(******************************************************************************************************************************)
		
		
		fun fillMoveRelated 0 = let
									val i = buscoEnTabla (0, !natToInstr)
								in case i of
									OPER {assem=_,dst=_,src=_,jump=_} => empty
									| LABEL {assem=_,lab=_} => empty
									| MOVE {assem=_,dst=d,src=s} => add (add (empty,s),d)
								end				
		  | fillMoveRelated n = let
									val i = buscoEnTabla (n, !natToInstr)
								in case i of
									OPER {assem=_,dst=_,src=_,jump=_} => fillMoveRelated (n-1)
									| LABEL {assem=_,lab=_} => fillMoveRelated (n-1)
									| MOVE {assem=_,dst=d,src=s} => add (add (fillMoveRelated (n-1),s),d) 
								end																		
														
		val moveRelated = ref (fillMoveRelated longNatToInstr)
		
		fun isMoveRelated t = member (!moveRelated,t)
	(*******************************************************************************************************************************)								

		fun fillInterf (~1,tab) = tab							
			| fillInterf (n,tab) = let
									val i = buscoEnTabla (n, !natToInstr)
									fun findSet (t : temp, tabla) = (case tabBusca (t,tabla) of
																	NONE => empty
																	| SOME c => c)
									val liveouts = buscoEnTabla(n,!liveOut)
									(* f inserta en la tabla la tupla (tmp, A union B)
									donde A son todos los nodos n donde ya existe la arista (tmp,n)
									B son todos los liveouts en la instrucción donde se define tmp*)									
									in case i of
											OPER {assem=a,dst=d,src=_,jump=_} => 
												if List.null(d) then (print(Int.toString(n)^" "^tigerassem.format name i^"\n");tigertab.tabPrintTempTempSet(tab);print("\n");fillInterf(n-1,tab)) else (let 
													val dSet = Splayset.addList(empty, d)
													val liveouts' = difference (liveouts,dSet)
													val _ = print(Int.toString(numItems(liveouts'))^"\n")
													fun f ((tmp, t) : (temp * interfTab)) : interfTab = (tabRInserta (tmp,union(findSet(tmp,tab),liveouts'),t))	(* tab' tiene todos las aristas que comienzan con di*)
													val tab' = List.foldl f tab d
													val g = (fn (tmp,t) => tabRInserta (tmp,Splayset.union(findSet(tmp,tab'),dSet),t)) : (temp * interfTab) -> interfTab
													val liveoutsList = Splayset.listItems liveouts'	
													val _ = print(Int.toString(numItems(dSet))^"\n")			
													val tab'' = List.foldl g tab' liveoutsList
													(*quede en el numero de nodo 20, por que no agrega a t2 el t8?*)
												in (print(Int.toString(n)^" "^tigerassem.format name i^"\n");tigertab.tabPrintTempTempSet(tab'');print("\n");fillInterf(n-1,tab'')) end)
												
											| LABEL {assem=a,lab=_} => (print(Int.toString(n)^" "^tigerassem.format name i^"\n");tigertab.tabPrintTempTempSet(tab);print("\n");fillInterf(n-1,tab))
											
											| MOVE {assem=a,dst=d,src=s} =>
												let
													val dSet = Splayset.addList(empty, [s,d])
													val liveouts' = difference (liveouts,dSet)
													(*val liveouts' = if member(liveouts,s) then delete (liveouts,s) else liveouts  todos los liveouts menos el destino y la fuente*)
													fun f' ((tmp, t) : (temp * interfTab)) : interfTab = (tabRInserta (tmp,union(findSet(tmp,tab),liveouts'),t)) 
													val tab' = f'(d,tab)		
													val g = (fn (tmp,t) => tabRInserta (tmp,Splayset.add(findSet(tmp,tab'),d),t)) : (temp * interfTab) -> interfTab	val liveoutsList = Splayset.listItems liveouts'				
													val tab'' = List.foldl g tab' liveoutsList												
												in (print(Int.toString(n)^" MOVE"^tigerassem.format name i^"\n");tigertab.tabPrintTempTempSet(tab'');print("\n");fillInterf(n-1,tab'')) end
								end		

		val interf = ref(fillInterf(longNatToInstr,tabNueva()))
		val _ = print ("\nImprimo interf\n")
		val _ = tigertab.tabPrintTempTempSet(!interf)
		(* Que pasa con el grado igual a K? 
		   Suponemos que debe estar incluído con el conjunto high*)

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
														
		val WorkSetMoves = ref (fillWorkSetMoves longNatToInstr)
		
	(* selectStack: pila que contiene los temporales eliminados del grafo *)
	val selectStack = ref ([])
	
	(* Simplify algoritmo en pagina 246 *)
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

