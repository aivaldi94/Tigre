structure tigerflow :> tigerflow =
struct
	open tigerassem
	open tigertemp
	open tigertab
	open Splayset

	val K = 3
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
															val in' = tabRInserta (0,b,inNueva)
															
															val b1 = buscoEnTabla (0,outVieja)
															val out' = tabRInserta (0,b1,outNueva)													
															
															val useN = buscoEnTabla (0,!uses)										
															val outN = buscoEnTabla (0,outVieja)							
															
															val defN = buscoEnTabla (0,!defs)
																																			
															val in'' = tabRInserta(0,union(useN,difference(outN,defN)),inNueva)
															val succsN = listItems (buscoEnTablaInt (0,!succs))
															fun index n = listItems (buscoEnTabla (0,in''))
															val m = Splayset.addList(empty,List.concat (List.map index succsN))
															val out'' = tabRInserta (0,m,in'')
														in
															(out',out'',in',in'')
														end
		| forEachN (n,outNueva,outVieja,inNueva,inVieja) = let
															fun buscoEnTabla (x,t) = (case (tabBusca (x,t)) of 
																						NONE => empty
																						| SOME v => v)
															fun buscoEnTablaInt (x,t) = (case (tabBusca (x,t)) of 
																						NONE => Splayset.empty Int.compare
																						| SOME v => v)
															val b = buscoEnTabla (n,inVieja)
															val in' = tabRInserta (n,b,inNueva)
															
															val b1 = buscoEnTabla (n,outVieja)
															val out' = tabRInserta (n,b1,outNueva)
															
															val useN = buscoEnTabla (n,!uses)										
															val outN = buscoEnTabla (n,outVieja)							
															
															val defN = buscoEnTabla (n,!defs)
																																			
															val in'' = tabRInserta(n,union(useN,difference(outN,defN)),inNueva)
															val succsN = listItems (buscoEnTablaInt (n,!succs))
															fun index n = listItems (buscoEnTabla (n,in''))
															val m = Splayset.addList(empty,List.concat (List.map index succsN))
															val out'' = tabRInserta (n,m,in'')
														in
															forEachN (n-1, out',out'',in',in'')
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
			
		(*******************************************************************************************************************************)
		(* adj = interf ?*)
		
		val interf = ref (tabNueva())
		val adj = ref (tabNueva())
		(*val adj : (tigertemp.temp, tigertemp.temp list) tigertab.Tabla ref = ref (tabNueva ())*)
		
		fun getAdj t = case (tabBusca (t,!adj)) of
							NONE => raise Fail "No deberia pasar (temp no encontrado)"
							| SOME l => l
							
		(*
		Esta funciones de cuando adj tenia tipo (temp,temp list) tabla
		fun areAdj (t1,t2) = case (tabBusca (t1,!adj)) of
								NONE => raise Fail "No deberia pasar (temp no encontrado)"
								| SOME l => List.null (List.filter (fn e => ((e <= t2) andalso (e >= t2))) l)
		*)
		(*
		fun areAdj (t1,t2) = case (tabBusca (t1,!adj)) of
							NONE => raise Fail "No deberia pasar (temp no encontrado)"
							| SOME l => List.null (List.filter (fn e => ((e <= t2) andalso (e >= t2))) (Splayset.listItems l))
	*)
		
		fun getDegree t = Splayset.numItems (getAdj t)
		
		val degree = ref (tabAAplica (id,Splayset.numItems,!adj)											)
		

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
									val i = buscoEnTabla (0, !natToInstr)
								in case i of
									OPER {assem=_,dst=_,src=_,jump=_} => fillMoveRelated (n-1)
									| LABEL {assem=_,lab=_} => fillMoveRelated (n-1)
									| MOVE {assem=_,dst=d,src=s} => add (add (fillMoveRelated (n-1),s),d) 
								end																		
														
		val moveRelated = ref (fillMoveRelated longNatToInstr)
		
		fun isMoveRelated t = member (!moveRelated,t)
	(*******************************************************************************************************************************)								
		fun fillInterf (0,tab) = let
									val i = buscoEnTabla (0, !natToInstr)
									fun findSet (t : temp) = (case tabBusca (t,tab) of
																	NONE => empty
																	| SOME c => c)
									val liveouts = buscoEnTabla(0,!liveOut)
									(* f inserta en la tabla la tupla (tmp, A union B)
									donde A son todos los nodos n donde ya existe la arista (tmp,n)
									B son todos los liveouts en la instrucción donde se define tmp*)
									fun f ((tmp, t) : (temp * interfTab)) : interfTab = (tabInserta (tmp,union(findSet(tmp),liveouts),t)) 
									in case i of
											OPER {assem=_,dst=d,src=_,jump=_} => 
												let 
													val g = (fn (tmp,t) => tabInserta (tmp,Splayset.addList(findSet(tmp),d),t)) : (temp * interfTab) -> interfTab
													(* tab' tiene todos las aristas que comienzan con di*)
													val tab' = List.foldl f tab d
													val liveoutsList = Splayset.listItems liveouts				
													val tab'' = List.foldl g tab' liveoutsList
												in tab'' end
											| LABEL {assem=_,lab=_} => tab
											| MOVE {assem=_,dst=d,src=_} => 
												let
													val g = (fn (tmp,t) => tabInserta (tmp,Splayset.add(findSet(tmp),d),t)) : (temp * interfTab) -> interfTab
													val liveouts' =  if member(liveouts,d) then delete (liveouts,d) else liveouts (* todos los liveouts menos el destino*)
													val tab' = f(d,tab)
													val liveoutsList = Splayset.listItems liveouts'				
													val tab'' = List.foldl g tab' liveoutsList
												in tab'' end
								end								
			| fillInterf (n,tab) = let
									val i = buscoEnTabla (n, !natToInstr)
									fun findSet (t : temp) = (case tabBusca (t,tab) of
																	NONE => empty
																	| SOME c => c)
									val liveouts = buscoEnTabla(n,!liveOut)
									(* f inserta en la tabla la tupla (tmp, A union B)
									donde A son todos los nodos n donde ya existe la arista (tmp,n)
									B son todos los liveouts en la instrucción donde se define tmp*)
									fun f ((tmp, t) : (temp * interfTab)) : interfTab = (tabInserta (tmp,union(findSet(tmp),liveouts),t)) 
									in case i of
											OPER {assem=_,dst=d,src=_,jump=_} => 
												let 
													val g = (fn (tmp,t) => tabInserta (tmp,Splayset.addList(findSet(tmp),d),t)) : (temp * interfTab) -> interfTab
													(* tab' tiene todos las aristas que comienzan con di*)
													val tab' = List.foldl f tab d
													val liveoutsList = Splayset.listItems liveouts				
													val tab'' = List.foldl g tab' liveoutsList
												in fillInterf(n-1,tab'') end
											| LABEL {assem=_,lab=_} => fillInterf(n-1,tab)
											| MOVE {assem=_,dst=d,src=_} =>
												let
													val g = (fn (tmp,t) => tabInserta (tmp,Splayset.add(findSet(tmp),d),t)) : (temp * interfTab) -> interfTab
													val liveouts' = if member(liveouts,d) then delete (liveouts,d) else liveouts (* todos los liveouts menos el destino*)
													val tab' = f(d,tab)
													val liveoutsList = Splayset.listItems liveouts'				
													val tab'' = List.foldl g tab' liveoutsList
												in fillInterf(n-1,tab'') end
								end		
		
		val interf = ref(fillInterf(longNatToInstr,tabNueva()))
		(* Que pasa con el grado igual a K? 
		   Suponemos que debe estar incluído con el conjunto high*)

	(* simplifyWorklist: nodos no relacionados con move y de grado menor a K *)

		fun getSimplifyList (tDegree, tMoveRel) = let
													val lowDegreeList = tabClaves (tabFiltra ((fn n => if n < K then true else false),!tDegree))
													val nonMoveRelList = tabClaves (tabFiltra ((fn n => if n = false then true else false),!tMoveRel))
												  in addList (empty,(lowDegreeList @ nonMoveRelList)) end

	(* freezeWorklist: nodos relacionados con move y de grado menor a K *)

	   fun getFreezeList (tDegree, tMoveRel) = let 
													val lowDegreeList = tabClaves (tabFiltra ((fn n => if n < K then true else false),!tDegree))
													val moveRelList = tabClaves (tabFiltra ((fn n => if n = false then false else true),!tMoveRel))
												  in addList (empty,(lowDegreeList @ moveRelList)) end
												
	(* spillWorklist: nodos con grado mayor a K *)
												
		val spillWorkList = addList (empty,tabClaves (tabFiltra ((fn n => if n > K then true else false),!degree)))						
												  
	(* Hacer lista worklistMoves: moves de temp a temp que pueden eliminarse (o sea que dst y src no tienen que estar unidos en interf).*)

	in print("ok\n") end	 

end
