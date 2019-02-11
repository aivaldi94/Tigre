signature tigercolor =
sig
(*
val getSimplifyList : (tigertemp.temp, int) tigertab.Tabla ref * (tigertemp.temp, bool) tigertab.Tabla ref -> tigertemp.temp Splayset.set

val getFreezeList : (tigertemp.temp, int) tigertab.Tabla ref * (tigertemp.temp, bool) tigertab.Tabla ref -> tigertemp.temp Splayset.set

val spillWorkList : tigertemp.temp Splayset.set

val areAdj : tigertemp.temp * tigertemp.temp -> bool

val getDegree : tigertemp.temp -> int

val fillMoveRelated : int ->  string Splayset.set 

val isMoveRelated : tigertemp.temp -> bool

val fillInterf : int * (tigertemp.temp, tigertemp.temp Splayset.set) tigertab.Tabla -> (tigertemp.temp, tigertemp.temp Splayset.set) tigertab.Tabla

val worklistMoves : int Splayset.set

*)

	
	(*
		fun areAdj (t1,t2) = case (tabBusca (t1,!interf)) of
								NONE => raise Fail "No deberia pasar (temp no encontrado)"
								| SOME l => List.null (List.filter (fn e => ((e <= t2) andalso (e >= t2))) (Splayset.listItems l))	
		
		val setOfAllTemps = addList (empty, tabClaves (!degree))
		simplifyWorklist: tigertemp.temp Splayset.set - nodos no relacionados con move y de grado menor a K *)
			
		
		(* freezeWorklist: tigertemp.temp Splayset.set - nodos relacionados con move y de grado menor a K *)
(*
		fun fillFreezeWorkSet (tDegree, tMoveRel) = let 
														val lowDegreeList = tabClaves (tabFiltra ((fn n => if n < K then true else false),!tDegree))
														val moveRelSet = !tMoveRel
													  in addList (moveRelSet,lowDegreeList) end
													  
		val freezeWorkSet = ref (fillFreezeWorkSet (degree, moveRelated)) 	*)									
		
													
		
													  
		(* Hacer lista worklistMoves: moves de temp a temp que pueden eliminarse (o sea que dst y src no tienen que estar unidos en interf).*)
		(* me conviene que esto sea un conjunto de tuplas? o sea si (a,b) pertenece a este conjunto quiere decir que a y b pueden unirse
			en un solo nodo porque a y b no estan unidos en interf *)
		(*fun fillWorkSetMoves 0 = let
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
		
		(*
		(*moveList: tabla que asocia a cada temp con el conjunto de temps mediante los cuales esta relaiconado con un move*)
		fun fillMoveList 0 = empty
		val moveList = ref (fillMoveList (!longNatToInstr))
		(*funcion noveMoves*)
		fun nodeMoves n = intersection(buscoEnTabla(n,!moveList),union(activeMoves,WorkSetMoves))
		(* Simplify algoritmo en pagina 246 *)
		*)
val degree : (tigertemp.temp , int) tigertab.Tabla ref		
val color : (tigertemp.temp ,tigertemp.temp) tigertab.Tabla ref
val colorear : (tigerassem.instr list * int) -> (tigertemp.temp -> tigertemp.temp)

end
	
