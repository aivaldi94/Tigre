signature tigercolor =
sig

(* selectStack: pila que contiene los temporales eliminados del grafo *)		
val selectStack : tigertemp.temp list ref		

(* spillWorkSet: nodos con grado mayor o igual a K *)
val spillWorkSet : tigertemp.temp Splayset.set ref

(* nodos no relacionados con move y de grado menor a K *)
val simplifyWorkSet : tigertemp.temp Splayset.set ref

(* degree: tabla que asocia a cada nodo con la cantidad de vecinos que posee *)
val degree : (tigertemp.temp,int) tigertab.Tabla ref		

(* color: tabla que asocia a cada nodo con su color elegido por el algoritmo *)
val color : (tigertemp.temp ,tigertemp.temp) tigertab.Tabla ref


(* precoloredList: lista de nodos precoloreados*)
val precoloredList : tigertemp.temp list ref

(* precoloredSet: conjunto de nodos precoloreados*)
val precoloredSet : tigertemp.temp Splayset.set ref

(*spilledNodes: nodos marcados para hacer spill *)
val spilledNodes : tigertemp.temp list ref


(* alias: cuando un move (a,b) se une, v se pone en coalescedNodes y alias(v) = u *)
val alias : (tigertemp.temp,tigertemp.temp) tigertab.Tabla ref

(* temporales que han sido unidos, si tengo move (v,u) entonces v se une a este conjunto y u es puesto en alguna work list o viceversa *) 
val coalescedNodes: tigertemp.temp Splayset.set ref

(* moves que han sido unidos *)
val coalescedMoves : int Splayset.set ref

(* moves cuya fuente y destino interfieren *)
val constrainedMoves : int Splayset.set ref

(* temporales relacionados con moves y con grado menor a k *)
val freezeWorkSet : tigertemp.temp Splayset.set ref

(* moves que todavia no estan listos para ser unidos *)
val activeMoves : int Splayset.set ref	

(* moves que no van a ser considerasdos para coalescing nunca mas*)
val frozenMoves : int Splayset.set ref	

(* conjunto de todos los temporales existentes*)
val setOfAllTemps : tigertemp.temp Splayset.set ref

(*registersSet: conjunto de registros de la arquitectura (son los registers de frame pero pasados a conjunto) *)
val registersSet : tigertemp.temp Splayset.set ref

val nodeMoves : tigertemp.temp -> int Splayset.set

val enableMoves: tigertemp.temp Splayset.set -> unit

val adjacent : tigertemp.temp -> tigertemp.temp Splayset.set

val getAlias: tigertemp.temp -> tigertemp.temp 

(*nuestro moveRelated es un conjunto donde estan todos los nodos involucrados en un move, aca determina si un nodo en especial todavia tiene que ver con un move o no*)
val moveRelatedFun: tigertemp.temp -> bool

val areAdj: (tigertemp.temp * tigertemp.temp) -> bool

val ok: (tigertemp.temp * tigertemp.temp) -> bool

(*val fillFreezeWorkSet: unit-> tigertemp.temp Splayset.set*)

val conservative: tigertemp.temp Splayset.set -> bool

(*  toma un entero que representa una instruccion move y devuelve (src,dst) *)
	(* funcion auxiliar que toma un entero que es el numero de instruccion y devuelve la tupla (src,dst) de un move *)
	(* se usa para cuando el libro dice m (copy(x,y))*)
	
val tempsInMove: int -> (tigertemp.temp * tigertemp.temp)

val freezeMoves : tigertemp.temp -> unit 

val addEdge: (tigertemp.temp * tigertemp.temp) -> unit

val combine: (tigertemp.temp * tigertemp.temp ) -> unit



val coalesce: unit -> unit

val freeze: unit -> unit


(* getDegree: funcion que dado un temporal busca su cantidad de vecinos en la tabla degree*)
val getDegree : tigertemp.temp -> int

(* fillSimplifyWorkSet: funcion que rellena el conjunto simplifyWorkSet *)
(*val fillSimplifyWorkSet : unit -> tigertemp.temp Splayset.set*)

(* decrementDegree: dado un conjunto de temporales hace lo que pide el libro pagina 246*)
val decrementDegree: tigertemp.temp Splayset.set -> unit

(* simplify: libro pagina 246*)
val simplify: unit -> unit

(* fillColor: funcion que dada una lista de nodos precoloreados los completa en la tabla de color*)
val fillColor: (tigertemp.temp list * (tigertemp.temp ,tigertemp.temp) tigertab.Tabla) -> (tigertemp.temp ,tigertemp.temp) tigertab.Tabla

(*  *)
val selectSpill : unit -> unit

(* repeatDo: funcion que implementa la parte de repeat until de la pagina 244*)
val repeatDo : (int * int * int * int) -> unit

(* repeatUntil: funcion que implementa la parte de repeat until de la pagina 244*)
val repeatUntil : unit -> unit

(* assignColors: funcion de la pagina 249 *)
val assignColors: (tigertemp.temp Splayset.set * tigertemp.temp list) -> tigertemp.temp Splayset.set

(* colorear: funcion que junta todas las partes y hace el coloreo *)
val colorear : (tigerassem.instr list * tigerframe.frame * int) -> ((tigertemp.temp -> tigertemp.temp) * tigerassem.instr list)

end
	
	
	(*
		fun areAdj (t1,t2) = case (tabBusca (t1,!interf)) of
								NONE => raise Fail "No deberia pasar (temp no encontrado)"
								| SOME l => List.null (List.filter (fn e => ((e <= t2) andalso (e >= t2))) (Splayset.listItems l))	
		
		val setOfAllTemps = addList (empty, tabClaves (!degree))
		simplifyWorklist: tigertemp.temp Splayset.set -  *)
			
		
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
