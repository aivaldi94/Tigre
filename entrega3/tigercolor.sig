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

(*spilledNodes: nodos marcados para hacer spill *)
val spilledNodes : tigertemp.temp list ref

(* coloredNodes: conjunto de temporales coloreados *)
val coloredNodes : tigertemp.temp Splayset.set ref

val initial : tigertemp.temp Splayset.set ref

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

val newTemps : tigertemp.temp Splayset.set ref

val findSetInt : (tigertemp.temp * (tigertemp.temp , int Splayset.set) tigertab.Tabla) -> int Splayset.set

val findSetStr : (tigertemp.temp * (tigertemp.temp , tigertemp.temp Splayset.set) tigertab.Tabla) -> tigertemp.temp Splayset.set

val printList : tigertemp.temp list -> unit

val invNodes : unit -> unit

val invMoves : unit -> unit

val invDegree : unit -> unit

val invSpill : unit -> unit

val invSimplify : unit -> unit

val invFreeze : unit -> unit

val nodeMoves : tigertemp.temp -> int Splayset.set

(* determina si un nodo en especial todavia tiene que ver con un move o no *)
val moveRelatedFun: tigertemp.temp -> bool

val enableMoves: tigertemp.temp Splayset.set -> unit

val adjacent : tigertemp.temp -> tigertemp.temp Splayset.set

val adjacent' : tigertemp.temp -> tigertemp.temp Splayset.set

val decrementDegree: tigertemp.temp Splayset.set -> unit

val getAlias: tigertemp.temp -> tigertemp.temp 

val areAdj: (tigertemp.temp * tigertemp.temp) -> bool

val ok: (tigertemp.temp * tigertemp.temp) -> bool

val conservative: tigertemp.temp Splayset.set -> bool

val addWorkList : tigertemp.temp -> unit

(*  toma un entero que representa una tigerassem.instruccion move y devuelve (src,dst) *)
(* funcion auxiliar que toma un entero que es el numero de tigerassem.instruccion y devuelve la tupla (src,dst) de un move *)
(* se usa para cuando el libro dice m (copy(x,y))*)
	
val tempsInMove: int -> (tigertemp.temp * tigertemp.temp)

val freezeMoves : tigertemp.temp -> unit 

val addEdge: (tigertemp.temp * tigertemp.temp) -> unit

val combine: (tigertemp.temp * tigertemp.temp ) -> unit

(* getDegree: funcion que dado un temporal busca su cantidad de vecinos en la tabla degree*)
val getDegree : tigertemp.temp -> int

(* fillColor: funcion que dada una lista de nodos precoloreados los completa en la tabla de color*)
val fillColor: (tigertemp.temp list * (tigertemp.temp ,tigertemp.temp) tigertab.Tabla) -> (tigertemp.temp ,tigertemp.temp) tigertab.Tabla

val simplify: unit -> unit

val coalesce: unit -> unit

val freeze: unit -> unit

val selectSpill : unit -> unit

val repeatDo : (int * int * int * int) -> unit

val repeatUntil : unit -> unit

val assignColors: (tigertemp.temp Splayset.set * tigertemp.temp list) -> tigertemp.temp Splayset.set

val its : int -> string

val forEachSpilled : (tigerassem.instr list * tigertemp.temp * int) -> (tigerassem.instr list * tigertemp.temp list)

val rewriteProgram : (tigerassem.instr list * tigerframe.frame * tigertemp.temp list) -> (tigerassem.instr list * tigertemp.temp list)

(*val deleteCoalescedMoves : tigerassem.instr list -> *)

val makeWorkList : tigertemp.temp Splayset.set -> unit

val initializeMoves : unit -> unit

val initializeNodes : unit -> unit

val initializeTables : unit -> unit

val pintar : tigertemp.temp -> tigertemp.temp

val colorear' : (tigerassem.instr list * tigerframe.frame * tigertemp.temp Splayset.set * bool) -> ((tigertemp.temp -> tigertemp.temp) * tigerassem.instr list)

val colorear : (tigerassem.instr list * tigerframe.frame * bool) -> ((tigertemp.temp -> tigertemp.temp) * tigerassem.instr list)

end
	

