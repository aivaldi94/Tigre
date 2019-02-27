signature tigerbuild =
sig

(*

val fillNatToInstr : tigerassem.instr list * int -> (int, tigerassem.instr) tigertab.Tabla

val fillDefs : tigerassem.instr -> tigertemp.temp Splayset.set

val fillUses : tigerassem.instr -> tigertemp.temp Splayset.set

(* findLabel retorna los nodos del natToInstr que tienen como etiqueta a l *)

val findLabel : tigertemp.label -> int list

(*instrList lista de instrucciones de natToInstr*)

val instrList  : tigerassem.instr list ref

val fillSuccs : tigerassem.instr list * int -> (int, int Splayset.set) tigertab.Tabla

type interfTab = (tigertemp.temp, tigertemp.temp Splayset.set) tigertab.Tabla

val getTemps : tigerassem.instr list * tigertemp.temp Splayset.set -> tigertemp.temp Splayset.set

*)
(* funcion identidad para temporales *)
val id : tigertemp.temp -> tigertemp.temp

(* tabla que asocia enteros (nodos) con instrucciones *)
val natToInstr : (int, tigerassem.instr) tigertab.Tabla ref

(* tabla que asocia cada nodo con los tigertemp.temporales que se definen en el *)
val defs: (int, tigertemp.temp Splayset.set) tigertab.Tabla ref 

(* tabla que asocia cada nodo con los tigertemp.temporales que se usan en el *)
val uses: (int, tigertemp.temp Splayset.set) tigertab.Tabla ref 

(* tabla que asocia nodos con sus sucesores *)
val succs: (int,int Splayset.set) tigertab.Tabla ref

(* tabla que asocia nodos con tigertemp.temporales liveOut *)
val liveOut : (int, tigertemp.temp Splayset.set) tigertab.Tabla ref

(* tabla que asocia nodos con tigertemp.temporales liveIn *)
val liveIn : (int, tigertemp.temp Splayset.set) tigertab.Tabla ref

(* tabla de interferencias *)
val interf : (tigertemp.temp, tigertemp.temp Splayset.set) tigertab.Tabla ref

(* tabla de interferencias donde no estan los nodos precoloreados como claves *)
val interfNoPrec : (tigertemp.temp, tigertemp.temp Splayset.set) tigertab.Tabla ref

(* conjunto de temps relacionados con moves *)
val moveRelated : tigertemp.temp Splayset.set ref
val workSetMoves: int Splayset.set ref
val moveSet: (tigertemp.temp, int Splayset.set) tigertab.Tabla ref
val allMoves : int Splayset.set ref

(*dado un temporal, devuelve true si pertenece al conjunto de moveRelated*)
val isMoveRelated : tigertemp.temp -> bool

(* Lista y conjuntos de precoloreados*)
val precoloredList : string list ref
val precoloredSet : string Splayset.set ref
(*funcion unificadora. construye "grafo" de flujo y "grafo" de interferencia *)
val build : (tigerassem.instr list * int) -> unit

end
