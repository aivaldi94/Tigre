signature tigerbuild =
sig
    
(* funcion identidad para temporales *)
val id : tigertemp.temp -> tigertemp.temp

(* tabla que asocia enteros (instrucciones) con instrucciones *)
val natToInstr : (int, tigerassem.instr) tigertab.Tabla ref

(* tabla que asocia cada instruccion con los tigertemp.temporales que se definen en el *)
val defs: (int, tigertemp.temp Splayset.set) tigertab.Tabla ref 

(* tabla que asocia cada instruccion con los tigertemp.temporales que se usan en el *)
val uses: (int, tigertemp.temp Splayset.set) tigertab.Tabla ref 

(* tabla que asocia instruccion con sus sucesores *)
val succs: (int,int Splayset.set) tigertab.Tabla ref

(* tabla que asocia instruccion con tigertemp.temporales liveOut *)
val liveOut : (int, tigertemp.temp Splayset.set) tigertab.Tabla ref

(* tabla que asocia instruccion con tigertemp.temporales liveIn *)
val liveIn : (int, tigertemp.temp Splayset.set) tigertab.Tabla ref

(* tabla de interferencias *)
val interf : (tigertemp.temp, tigertemp.temp Splayset.set) tigertab.Tabla ref

(* tabla de interferencias donde no estan los nodos precoloreados como claves *)
val interfNoPrec : (tigertemp.temp, tigertemp.temp Splayset.set) tigertab.Tabla ref

(* conjunto de temps relacionados con moves *)
val moveRelated : tigertemp.temp Splayset.set ref

(* contiene los numeros de instruccion que son moves - no agrega aquellas instrucciones
   que son moves entre precoloreados *)
val workSetMoves: int Splayset.set ref

(* tabla que asocia a cada temporal con el conjunto de instrucciones moves en donde aparece *)
val moveSet: (tigertemp.temp, int Splayset.set) tigertab.Tabla ref

(* es equivalente a workSetMoves pero no se modificara durante la ejecucion del algortimo *)
val allMoves : int Splayset.set ref

(* Lista y conjuntos de precoloreados*)
val precoloredList : string list ref
val precoloredSet : string Splayset.set ref

(*dado un temporal, devuelve true si pertenece al conjunto de moveRelated*)
val isMoveRelated : tigertemp.temp -> bool

(*funcion unificadora. construye "grafo" de flujo y "grafo" de interferencia *)
val build : (tigerassem.instr list * bool) -> unit

end
