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
val colorear : unit -> unit

end
	
