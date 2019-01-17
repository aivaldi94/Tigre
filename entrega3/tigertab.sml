structure tigertab :> tigertab =
struct
open tigerassem
open Splayset
open Polyhash


type ('a, 'b) Tabla = ('a, 'b) hash_table

exception yaExiste of string
exception noExiste
exception noExisteS of string

fun tabNueva() = mkPolyTable(100, noExiste)

fun fromTab t =
	let	val t' = tabNueva()
	in	apply (fn x => insert t' x) t; t' end

fun name x = x

fun tabEsta(s, t) = 
	case Polyhash.peek t s of
	SOME _ => true
	| NONE => false


fun tabInserta(s, e, t) = let val t' = copy t in (peekInsert t' (s, e); t') end

fun tabRInserta(s, e, t) = let val t' = copy t in (insert t' (s, e); t') end

fun tabBusca(s, t) = peek t s

fun tabSaca(s, t) =
	case tabBusca(s, t) of
	SOME t => t
	| NONE => raise noExiste

fun tabAplica(f, t) = map(fn(_, e) => f e) t

fun tabAAplica(f, g, t) = 
	let	val l' = listItems t
		val t' = mkPolyTable(100, noExiste)
	in
		List.app(fn(k, e) => insert t' (k, e))
			(List.map(fn(k, e) => (f k, g e)) l');
		t'
	end

fun tabRAAplica(f, g, t) = 
	let	val l' = rev(listItems t)
		val t' = mkPolyTable(100, noExiste)
	in
		List.app(fn(k, e) => insert t' (k, e))
			(List.map(fn(k, e) => (f k, g e)) l');
		t'
	end

fun tabInserList(t, l) = 
	let val t' = copy t in (List.app(fn(s, e) => insert t' (s, e)) l; t') end

fun tabAList t = listItems t

fun tabFiltra(f, t) =
	let	val l = listItems t
		val t' = mkPolyTable(100, noExiste)
	in
		List.app(fn(k, e) => insert t' (k,e))
			(List.filter (fn(a, b) => f b) l);
		t'
	end
	
fun tabIgual1 (f,[],ls') = true
	| tabIgual1 (f : ('a * 'a -> bool),(_,l)::ls,(_,l')::ls') = if f (l,l') then tabIgual1(f,ls,ls') else false
	
fun tabIgual (f : ('a * 'a -> bool),t,t') = if (numItems t = numItems t') then tabIgual1 (f,tabAList t, tabAList t') else false
	
fun tabPrimer(f, t) = hd(List.filter (fn(a, b) => f b) (listItems t))

fun tabClaves t = List.map (fn(x, y) => x) (listItems t)


(*----------------------------------------------------------------------------------------------*)

val its = Int.toString

fun concatStrings [] = " "
	|concatStrings (x::xs) = " "^x^concatStrings(xs)
								
fun tabPrintIntInstr t = let 
							val l = tabAList t
							fun getAsmInstr i = case i of 
													tigerassem.OPER {assem=a,dst=_,src=_,jump=_} => a
													| tigerassem.LABEL {assem=a,lab=_} => a
													| tigerassem.MOVE {assem=a,dst=_,src=_} => a
							val _ = print("\nTipo : (int * instr) Tabla\n")
							val _ = List.app (fn (n,i) => print (its(n)^" -> "^getAsmInstr i^"\n")) l
						in () end


fun tabPrintIntTempSet t = let
							val l = tabAList t														
							val _ = print ("\nTipo : (int * temp Set) Tabla\n")
							val _ = List.app (fn (n,set) => print (its(n)^" -> {"^(concatStrings(Splayset.listItems set))^"}\n")) l
						 in () end
					 
fun tabPrintIntIntSet t = let
							val l = tabAList t
							val _ = print ("\nTipo:(int * int Set) Tabla\n")
							val _ = List.app (fn (n,set) => print (its(n)^" -> {"^(concatStrings(List.map its (Splayset.listItems set)))^"}\n")) l
						 in () end
						 
fun tabPrintTempTempSet t = let 
							val l = tabAList t														
							val _ = print ("\nTipo : (temp * temp Set) Tabla\n")
							val _ = List.app (fn (t,set) => print (t^" -> {"^(concatStrings(Splayset.listItems set))^"}\n")) l
						 in () end														
						 
end
