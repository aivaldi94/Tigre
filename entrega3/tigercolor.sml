structure tigercolor :> tigercolor =
struct
	open tigerassem
	open tigertemp
	open tigertab
	open Splayset
	open tigerbuild
	open tigerframe

	val emptyStr = empty String.compare														
	val emptyInt = empty Int.compare	
	
	val selectStack = ref ([])	
	val spillWorkSet = ref (emptyStr)
	val simplifyWorkSet = ref(emptyStr)
	val degree = ref (tabNueva())
	val color = ref (tabNueva())
	val precoloredList = ref([])
	val precoloredSet = ref(emptyStr)
	val spilledNodes = ref ([])
	val registersSet = ref(emptyStr)
	
	val alias = ref (tabNueva()) 
	val coalescedNodes = ref(emptyStr) 
	val coalescedMoves = ref(emptyInt) 
	val constrainedMoves = ref(emptyInt) 
	val freezeWorkSet = ref (emptyStr) 
	val activeMoves = ref(emptyInt) 
	val frozenMoves = ref(emptyInt) 
	val setOfAllTemps = ref(emptyStr) 
	

	fun findSet (t : temp, tabla) = (case tabBusca (t,tabla) of
											NONE => emptyInt
											| SOME c => c)
											
    fun nodeMoves n = intersection (findSet(n,!moveSet), union(!activeMoves,!workSetMoves))

    fun enableMoves nodes = let 
								val nList = listItems(nodes)
								val l1 = map nodeMoves nList (* (int Splayset.set ) list*)
								val l2 = map (fn n => intersection (n,!activeMoves)) l1 
							in List.app (fn n => (activeMoves := difference(!activeMoves,n); workSetMoves := union (!workSetMoves, n))) l2 end
							
	 fun adjacent n = difference(buscoEnTabla (n,!interf),union(addList(emptyStr,!selectStack),!coalescedNodes))
	 
	fun minusOneSet s x = x-1 
														
	fun decrementDegree (s) = let 								
								(* paso a lista el conjunto de temporales*)
								val listTemps = listItems s
								(* me quedo con los temps de la lista cuyo grado es K *)									
								
								val listKNeig = List.filter (fn n => (buscoEnTabla(n,!degree)) = K) listTemps
								(* a cada temp de la lista original le resto un vecino *)
								
								fun minusOne n = case tabBusca(n,!degree) of
													NONE => raise Fail "No deberia pasar minusOne"
													| SOME i => i-1
								
								fun f (tmp, t) = tabRInserta (tmp,minusOne tmp,t) 					
								val _ = degree := List.foldl f (!degree) listTemps
								
								(*elimino del conjunto spillWorkSet los elementos de listKNeig*)
								val setKNeig = addList(emptyStr,listKNeig)
								
								val activarMoves = List.foldl (fn (n,set) =>union(adjacent(n),set)) setKNeig listKNeig
								val _ = enableMoves (activarMoves)
								
								val _ = spillWorkSet := difference (!spillWorkSet,setKNeig)
								
								val adjMoveRelated = intersection (!moveRelated, setKNeig)
								val adjNoMoveRelated = difference (setKNeig, adjMoveRelated)
								
								val _ = freezeWorkSet := union(!freezeWorkSet,adjMoveRelated)
								val _ = simplifyWorkSet := union (!simplifyWorkSet,adjNoMoveRelated)															
								
								in () end 
	
    fun getAlias (t) = if member(!coalescedNodes,t) then getAlias(buscoEnTabla(t,!alias)) else t        
            
    fun moveRelatedFun n = equal(nodeMoves(n),emptyInt) 
        
    
    fun areAdj (t1,t2) = member(buscoEnTabla(t1,!interf),t2)
    
    fun ok (t,r) = (buscoEnTabla(t,!degree) < K) orelse member(!precoloredSet,t) orelse areAdj (t,r)
    
    fun fillFreezeWorkSet () = let 
								val lowDegreeList = tabClaves (tabFiltra ((fn n => n < K ),!degree))								
							   in addList (!moveRelated,lowDegreeList) end
							   
	fun conservative nodes = length(List.filter (fn n => (buscoEnTabla(n,!degree) >= K)) (listItems nodes)) < K
	
	fun addWorkList u = let 
							val c1 = not (member (!precoloredSet,u))
							val c2 = not ((member (!moveRelated, u)) andalso (buscoEnTabla(u,!degree) < K))
							val cond = c1 andalso c2
							val uSet = add(emptyStr,u)
						in (if cond then (freezeWorkSet := difference(!freezeWorkSet,uSet) ; simplifyWorkSet := union(!simplifyWorkSet,uSet)) else ()) end	
	
	
	fun tempsInMove n = case buscoEnTabla (n,!natToInstr) of
							MOVE {assem=_,dst=d,src=s} => (s,d)
							| _ => raise Fail "tempsInMove no deberia pasar"

	fun freezeMoves u = let
							fun aux n = let
											val nSet = add(emptyInt,n)
											val _ = print ("freezeMoves")
											val (x,y) = tempsInMove n (*NO SABEMOS ORDEN CORRECTO *)
											val v = if getAlias(y) = getAlias u then getAlias(x) else getAlias(y)
											val vSet = add(emptyStr,v)
											val _ = activeMoves := difference(!activeMoves,nSet)
											val _ = frozenMoves := union (!frozenMoves,nSet)
											val cond = equal(nodeMoves(v),emptyInt) andalso (buscoEnTabla(v,!degree) < K)
										    val _ =  if cond then (freezeWorkSet := difference(!freezeWorkSet,vSet);
											                      simplifyWorkSet := union(!simplifyWorkSet, vSet)) else ()
									    in () end											
							val _ = Splayset.app aux (nodeMoves(u))
						in () end
						  										
    
	fun addEdge (u,v) = let
							val vSet = Splayset.singleton String.compare v 
							val uSet = Splayset.singleton String.compare u
						in (if u = v then () else (interf := tabRInserta(u,union(buscoEnTabla(v,!interf),vSet),!interf);
												  interf := tabRInserta(v,union(buscoEnTabla(u,!interf),uSet),!interf);
						                          degree := tabRInserta(u,buscoEnTabla(u,!degree)+1,!degree);
						                          degree := tabRInserta(v,buscoEnTabla(v,!degree)+1,!degree))) end
				
	fun combine (u,v) = let 
							val vSet = Splayset.singleton String.compare v
							val uSet = Splayset.singleton String.compare u
							val _ = if member (!freezeWorkSet, v) then freezeWorkSet := difference (!freezeWorkSet, vSet) else spillWorkSet := difference (!spillWorkSet,vSet)
							val _ = coalescedNodes := union (!coalescedNodes,vSet)
							val _ = alias := tabRInserta (v,u,!alias)
							val _ = moveSet := tabRInserta(u,union(buscoEnTabla(u,!moveSet),buscoEnTabla(v,!moveSet)),!moveSet)
							val adj = adjacent (v)
							val _ = Splayset.app (fn t => addEdge(t,u)) adj
							val _ = decrementDegree(adj)
							val cond = (buscoEnTabla(u,!degree) >= K) andalso member(!freezeWorkSet,u)
							val _ = if cond then (freezeWorkSet := delete (!freezeWorkSet,u);
												 spillWorkSet := union(!spillWorkSet,uSet)) else ()
						in () end 	
												  
    
    
	fun getDegree t = buscoEnTabla (t,!degree)
		
	fun fillSimplifyWorkSet () = let
									val lowDegreeList = tabClaves (tabFiltra ((fn n => n < K ),(!degree)))									
									val lowDegreeSet = addList(emptyStr,lowDegreeList)
									val nonMoveRelSet = difference (!setOfAllTemps, !moveRelated)
									(* Agregado para COALESCE *)
								 in intersection (lowDegreeSet,nonMoveRelSet) end								 
								(* sin coalesce in addList(emptyStr,lowDegreeList) end *)													  								
								

	fun fillColor ([],c) = c
	  | fillColor ((x::xs),c) = tabRInserta(x,x,(fillColor (xs,c)))
	  														
	
	fun simplify () = case (numItems(!simplifyWorkSet)) of 
								0 => repeatUntil()
								| _ => (let val n = hd(listItems (!simplifyWorkSet))
											val _ = simplifyWorkSet := delete(!simplifyWorkSet,n)
											val _ = selectStack := !selectStack @ [n]															
											val adjN = adjacent n
											val _ = decrementDegree (adjN)											
											in  simplify () end)
	and coalesce ()	= let 
						val m = hd (listItems(!workSetMoves))
						val mSet = Splayset.singleton Int.compare m
						val _ = print("coalsece")
						val (x',y') = tempsInMove m (*NO SABEMOS ORDEN CORRECTO *)
						val x = buscoEnTabla(x',!alias)
						val y = buscoEnTabla(y',!alias)
						val (u,v) = if member(!precoloredSet,y) then (y,x) else (x,y)
						val _ = workSetMoves := delete (!workSetMoves,m)
						val cond1 = member(!precoloredSet,v) orelse areAdj(u,v)
						val adjV = adjacent(v)
						val adjU = adjacent(u)
						val allAreOk = List.foldl (fn (b1,b2) => b1 andalso b2) true (List.map (fn t => ok(t,u)) (listItems adjV))
						val uIsMember = member(!precoloredSet,u)
						val cond2 = (uIsMember andalso allAreOk) orelse ((not uIsMember) andalso conservative(union(adjV,adjU)))
						val _ = if (u = v) then (coalescedMoves := union(!coalescedMoves,mSet);
												 addWorkList(u)) else (if cond1 then (constrainedMoves := union(!constrainedMoves,mSet);
																					 addWorkList(u); addWorkList(v)) else (if cond2 then (coalescedMoves := union (!coalescedMoves,mSet);
																																		  combine(u,v); addWorkList(u)) else activeMoves := union(!activeMoves,mSet)))						
					  in repeatUntil() end
    
    and freeze () = let
						val u = hd (listItems(!freezeWorkSet))
						val uSet = add(emptyStr,u)
						val _ = freezeWorkSet := difference(!freezeWorkSet, uSet)
						val _ = simplifyWorkSet := union(!simplifyWorkSet,uSet)
						val _ = freezeMoves(u)
					 in repeatUntil() end
  												
	and selectSpill () = let
							val m = hd(listItems (!spillWorkSet))
							val _ = spillWorkSet := difference (!spillWorkSet,add(emptyStr,m))
							val _ = simplifyWorkSet := union (!simplifyWorkSet,add(emptyStr,m))
							val _ = freezeMoves(m)
						in repeatUntil() end

	and repeatDo (lengthSimplify,lengthCoalesce,lengthFreeze,lengthSelectSpill) = 
		if (lengthSimplify <> 0) then 
			simplify() else (if (lengthCoalesce <> 0) then
								coalesce () else (if ( lengthFreeze <> 0) then
													freeze() else (if (lengthSelectSpill <> 0) then
																	selectSpill () else raise Fail "No deberia pasar (repeatDo)")))
					   
	and repeatUntil () = let
							val lengthSimplify = numItems (!simplifyWorkSet)
							val lengthCoalesce = numItems(!workSetMoves)
							val lengthFreeze = numItems (!freezeWorkSet)
							val lengthSelectSpill = numItems(!spillWorkSet)							 
							val fin = ((lengthSimplify = 0) andalso (lengthCoalesce = 0) andalso (lengthFreeze = 0) andalso (lengthSelectSpill = 0))							
						  in if fin then () else repeatDo (lengthSimplify,lengthCoalesce,lengthFreeze,lengthSelectSpill) end
						       					  									
													
	fun assignColors (cNodes, stack) = case (length (stack)) of
						
								0 => (print("stack:\n");List.app (fn n => print (n^"\n")) (!selectStack);selectStack:=[];print ("Tabla colores\n");tigertab.tabPrintTempTemp(!color);cNodes)
								| _ => case (member(!precoloredSet,hd (stack))) of
									false =>
										(let 
											val n = hd (stack)
											val stack' = tl(stack)
											val adj = buscoEnTabla (n,!interf) : tigertemp.temp Splayset.set
											val uni = union (cNodes, !precoloredSet) : tigertemp.temp Splayset.set
											(*val _ = print "\nuni es\n"
											val _ = List.app print (listItems uni)*)
											fun discardColors (n : tigertemp.temp,s) = let 
																					val isMember = member (uni,n)
																					val colorUsed = if isMember then add(emptyStr,buscoEnTabla(n,!color)) else emptyStr
																					(*val _ = print ("Descarto\n")
																					val _ =  List.app print (listItems colorUsed)*)
																				in difference (s,colorUsed) end
											val okColors = Splayset.foldl discardColors (!registersSet) adj
											(*val _ = print "\nokColors es\n"
											val _ = List.app print (listItems okColors)*)
											val cNodes' = case length (listItems(okColors)) of
														0 => (let val _ = print ("\nEL NODO SPILL ES "^n^"\n")
																val _ = spilledNodes := n::(!spilledNodes)
															 in cNodes end)
														| _ => (let 
																	val c = hd(listItems(okColors))
																	val _ = color := tabRInserta (n,c,!color)					
																in union (cNodes, add(emptyStr, n)) end)
										in assignColors (cNodes', stack') end)
									| true => assignColors (cNodes, tl(stack))											

	(* Tomará la lista de instrucciones, un temporal, un offset*)

	fun its n =  if n<0 then "-" ^ Int.toString(~n) else Int.toString(n) 

	fun forEachSpilled ([] : instr list, tmp : tigertemp.temp, offset : int) = ([],[]): (instr list * tigertemp.temp list)
		| forEachSpilled (i::instr, tmp, offset) = 
			case i of
			OPER {assem=a,dst=d,src=s,jump=j} => 
				(let	
					fun igual n = (n=tmp)
					val isDst = List.exists igual d
					val isSrc = List.exists igual s
				 in (case (isDst andalso isSrc) of
					true => let val _ = print "Es fuente y destino"
								val newTemp = newtemp()
								val newInstr1 = OPER {assem="movq "^its(offset)^"(%'s0), %'d0\n",dst=[newTemp],src=[fp],jump=NONE}
								val d' = map (fn n => if n = tmp then newTemp else n) d
								val s' = map (fn n => if n = tmp then newTemp else n) s
								val rewInstr = OPER {assem=a,dst=d',src=s',jump=j}
								val newInstr2 = OPER {assem="movq %'s0, "^its(offset)^"(%'s1)\n",dst=[],src=[fp,newTemp],jump=NONE}
								val (instructions, temps) = forEachSpilled(instr,tmp,offset)
							in ([newInstr1,rewInstr,newInstr2]@instructions, newTemp::temps)end
					| false => let val newTemp = newtemp()
								in (case isDst of
									true => let val _ = print "Es destino"
												val d' = map (fn n => if n = tmp then newTemp else n) d
												val rewInstr = OPER {assem=a,dst=d',src=s,jump=j}
												val newInstr = OPER {assem="movq %'s0, "^its(offset)^"(%'s1)\n",dst=[],src=[newTemp,fp],jump=NONE}
												val (instructions, temps) = forEachSpilled(instr,tmp,offset)
											in ([rewInstr,newInstr]@instructions, newTemp::temps)end
									| false => (case isSrc of
										true => let 
													val s' = map (fn n => if n = tmp then newTemp else n) s
													val newInstr = OPER {assem="movq "^its(offset)^"(%'s0), %'d0\n",dst=[newTemp],src=[fp],jump=NONE}
													val rewInstr = OPER {assem=a,dst=d,src=s',jump=j}
													val (instructions, temps) = forEachSpilled(instr,tmp,offset)
												in ([newInstr,rewInstr]@instructions, newTemp::temps)end
										| false => let val (instructions, temps) = forEachSpilled(instr,tmp,offset)
													in (i::instructions,temps) end))end)end)
			 | MOVE {assem=a,dst=d,src=s} => 
				(let	val isDst = (d = tmp)
						val isSrc = (s = tmp)
				 in (case (isDst andalso isSrc) of
					true => forEachSpilled(instr,tmp,offset)
					| false => let val newTemp = newtemp()
								in (case isDst of
									true => let val rewInstr = MOVE {assem=a,dst=newTemp,src=s}
												val newInstr = OPER {assem="movq %'s0, "^its(offset)^"(%'s1)\n",dst=[],src=[newTemp,fp],jump=NONE}
												val (instructions, temps) = forEachSpilled(instr,tmp,offset)
											in ([rewInstr,newInstr]@instructions, newTemp::temps)end
									| false => (case isSrc of
										true => let val newInstr = OPER {assem="movq "^its(offset)^"(%'s0), %'d0\n",dst=[newTemp],src=[fp],jump=NONE}
													val rewInstr = MOVE {assem=a,dst=d,src=newTemp}
													val (instructions, temps) = forEachSpilled(instr,tmp,offset)
												in ([newInstr,rewInstr]@instructions, newTemp::temps)end
										| false => let val (instructions, temps) = forEachSpilled(instr,tmp,offset)
													in (i::instructions,temps) end))end)end)
				| _ => let val (instructions, temps) = forEachSpilled(instr,tmp,offset)
						in (i::instructions,temps) end																			
														
	(* La lista de instrucciones y el frame serán importados. La lista de temporales primera debe ser vacía*) 
	fun rewriteProgram(linstr : instr list, f : frame, ltemp : tigertemp.temp list) = case length(!spilledNodes) of
												0 => (print("Programa reescrito\n");printInstrList linstr;(linstr,ltemp))
												| _ => 	let 
															val n = hd(!spilledNodes) 
															val _ = spilledNodes := tl(!spilledNodes)
															val InFrame k = allocLocal f true
															val (instructions, temps) = forEachSpilled(linstr,n,k)
														in rewriteProgram(instructions,f,ltemp@temps) end

	fun initialize() = let 	val _ = selectStack := []
							val _ = spillWorkSet := emptyStr
							val _ = simplifyWorkSet := emptyStr
							val _ = degree := tabNueva()
							val _ = color := tabNueva()
							val _ = precoloredSet := emptyStr
							val _ = spilledNodes := []
							val _ = registersSet := emptyStr
							val _ = coalescedNodes := emptyStr
						in () end

	fun pintar n = (case tabBusca(n,!color) of
						NONE => raise Fail ("Temporal sin color asignado "^n)
						| SOME c => c) 	
										
	fun colorear'(l,f,initial) = 
		let 
			val _ = tigerbuild.build(l,0)		
		
			val _ = spilledNodes := []
			
			(*makeWorkList()*)
			val _ = degree := tabAAplica (id,Splayset.numItems,!interf)
			val _ = precoloredList := ["rbx", "rsp","rdi", "rsi", "rdx", "rcx", "r8", "r9", "rbp", "rax","r10","r11","r12","r13","r14","r15"]
			
			val _ = color := fillColor(!precoloredList,!color)	
			val _ = coalescedNodes := emptyStr
			val _ = precoloredSet := addList(emptyStr, !precoloredList)		
			val _ = simplifyWorkSet := initial
			val _ = freezeWorkSet := fillFreezeWorkSet ()
			val _ = spillWorkSet := addList (emptyStr,tabClaves (tabFiltra ((fn n => n >= K),!degree)))		
									
			(*repeat until*)		
			val _ = repeatUntil()	

			(* assign colors*)
			val coloredNodes = assignColors(emptyStr, !selectStack)

			(* rewrite program*)
			val (instructions, temps) = rewriteProgram(l,f,[])
			
			val _ = (print("Temporales agregados\n");List.app print temps)
			
		in if temps = [] then (pintar,instructions) else colorear'(instructions,f, addList (coloredNodes, temps) ) end	
		
	fun colorear (l,f,printt) = 
		let
			(* OJOO: HAY QUE VACIAR TODAS LAS LISTAS Y CONJUNTOS CADA VEZ QUE EMPIEZO EL ALGORITMO*)
			val _ = initialize()
			
			val _ = tigerbuild.build(l,printt)		
			(*makeWorkList()*)
			val _ = degree := tabAAplica (id,Splayset.numItems,!interf)
			val _ = setOfAllTemps := addList (emptyStr, tabClaves (!degree))
			val _ = precoloredList := ["rbx", sp,"rdi", "rsi", "rdx", "rcx", "r8", "r9", fp, "rax","r10","r11","r12","r13","r14","r15"]

			val _ = registersSet := addList (emptyStr, registers) 
				
			val _ = color := fillColor(!precoloredList,!color)			
			val _ = precoloredSet := addList(emptyStr, !precoloredList)
			val _ = simplifyWorkSet := fillSimplifyWorkSet ()
			val _ = freezeWorkSet := fillFreezeWorkSet ()
			val _ = spillWorkSet := addList (emptyStr,tabClaves (tabFiltra ((fn n => n >= K),!degree)))								
			(*repeat until*)		
			val _ = repeatUntil()	

			(* assign colors*)
			val coloredNodes = assignColors(emptyStr, !selectStack)

			(* rewrite program*)
			val (instructions, temps) = rewriteProgram(l,f,[])
			val initial = addList (union(coloredNodes,!coalescedNodes), temps)
			
			val _ = (print("Temporales agregados\n");List.app print temps)
									 		 				
		in if temps = [] then (print("No hizo spill\n");(pintar,instructions)) else colorear'(instructions,f, initial) end	 
end

