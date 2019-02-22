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
	val simplifyWorkSet2 = ref(emptyStr)
	val degree = ref (tabNueva())
	val color = ref (tabNueva())
	val spilledNodes = ref ([])
	val registersSet = ref(emptyStr)
	val coloredNodes = ref(emptyStr)
	val initial = ref(emptyStr)
	
	val alias = ref (tabNueva()) 
	val coalescedNodes = ref(emptyStr) 
	val coalescedMoves = ref(emptyInt) 
	val constrainedMoves = ref(emptyInt) 
	val freezeWorkSet = ref (emptyStr) 
	val activeMoves = ref(emptyInt) 
	val frozenMoves = ref(emptyInt) 
	val setOfAllTemps = ref(emptyStr) 

	(**********************************************************************************************************)
	fun invNodes() = let
						val listNodes = listItems (!setOfAllTemps)
						val setOfSpilled = addList(emptyStr,!spilledNodes)
						val selectStackSet = addList(emptyStr,!selectStack)
						fun f n = let
									val lista = [!precoloredSet,!initial,!simplifyWorkSet,!freezeWorkSet,!spillWorkSet,setOfSpilled,!coalescedNodes,!coloredNodes,selectStackSet]
									val lInts = map (fn s => if member(s,n) then 1 else 0) lista									
									val suma = List.foldl (fn (n1,n2) => n1 + n2) 0 lInts
									val _ = if suma <> 1 then (print (n^": ");List.app (fn n => print(Int.toString(n)^" ")) lInts;print("\n")) else ()
									val _ = if suma = 1 then () else raise Fail "invNodes"
								 in () end
					 in List.app f listNodes end
					 
	fun invMoves() = let
						val listMoves = listItems(!allMoves)
						fun f n = let 
									val listaSets = [!coalescedMoves,!constrainedMoves,!frozenMoves,!workSetMoves,!activeMoves]
									val lInts = map (fn s => if member(s,n) then 1 else 0) listaSets
									val suma = List.foldl (fn (n1,n2) => n1 + n2) 0 lInts
									val _ = if suma <> 1 then (print ("instruccion "^Int.toString(n)^": ");List.app (fn n => print(Int.toString(n)^" ")) lInts;print("\n")) else ()
									val _ = if suma = 1 then () else raise Fail "invMoves"
								in () end
					 in List.app f listMoves end	
	
	(**********************************************************************************************************)
																							
	fun findSetInt (t : temp, tabla) = (case tabBusca (t,tabla) of
											NONE => emptyInt
											| SOME c => c)
											
	fun findSetStr (t : temp, tabla) = (case tabBusca (t,tabla) of
											NONE => emptyStr
											| SOME c => c)											
											
    fun nodeMoves n = intersection (findSetInt(n,!moveSet), union(!activeMoves,!workSetMoves))
	
	fun moveRelatedFun n = not (equal(nodeMoves(n),emptyInt))
	
    fun enableMoves nodes = let 
								val nList = listItems(nodes)
								val l1 = map nodeMoves nList (* (int Splayset.set ) list*)
								val l2 = map (fn n => intersection (n,!activeMoves)) l1 
							in List.app (fn n => (activeMoves := difference(!activeMoves,n); workSetMoves := union (!workSetMoves, n))) l2 end
							
	fun adjacent n = difference(buscoEnTabla (n,!interfNoPrec),union(addList(emptyStr,!selectStack),!coalescedNodes))
	 
	fun minusOneSet s x = x-1 
														
	fun decrementDegree (s) = let 		
								val _ = (print ("entrando a drecrement\n");invNodes())
								val listTemps = listItems s
								val listKNeig = List.filter (fn n => (buscoEnTabla(n,!degree)) = K) listTemps		
														
								fun minusOne n = case tabBusca(n,!degree) of
													NONE => raise Fail "No deberia pasar minusOne"
													| SOME i => i-1
								
								fun f (tmp, t) = tabRInserta (tmp,minusOne tmp,t) 					
								val _ = degree := List.foldl f (!degree) listTemps
								val setKNeig = addList(emptyStr,listKNeig)
								
								val activarMoves = List.foldl (fn (n,set) =>union(adjacent(n),set)) setKNeig listKNeig
								val _ = print("ok\n")
								
								val _ = enableMoves (activarMoves)
															
								val _ = spillWorkSet := difference (!spillWorkSet,setKNeig)
								
								val nodesKMoveRelated = addList(emptyStr,List.filter moveRelatedFun listKNeig)(*intersection (!moveRelated, setKNeig)*)
								val nodesKMoveRelated = addList(emptyStr,List.filter moveRelatedFun listKNeig)(*intersection (!moveRelated, setKNeig)*)
								val nodesKNoMoveRelated = difference (setKNeig, nodesKMoveRelated)
								
								val _ = freezeWorkSet := union(!freezeWorkSet,nodesKMoveRelated)
								val _ = simplifyWorkSet := union (!simplifyWorkSet,nodesKNoMoveRelated)	
								val _ = if equal (emptyStr, intersection (!simplifyWorkSet2,nodesKNoMoveRelated)) then () else raise Fail "agrego algo que ya estaba decrement"
								val _ = simplifyWorkSet2 := union (!simplifyWorkSet2,nodesKNoMoveRelated)	
								val _ = (print("saliendo de drecrement\n");invNodes() )
								in () end 
	
    fun getAlias (t) = if member(!coalescedNodes,t) then getAlias(buscoEnTabla(t,!alias)) else t                               
    
    fun areAdj (t1,t2) = member(buscoEnTabla(t1,!interf),t2)
    
    fun ok (t,r) = (buscoEnTabla(t,!degree) < K) orelse member(!precoloredSet,t) orelse areAdj (t,r)
        
	fun conservative nodes = length(List.filter (fn n => (buscoEnTabla(n,!degree) >= K)) (listItems nodes)) < K
	
	fun addWorkList u = let 
							val c1 = not (member (!precoloredSet,u))
							val c2 = not (moveRelatedFun(u)) andalso (buscoEnTabla(u,!degree) < K)
							val cond = c1 andalso c2
							val uSet = add(emptyStr,u)
						in (if cond then (freezeWorkSet := difference(!freezeWorkSet,uSet);
										  simplifyWorkSet := union(!simplifyWorkSet,uSet);
						                  (if member(!simplifyWorkSet2,u) then raise Fail ("agrego algo que ya estaba addWorkLis"^u) else ());
										  simplifyWorkSet2 := union (!simplifyWorkSet2,uSet))	

						                  else ()) end	
	
	
	fun tempsInMove n = case buscoEnTabla (n,!natToInstr) of
							MOVE {assem=_,dst=d,src=s} => (s,d)
							| _ => raise Fail "tempsInMove no deberia pasar"

	fun freezeMoves u = let
							fun aux n = let
											val nSet = add(emptyInt,n)			
											val (x,y) = tempsInMove n (*NO SABEMOS ORDEN CORRECTO *)
											val v = if getAlias(y) = getAlias(u) then getAlias(x) else getAlias(y)
											val _ = print("x:"^x^" getAlias(x):"^x^" y:"^y^" getAlias(y):"^y^"\n")
											val vSet = add(emptyStr,v)
											val _ = activeMoves := difference(!activeMoves,nSet)
											val _ = frozenMoves := union (!frozenMoves,nSet)
											val cond = equal(nodeMoves(v),emptyInt) andalso (buscoEnTabla(v,!degree) < K)
											(* esta condicion la agregué yo*)
											val cond2 = cond andalso not (member(!precoloredSet,v))
										    val _ =  if cond2 then ((*freezeWorkSet := difference(!freezeWorkSet,vSet);*)
																  freezeWorkSet := difference(!freezeWorkSet,vSet);													
											                      simplifyWorkSet := union(!simplifyWorkSet, vSet);		
											                      print("Estoy en el app de freezemoves\n");invNodes();          
																  simplifyWorkSet2 := union (!simplifyWorkSet2,vSet)) else ()
									    in () end	
							val _ = print("\nANTES DE freezeMoves: EJECUTO INVARIANTE NODOS\n")
							val _ = invNodes()
							val _ = print ("\nANTES DE freezeMoves: OK INVARIANTE NODOS\n")
							val _ = print("\nANTES DE freezeMoves: EJECUTO INVARIANTE MOVES\n")
							val _ = invMoves()
							val _ = print ("\nANTES DE freezeMoves: OK INVARIANTE MOVES\n")
							
							val _ = Splayset.app aux (nodeMoves(u))
							
							val _ = print("\nDESPUES DE freezeMoves: EJECUTO INVARIANTE NODOS\n")
							val _ = invNodes()
							val _ = print ("\nDESPUES DE freezeMoves: OK INVARIANTE NODOS\n")
							val _ = print("\nDESPUES DE freezeMoves: EJECUTO INVARIANTE MOVES\n")
							val _ = invMoves()
							val _ = print ("\nDESPUES DE freezeMoves: OK INVARIANTE MOVES\n")														
						
						in () end
						  										
    
	fun addEdge (u,v) = let
							val vSet = Splayset.singleton String.compare v 
							val uSet = Splayset.singleton String.compare u
						in (if not (u = v) andalso not (areAdj(u,v)) then 
													 (interf := tabRInserta(u,union(buscoEnTabla(u,!interf),vSet),!interf);
													 interf := tabRInserta(v,union(buscoEnTabla(v,!interf),uSet),!interf);
													 (if not (member(!precoloredSet, u)) then 
																(interfNoPrec := tabRInserta(u,union(buscoEnTabla(u,!interfNoPrec),vSet),!interfNoPrec);
																degree := tabRInserta(u,buscoEnTabla(u,!degree)+1,!degree)) else ());
													 (if not (member(!precoloredSet, v)) then 
																(interfNoPrec := tabRInserta(v,union(buscoEnTabla(v,!interfNoPrec),uSet),!interfNoPrec);
																degree := tabRInserta(v,buscoEnTabla(v,!degree)+1,!degree)) else ()))			
																else ()) end
				
	fun combine (u,v) = let 
							val vSet = Splayset.singleton String.compare v
							val uSet = Splayset.singleton String.compare u
							val _ = if member (!freezeWorkSet, v) then freezeWorkSet := difference (!freezeWorkSet, vSet) else spillWorkSet := difference (!spillWorkSet,vSet)
							val _ = (print("Agrego el nodo v:"^v^" a coalescedNodes u es "^u^"\n");coalescedNodes := union (!coalescedNodes,vSet))
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
					

	fun fillColor ([],c) = c
	  | fillColor ((x::xs),c) = tabRInserta(x,x,(fillColor (xs,c)))
	  														
	
	fun simplify () = (print("\nANTES DE SIMPLIFY: EJECUTO INVARIANTE NODOS\n");
					   invNodes();
					   print ("\nANTES DE SIMPLIFY: OK INVARIANTE NODOS\n");
					   print("\nANTES DE SIMPLIFY: EJECUTO INVARIANTE MOVES\n");
					   invMoves();
					   print ("\nANTES DE SIMPLIFY: OK INVARIANTE MOVES\n");
					  (case (numItems(!simplifyWorkSet)) of 
								0 => repeatUntil()
								| _ => (let 
											val n = hd(listItems (!simplifyWorkSet))
											val _ = simplifyWorkSet := delete(!simplifyWorkSet,n)
											val _ = selectStack := !selectStack @ [n]
											val adjN = adjacent n
											(* esta condición la agregué yo*)
											val _ = decrementDegree (difference(adjN,!precoloredSet))	
											in  simplify () end));
					   print("\nDESPUES DE SIMPLIFY: EJECUTO INVARIANTE NODOS\n");
					   invNodes();
					   print ("\nDESPUES DE SIMPLIFY: OK INVARIANTE NODOS\n");
					   print("\nDESPUES DE SIMPLIFY: EJECUTO INVARIANTE MOVES\n");
					   invMoves();
					   print ("\nDESPUES DE SIMPLIFY: OK INVARIANTE MOVES\n"))
						
						
	and coalesce ()	= let 
						val _ = print("\nANTES DE coalesce: EJECUTO INVARIANTE NODOS\n")
					    val _ = invNodes()
					    val _ = print ("\nANTES DE coalesce: OK INVARIANTE NODOS\n")
					    val _ = print("\nANTES DE coalesce: EJECUTO INVARIANTE MOVES\n")
					    val _ = invMoves()
					    val _ = print ("\nANTES DE coalesce: OK INVARIANTE MOVES\n")
					    
						val m = hd (listItems(!workSetMoves))
						val mSet = Splayset.singleton Int.compare m
						val (x',y') = tempsInMove m (*NO SABEMOS ORDEN CORRECTO *)
						val x = getAlias(x')
						val y = getAlias(y')
						val _ = print("x:"^x'^" getAlias(x):"^x^" y:"^y'^" getAlias(y):"^y^"\n")
						(* si hay precoloreado, que sea u*)
						val (u,v) = if member(!precoloredSet,y) then (y,x) else (x,y)
						val uIsMember = member(!precoloredSet,u)
						val vIsMember = member(!precoloredSet,v)
						val _ = if  member(!precoloredSet,y) then print(y^" es precoloreado") else ()
						val _ = workSetMoves := delete (!workSetMoves,m)
						val cond1 = vIsMember orelse areAdj(u,v)
						val adjU = if uIsMember then emptyStr else adjacent(u)
						val adjV = if vIsMember then emptyStr else adjacent(v)
						val allAreOk = List.foldl (fn (b1,b2) => b1 andalso b2) true (List.map (fn t => ok(t,u)) (listItems adjV))
						val cond2 = (uIsMember andalso allAreOk) orelse ((not uIsMember) andalso conservative(union(adjV,adjU)))
						val _ = if (u = v) then (coalescedMoves := union(!coalescedMoves,mSet);
												 addWorkList(u)) else (if cond1 then (constrainedMoves := union(!constrainedMoves,mSet);
																					 addWorkList(u); addWorkList(v)) else (if cond2 then (coalescedMoves := union (!coalescedMoves,mSet);
																																		  combine(u,v); addWorkList(u)) else activeMoves := union(!activeMoves,mSet)))
						val _ = print("\nDESPUES DE coalesce: EJECUTO INVARIANTE NODOS\n")
						val _ = invNodes()
						val _ = print ("\nDESPUES DE coalesce: OK INVARIANTE NODOS\n")
						val _ = print("\nDESPUES DE coalesce: EJECUTO INVARIANTE MOVES\n")
						val _ = invMoves()
						val _ = print ("\nDESPUES DE coalesce: OK INVARIANTE MOVES\n")
																																			  
					  in repeatUntil() end
    
    and freeze () = let
						val _ = print("\nANTES DE freeze: EJECUTO INVARIANTE NODOS\n")
						val _ = invNodes()
						val _ = print ("\nANTES DE freeze: OK INVARIANTE NODOS\n")
						val _ = print("\nANTES DE freeze: EJECUTO INVARIANTE MOVES\n")
						val _ = invMoves()
						val _ = print ("\nANTES DE freeze: OK INVARIANTE MOVES\n")
						    
						val u = hd (listItems(!freezeWorkSet))
						val uSet = add(emptyStr,u)
						val _ = freezeWorkSet := difference(!freezeWorkSet, uSet)
						val _ = simplifyWorkSet := union(!simplifyWorkSet,uSet)
						val _ = ((if member(!simplifyWorkSet2,u) then raise Fail ("agrego algo que ya estaba freeze"^u^"\n") else ());
																  simplifyWorkSet2 := union (!simplifyWorkSet2,uSet))
						val _ = freezeMoves(u)
	
						val _ = print("\nDESPUES DE freeze: EJECUTO INVARIANTE NODOS\n")
						val _ = invNodes()
						val _ = print ("\nDESPUES DE freeze: OK INVARIANTE NODOS\n")
						val _ = print("\nDESPUES DE freeze: EJECUTO INVARIANTE MOVES\n")
						val _ = invMoves()
						val _ = print ("\nDESPUES DE freeze: OK INVARIANTE MOVES\n")


					 in repeatUntil() end
  												
	and selectSpill () = let
							val _ = print("\nANTES DE selectSpill: EJECUTO INVARIANTE NODOS\n")
						    val _ = invNodes()
						    val _ = print ("\nANTES DE selectSpill: OK INVARIANTE NODOS\n")
						    val _ = print("\nANTES DE selectSpill: EJECUTO INVARIANTE MOVES\n")
						    val _ = invMoves()
						    val _ = print ("\nANTES DE selectSpill: OK INVARIANTE MOVES\n")
						    
							val m = hd(listItems (!spillWorkSet))
							val mSet = add(emptyStr,m)							
							val _ = spillWorkSet := difference (!spillWorkSet,mSet)
							val _ = simplifyWorkSet := union (!simplifyWorkSet,mSet)
							val _ = ((if member(!simplifyWorkSet2,m) then raise Fail ("agrego algo que ya estaba selectSpill"^m) else ());
																  simplifyWorkSet2 := union (!simplifyWorkSet2,mSet))							
							val _ = freezeMoves(m)
							
							val _ = print("\nDESPUES DE selectSpill: EJECUTO INVARIANTE NODOS\n")
							val _ = invNodes()
							val _ = print ("\nDESPUES DE selectSpill: OK INVARIANTE NODOS\n")
							val _ = print("\nDESPUES DE selectSpill: EJECUTO INVARIANTE MOVES\n")
							val _ = invMoves()
							val _ = print ("\nDESPUES DE selectSpill: OK INVARIANTE MOVES\n")
						in repeatUntil() end

	and repeatDo (lengthSimplify,lengthCoalesce,lengthFreeze,lengthSelectSpill) =
		if (lengthSimplify <> 0) then 
			simplify() else (if (lengthCoalesce <> 0) then
								coalesce () else (if (lengthFreeze <> 0) then
													freeze() else (if (lengthSelectSpill <> 0) then
																	(selectSpill ()) else raise Fail "No deberia pasar (repeatDo)")))
					   
	and repeatUntil () = let
							val lengthSimplify = numItems (!simplifyWorkSet)
							val lengthCoalesce = numItems(!workSetMoves)
							val lengthFreeze = numItems (!freezeWorkSet)
							val lengthSelectSpill = numItems(!spillWorkSet)							 
							val fin = ((lengthSimplify = 0) andalso (lengthCoalesce = 0) andalso (lengthFreeze = 0) andalso (lengthSelectSpill = 0))							
						  in if fin then () else repeatDo (lengthSimplify,lengthCoalesce,lengthFreeze,lengthSelectSpill) end
						       					  									
													
	fun assignColors (cNodes, stack) = case (length (stack)) of
								0 => (let 
									val _ = (print ("Tabla colores sin nodos coalesced\n");tigertab.tabPrintTempTemp(!color))
									val _ = (print ("Coalesced: "); Splayset.app  (fn n => print(getAlias(n)^" ")) (!coalescedNodes))
									val _ = (print ("Spillednodes: ");List.app  (fn n => print(n^" ")) (!spilledNodes))
								      fun f (n,tab) = (print (n^" "^getAlias(n)^"\n"); tabRInserta(n,buscoEnTabla(getAlias(n),tab),tab))
								      val _ = color := Splayset.foldl f (!color) (!coalescedNodes) 
								      val _ = (print ("Tabla colores\n");tigertab.tabPrintTempTemp(!color))					     
								      in cNodes end)
								| _ => case (member(!precoloredSet,hd (stack))) of
									false =>
										(let 
											val n = hd (stack) 
											val stack' = tl(stack)
											val _ = selectStack := stack'
											val adj = buscoEnTabla (n,!interfNoPrec) : tigertemp.temp Splayset.set
											val uni = union (cNodes, !precoloredSet) : tigertemp.temp Splayset.set
											(*val _ = print "\nuni es\n"
											val _ = List.app print (listItems uni)*)
											fun discardColors (n : tigertemp.temp,s) = let 
																					val nAlias = getAlias(n)
																					val isMember = member (uni,nAlias)
																					(*val _ = print ("antes busco en tabla discardcolors")*)
																					val colorUsed = if isMember then add(emptyStr,buscoEnTabla(nAlias,!color)) else emptyStr
																					(*val _ = print ("desp busco en tabla discardcolors")*)
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
											val _ = coloredNodes := cNodes'											
										in assignColors (cNodes', stack') end)
									| true => let
													val stack' =  tl(stack)
													val _ = selectStack := stack'
												in assignColors (cNodes,stack') end											

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
															val _ = print "reescribiendo\n"
															val n = hd(!spilledNodes) 
															val _ = spilledNodes := tl(!spilledNodes)
															val InFrame k = allocLocal f true
															val (instructions, temps) = forEachSpilled(linstr,n,k)
														in rewriteProgram(instructions,f,ltemp@temps) end														
															
	fun makeWorkList (ini) = let
								val iniList = listItems ini
								
								val greaterEqualKSet = addList(emptyStr,List.filter (fn n => buscoEnTabla(n,!degree) >= K) iniList)
								val _ = spillWorkSet := union(!spillWorkSet,greaterEqualKSet)
								
								val lowerKSet = difference(ini,greaterEqualKSet)
								val lowerKList = listItems lowerKSet
								
								val lowerMoveRelSet = addList(emptyStr, List.filter (fn n => moveRelatedFun(n)) lowerKList)
								val _ = freezeWorkSet := union(!freezeWorkSet,lowerMoveRelSet)
								val lowerNonMoveRelSet = difference	(lowerKSet,lowerMoveRelSet)
								val _ = simplifyWorkSet := union (!simplifyWorkSet,lowerNonMoveRelSet)
								val _ = simplifyWorkSet2 := union (!simplifyWorkSet2,lowerNonMoveRelSet)
								val launion = union(!spillWorkSet,union(!simplifyWorkSet,!freezeWorkSet))
								val lainter = intersection(!spillWorkSet,intersection(!simplifyWorkSet,!freezeWorkSet))
								val _ = if member(!freezeWorkSet,"T146") then print ("T55 en freeze\n") else ()
								val _ = if member (!simplifyWorkSet,"T146") then print ("t55 en simplify") else ()
								val _ = if member (!spillWorkSet,"T146") then print ("t55 en spill") else ()
								val _ = if equal(emptyStr,lainter) then print ("la interseccion es vacia\n") else raise Fail "Error makeWorkList"
								val _ = if equal(ini,launion) then print ("la union es initial\n") else raise Fail "Error makeWorkList"
								val _ = initial := emptyStr
							 in () end									
	
	fun initialize() = let 	val _ = selectStack := []
							val _ = spillWorkSet := emptyStr
							val _ = simplifyWorkSet := emptyStr
							val _ = simplifyWorkSet2 := emptyStr
							val _ = degree := tabNueva()
							val _ = color := tabNueva()
							val _ = precoloredSet := emptyStr
							val _ = spilledNodes := []
							val _ = registersSet := emptyStr
							val _ = coalescedNodes := emptyStr
							val _ = workSetMoves := emptyInt
							val _ = coloredNodes := emptyStr
						in () end

	fun pintar n = (case tabBusca(n,!color) of
						NONE => raise Fail ("Temporal sin color asignado "^n)
						| SOME c => c) 	
										
	fun colorear'(l,f,inicial) = 
		let 
			val _ = tigerbuild.build(l,1)				
			val _ = spilledNodes := []
			
			val _ = initial := inicial
			(*makeWorkList()*)
			val _ = degree := tabAAplica (id,Splayset.numItems,!interf)
			val _ = setOfAllTemps := addList (emptyStr, tabClaves (!interf))
			val _ = precoloredList := ["rbx", "rsp","rdi", "rsi", "rdx", "rcx", "r8", "r9", "rbp", "rax","r10","r11","r12","r13","r14","r15"]
			
			val _ = color := fillColor(!precoloredList,!color)	
			val _ = coalescedNodes := emptyStr
			val _ = precoloredSet := addList(emptyStr, !precoloredList)		
			
			
			val _ = makeWorkList(inicial)		
									
			(*repeat until*)		

			val _ = repeatUntil()				
			(* assign colors*)
			val coloredNodes = assignColors(emptyStr, !selectStack)

			val _ = print "reescribiendo\n"
			(* rewrite program*)
			val (instructions, temps) = rewriteProgram(l,f,[])
			val initial' = addList (union(coloredNodes,!coalescedNodes), temps)
			
			val _ = (print("Temporales agregadoss\n");List.app print temps)
			
			val _ = (print("Lista que es argumento de colorear' desde colorear': "); List.app (fn n => print(n^"\n")) (listItems(addList (coloredNodes, temps))))

		in if temps = [] then (pintar,instructions) else colorear'(instructions,f, initial') end	
		
	fun colorear (l,f,printt) = 
		let
			(* OJOO: HAY QUE VACIAR TODAS LAS LISTAS Y CONJUNTOS CADA VEZ QUE EMPIEZO EL ALGORITMO*)
			val _ = initialize()
			
			val _ = tigerbuild.build(l,printt)		
			
			
			(*makeWorkList()*)
			val _ = degree := tabAAplica (id,Splayset.numItems,!interf)
			val _ = setOfAllTemps := addList (emptyStr, tabClaves (!degree))
			
			val inicial = difference(!setOfAllTemps,!precoloredSet)
			val _ = initial := inicial

			val _ = registersSet := addList (emptyStr, registers) 
				
			val _ = color := fillColor(!precoloredList,!color)			
			
			val _ = makeWorkList(inicial)
			
			(*repeat until*)		
			val _ = repeatUntil()	
			(* assign colors*)
			val coloredNodes = assignColors(emptyStr, !selectStack)

			val _ = print "reescribiendo2\n"
			(* rewrite program*)
			val (instructions, temps) = rewriteProgram(l,f,[])
			val initial' = addList (union(coloredNodes,!coalescedNodes), temps)
			
			val _ = (print("colaescedNodes: "); List.app (fn n => print(n^"\n")) (listItems(!coalescedNodes)))
			
			val _ = (print("Temporales agregados\n");List.app print temps)
			 		 				
		in if temps = [] then (print("No hizo spill\n");(pintar,instructions)) else colorear'(instructions,f, initial') end	 
end
