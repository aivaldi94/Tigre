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
  val spilledNodes = ref ([])
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
  val newTemps = ref(emptyStr)

  fun findSetInt (t : temp, tabla) = (case tabBusca (t,tabla) of
                      NONE => emptyInt
                      | SOME c => c)
                      
  fun findSetStr (t : temp, tabla) = (case tabBusca (t,tabla) of
                      NONE => emptyStr
                      | SOME c => c)  
  
  fun printList [] = ()
    | printList (z::zs) = (print(z^" ");printList zs)
    
  fun printSet s = printList(listItems s)   
  
  fun updateProlEpil(f,l) = 
    let
      val lWhitoutProl = tl(tl(tl l))
      val lWhitoutProlEpil = rev( tl(tl(tl (rev lWhitoutProl))))
    in tigerframe.procEntryExit3 (f,lWhitoutProlEpil) end  
    
  fun its n =  if n<0 then "-" ^ Int.toString(~n) else Int.toString(n)                       
        
  (**********************************************************************************************************)
  
  fun invNodes() =
    let
      val listNodes = listItems (!setOfAllTemps)
      val setOfSpilled = addList(emptyStr,!spilledNodes)
      val selectStackSet = addList(emptyStr,!selectStack)            
      fun f n =
        let
          val lista = [!precoloredSet,!initial,!simplifyWorkSet,!freezeWorkSet,!spillWorkSet,setOfSpilled,
                       !coalescedNodes,!coloredNodes,selectStackSet]
          val lInts = map (fn s => if member(s,n) then 1 else 0) lista                  
          val suma = List.foldl (fn (n1,n2) => n1 + n2) 0 lInts
          val _ = if suma <> 1 then (print (n^": ");List.app (fn n => print(its(n)^" ")) lInts;print("\n")) else ()
          val _ = if suma = 1 then () else raise Fail "invNodes"
        in () end
    in List.app f listNodes end
           
  fun invMoves() =
    let
      val listMoves = listItems(!allMoves)
      fun f n = 
        let 
          val listaSets = [!coalescedMoves,!constrainedMoves,!frozenMoves,!workSetMoves,!activeMoves]
          val lInts = map (fn s => if member(s,n) then 1 else 0) listaSets
          val suma = List.foldl (fn (n1,n2) => n1 + n2) 0 lInts
          val _ = if suma <> 1 then (print ("instruccion "^its(n)^": ");List.app (fn n => print(its(n)^" ")) lInts;print("\n")) else ()
          val _ = if suma = 1 then () else raise Fail "invMoves"
        in () end
   in List.app f listMoves end  
           
  fun invDegree() =
    let
      val set = union(!simplifyWorkSet, union(!freezeWorkSet,!spillWorkSet))
      val list = listItems(set)
      fun f n =
        let
          val adjN = buscoEnTabla(n,!interfNoPrec)
          val set = union(!precoloredSet, union(!simplifyWorkSet, union(!freezeWorkSet, !spillWorkSet)))
        in numItems(intersection(adjN,set)) end
      val degreeList = map f list
      val degreeList2 = map (fn n => buscoEnTabla(n,!degree)) list
      fun equalLists [] [] = true
        | equalLists (x::xs) (y::ys) = if x=y then equalLists xs ys else false
        | equalLists _ _ = false 
    in if (equalLists degreeList degreeList2) then () else raise Fail "invDegree" end
            
  fun invSpill() = 
    let
      val list = listItems (!spillWorkSet)
      val booleanList = map (fn n => buscoEnTabla(n,!degree) >= K) list
    in if (List.foldl (fn (b1,b2) => b1 andalso b2) true booleanList) then () else raise Fail "invSpill" end
            
  fun invSimplify() = (*invariante relajado*)
    let 
      val list = listItems (!simplifyWorkSet)
      fun f n = 
        let
          val degreeLow = buscoEnTabla(n,!degree) < K                  
          val isEmpty = equal(intersection(findSetInt(n,!moveSet), union(!activeMoves, !workSetMoves)),emptyInt)
        in isEmpty end
      val booleanList = map f list
   in if (List.foldl (fn (b1,b2) => b1 andalso b2) true booleanList) then () else raise Fail "invSimplify" end
           
  fun invFreeze() =
  let
    val list = listItems (!freezeWorkSet)
    fun f n =
      let
        val degreeLow = buscoEnTabla(n,!degree) < K
        val isNotEmpty = not (equal(intersection(findSetInt(n,!moveSet), union(!activeMoves, !workSetMoves)),emptyInt))
      in degreeLow andalso isNotEmpty end
    val booleanList = map f list
  in if (List.foldl (fn (b1,b2) => b1 andalso b2) true booleanList) then () else raise Fail "invFreeze" end
           
  (***********************************************************************************************************)                   
                      
  fun nodeMoves n = intersection (findSetInt(n,!moveSet), union(!activeMoves,!workSetMoves))
  
  fun moveRelatedFun n = not (equal(nodeMoves(n),emptyInt))
  
  fun enableMoves nodes =
  let 
    val nList = listItems(nodes)
    val l1 = map nodeMoves nList 
    val l2 = map (fn n => intersection (n,!activeMoves)) l1 
  in List.app (fn n => (activeMoves := difference(!activeMoves,n); workSetMoves := union (!workSetMoves, n))) l2 end
            
  fun adjacent n = difference(buscoEnTabla (n,!interfNoPrec),union(addList(emptyStr,!selectStack),!coalescedNodes))
  
  fun adjacent' n = difference(buscoEnTabla (n,!interfNoPrec),!coalescedNodes)
   
  fun minusOneSet s x = x-1 
                            
  fun decrementDegree (s) =
    let     
      val _ = (invNodes();invSpill())
      val listTemps = listItems s
      val listKNeig = List.filter (fn n => (buscoEnTabla(n,!degree)) = K) listTemps   
                  
      fun minusOne n = case tabBusca(n,!degree) of
                NONE => raise Fail "No deberia pasar minusOne"
                | SOME i => i-1
      
      fun f (tmp, t) = tabRInserta (tmp,minusOne tmp,t)           
      val _ = degree := List.foldl f (!degree) listTemps
      val setKNeig = addList(emptyStr,listKNeig)
      
      val activarMoves = List.foldl (fn (n,set) =>union(adjacent(n),set)) setKNeig listKNeig
      
      val _ = enableMoves (activarMoves)
                    
      val _ = spillWorkSet := difference (!spillWorkSet,setKNeig)
      
      val nodesKMoveRelated = addList(emptyStr,List.filter moveRelatedFun listKNeig)
      val nodesKMoveRelated = addList(emptyStr,List.filter moveRelatedFun listKNeig)
      val nodesKNoMoveRelated = difference (setKNeig, nodesKMoveRelated)
      
      val _ = freezeWorkSet := union(!freezeWorkSet,nodesKMoveRelated)
      val _ = simplifyWorkSet := union (!simplifyWorkSet,nodesKNoMoveRelated) 
      val _ = (invNodes();invDegree();invSpill();invSimplify())
    in () end 
  
  fun getAlias (t) = if member(!coalescedNodes,t) then getAlias(buscoEnTabla(t,!alias)) else t                               
  
  fun areAdj (t1,t2) = member(buscoEnTabla(t1,!interf),t2)
  
  fun ok (t,r) = (buscoEnTabla(t,!degree) < K) orelse member(!precoloredSet,t) orelse areAdj (t,r)
      
  fun conservative nodes = length(List.filter (fn n => (buscoEnTabla(n,!degree) >= K)) (listItems nodes)) < K
  
  fun addWorkList u =
    let 
      val c1 = not (member (!precoloredSet,u))
      val c2 = not (moveRelatedFun(u)) andalso (buscoEnTabla(u,!degree) < K)
      val cond = c1 andalso c2
      val uSet = add(emptyStr,u)
    in (if cond then (freezeWorkSet := difference(!freezeWorkSet,uSet);
                      simplifyWorkSet := union(!simplifyWorkSet,uSet))                 
                else ()) end  

  fun tempsInMove n = case buscoEnTabla (n,!natToInstr) of
              MOVE {assem=_,dst=d,src=s} => (s,d)
              | _ => raise Fail "tempsInMove no deberia pasar"

  fun freezeMoves u =
    let
      fun aux n =
        let
          val nSet = add(emptyInt,n)      
          val (x,y) = tempsInMove n 
          val v = if getAlias(y) = getAlias(u) then getAlias(x) else getAlias(y)            
          val vSet = add(emptyStr,v)
          val _ = activeMoves := difference(!activeMoves,nSet)
          val _ = frozenMoves := union (!frozenMoves,nSet)
          val cond = equal(nodeMoves(v),emptyInt) andalso (buscoEnTabla(v,!degree) < K)
          (* condicion agregada *)
          val cond2 = cond andalso not (member(!precoloredSet,v))
          val _ =  if cond2 then (freezeWorkSet := difference(!freezeWorkSet,vSet);                         
                                  simplifyWorkSet := union(!simplifyWorkSet, vSet);   
                                  invNodes())
                            else ()
        in () end 
      val _ = invNodes()
      val _ = invMoves()
      val _ = Splayset.app aux (nodeMoves(u))
      val _ = invNodes()
      val _ = invMoves()          
    in () end
                          
    
  fun addEdge (u,v) =
    let
      val vSet = Splayset.singleton String.compare v 
      val uSet = Splayset.singleton String.compare u
    in ((if not (u = v) andalso not (areAdj(u,v)) then(interf := tabRInserta(u,union(buscoEnTabla(u,!interf),vSet),!interf);
                                                       interf := tabRInserta(v,union(buscoEnTabla(v,!interf),uSet),!interf);
                                                      (if not (member(!precoloredSet, u)) then 
                                (
                                interfNoPrec := tabRInserta(u,union(buscoEnTabla(u,!interfNoPrec),vSet),!interfNoPrec);
                                degree := tabRInserta(u,buscoEnTabla(u,!degree)+1,!degree)) else ());
                           (if not (member(!precoloredSet, v)) then 
                                (
                                interfNoPrec := tabRInserta(v,union(buscoEnTabla(v,!interfNoPrec),uSet),!interfNoPrec);
                                degree := tabRInserta(v,buscoEnTabla(v,!degree)+1,!degree)) else ()))     
                                else ())) end
        
  fun combine (u,v) =
    let 
      val _ = (invDegree();invSpill())
      val vSet = Splayset.singleton String.compare v
      val uSet = Splayset.singleton String.compare u
      val _ = if member (!freezeWorkSet, v) then freezeWorkSet := difference (!freezeWorkSet, vSet)
                                            else spillWorkSet := difference (!spillWorkSet,vSet)
      val _ = coalescedNodes := union (!coalescedNodes,vSet)
      val _ = alias := tabRInserta (v,u,!alias)
      val _ = moveSet := tabRInserta(u,union(buscoEnTabla(u,!moveSet),buscoEnTabla(v,!moveSet)),!moveSet)
      val _ = enableMoves(vSet)
      val adj = adjacent (v)      
      val _ = Splayset.app (fn t => addEdge(t,u)) adj
      val _ = decrementDegree(difference(adj,!precoloredSet))
      val cond = (buscoEnTabla(u,!degree) >= K) andalso member(!freezeWorkSet,u)
      val _ = if cond then (freezeWorkSet := delete (!freezeWorkSet,u);
                            spillWorkSet := union(!spillWorkSet,uSet))
                     else ()
      val _ = (invDegree();invSpill())
    in () end   
    
  fun getDegree t = buscoEnTabla (t,!degree)
          
  fun fillColor ([],c) = c
    | fillColor ((x::xs),c) = tabRInserta(x,x,(fillColor (xs,c)))
                                
  
  fun simplify () =
    (invNodes();
     invMoves();
     invDegree();
     invSpill();
     invSimplify();
     invFreeze();
     (case (numItems(!simplifyWorkSet)) of 
                0 => repeatUntil()
                | _ => (let 
                      val n = hd(listItems (!simplifyWorkSet))
                      val _ = simplifyWorkSet := delete(!simplifyWorkSet,n)
                      val _ = selectStack := !selectStack @ [n]
                      val adjN = adjacent n
                      (* condicion agregada *)
                      val _ = decrementDegree (difference(adjN,!precoloredSet)) 
                      in  simplify () end));
     invNodes();
     invMoves();
     invDegree();
     invSpill();
     invSimplify();
     invFreeze())
    
            
  and coalesce () =
    let 
      val _ = invNodes()
      val _ = invMoves()
      val _ = invDegree()
      val _ = invSpill()
      val _ = invSimplify()
      val _ = invFreeze()
      val m = hd (listItems(!workSetMoves))
      val mSet = Splayset.singleton Int.compare m
      val (x',y') = tempsInMove m 
      val x = getAlias(x')
      val y = getAlias(y')            
      (* si hay precoloreado, que sea u*)
      val (u,v) = if member(!precoloredSet,y) then (y,x) else (x,y)
      val uIsMember = member(!precoloredSet,u)
      val vIsMember = member(!precoloredSet,v)
      val _ = workSetMoves := delete (!workSetMoves,m)
      val isNewTempsRelated = member(!newTemps, u) orelse member(!newTemps, v)
      val cond1 = (vIsMember orelse areAdj(u,v)) orelse isNewTempsRelated
      val adjU = if uIsMember then emptyStr else adjacent(u)
      val adjV = if vIsMember then emptyStr else adjacent(v)
      val allAreOk = List.foldl (fn (b1,b2) => b1 andalso b2) true (List.map (fn t => ok(t,u)) (listItems adjV))
      val cond2 = (uIsMember andalso allAreOk) orelse ((not uIsMember) andalso conservative(union(adjV,adjU)))
      val _ = if (u = v) then (coalescedMoves := union(!coalescedMoves,mSet);
                               addWorkList(u))
                         else (if cond1 then (constrainedMoves := union(!constrainedMoves,mSet);
                                              addWorkList(u); addWorkList(v))
                                        else (if cond2 then (coalescedMoves := union (!coalescedMoves,mSet);
                                                             invDegree();
                                                             combine(u,v); 
                                                             addWorkList(u);
                                                             invDegree())
                                                       else activeMoves := union(!activeMoves,mSet)))
      val _ = invNodes()
      val _ = invMoves()
      val _ = invDegree()
      val _ = invSpill()
      val _ = invSimplify()
      val _ = invFreeze() 
          
    in repeatUntil() end
    
    and freeze () =
      let          
        val _ = invNodes()
        val _ = invMoves()
        val _ = invDegree()
        val _ = invSpill()
        val _ = invSimplify()
        val _ = invFreeze()

        val u = hd (listItems (difference(!freezeWorkSet,!newTemps)))
        val uSet = add(emptyStr,u)
        val _ = freezeWorkSet := difference(!freezeWorkSet, uSet)
        val _ = simplifyWorkSet := union(!simplifyWorkSet,uSet)
        val _ = freezeMoves(u)
  
        val _ = invNodes()
        val _ = invMoves()
        val _ = invDegree()
        val _ = invSpill()
        val _ = invSimplify()
        val _ = invFreeze()
      in repeatUntil() end
                          
  and selectSpill () =
    let
      val _ = invNodes()
      val _ = invMoves()
      val _ = invDegree()
      val _ = invSpill()
      val _ = invSimplify()
      val _ = invFreeze()
      (*Heurística para seleccionar spills: 
      - se evitan los temporales generados por spills previos
      - tienen prioridad los temporales que guardan registros callesaved*)
      val m = hd(listItems (difference(!spillWorkSet,!newTemps)))
      val mSet = add(emptyStr,m)              
      val _ = spillWorkSet := difference (!spillWorkSet,mSet)
      val _ = simplifyWorkSet := union (!simplifyWorkSet,mSet)
      val _ = freezeMoves(m)
      
      val _ = invNodes()
      val _ = invMoves()
      val _ = invDegree()
      val _ = invSpill()
      val _ = invSimplify()
      val _ = invFreeze()
    in repeatUntil() end

  and repeatDo (longSimplify,longCoalesce,longFreeze,longSelectSpill) =
    if (longCoalesce <> 0)
       then coalesce()
       else (if (longFreeze <> 0)
                 then freeze()
                 else (if (longSimplify <> 0)
                           then simplify()
                           else (if (longSelectSpill <> 0)
                                     then (selectSpill ())
                                     else raise Fail "No deberia pasar (repeatDo)")))
             
  and repeatUntil () =
    let
      val lengthSimplify = numItems (!simplifyWorkSet)
      val lengthCoalesce = numItems(!workSetMoves)
      val lengthFreeze = numItems (!freezeWorkSet)
      val lengthSelectSpill = numItems(!spillWorkSet)              
      val fin = ((lengthSimplify = 0) andalso (lengthCoalesce = 0) andalso (lengthFreeze = 0) andalso (lengthSelectSpill = 0))              
    in if fin then () else repeatDo (lengthSimplify,lengthCoalesce,lengthFreeze,lengthSelectSpill) end
                                                
                          
  fun assignColors (cNodes, stack) =
    case (length (stack)) of
        0 => ( if !spilledNodes = [] then                      
                        (let 
                          fun f (n,tab) = (tabRInserta(n,buscoEnTabla(getAlias(n),tab),tab))
                          val _ = color := Splayset.foldl f (!color) (!coalescedNodes)
                         in cNodes end)   
                         (*si hay algún spill efectivo, descarto todos los coalesced *)
                      else cNodes) 
        | _ => case (member(!precoloredSet,hd (stack))) of
                  false =>
                    (let 
                      val n = hd (stack) 
                      val stack' = tl(stack)
                      val _ = selectStack := stack'
                      val adj = buscoEnTabla (n,!interfNoPrec) : tigertemp.temp Splayset.set
                      val uni = union (cNodes, !precoloredSet) : tigertemp.temp Splayset.set
                      fun discardColors (n : tigertemp.temp,s) = let 
                                          val nAlias = getAlias(n)
                                          val isMember = member (uni,nAlias)                                          
                                          val colorUsed = if isMember then add(emptyStr,buscoEnTabla(nAlias,!color)) else emptyStr
                                        in difference (s,colorUsed) end
                      val okColors = Splayset.foldl discardColors (registersSet) adj
                      val cNodes' = case length (listItems(okColors)) of
                            0 => (let 
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

  fun forEachSpilled ([], tmp, offset) = ([],[])
    | forEachSpilled (i::instr, tmp, offset) = 
      case i of
        OPER {assem=a,dst=d,src=s,jump=j} => 
          (let  
             fun igual n = (n=tmp)
             val isDst = List.exists igual d
             val isSrc = List.exists igual s
           in (case (isDst andalso isSrc) of
                 true => let 
                           val newTemp = newtemp()
                           val newInstr1 = OPER {assem="movq "^its(offset)^"(%'s0), %'d0\n",
                                                 dst=[newTemp],src=[fp],jump=NONE}
                           val d' = map (fn n => if n = tmp then newTemp else n) d
                           val s' = map (fn n => if n = tmp then newTemp else n) s
                           val rewInstr = OPER {assem=a,dst=d',src=s',jump=j}
                           val newInstr2 = OPER {assem="movq %'s0, "^its(offset)^"(%'s1)\n",
                                                 dst=[],src=[newTemp,fp],jump=NONE}
                           val (instructions, temps) = forEachSpilled(instr,tmp,offset)
                         in ([newInstr1,rewInstr,newInstr2]@instructions, newTemp::temps)end
                 | false => let
                              val newTemp = newtemp()
                            in (case isDst of
                                  true =>
                                    let
                                      val d' = map (fn n => if n = tmp then newTemp else n) d
                                      val rewInstr = OPER {assem=a,dst=d',src=s,jump=j}
                                      val newInstr = OPER {assem="movq %'s0, "^its(offset)^"(%'s1)\n",dst=[],
                                                           src=[newTemp,fp],jump=NONE}
                                      val (instructions, temps) = forEachSpilled(instr,tmp,offset)
                                    in ([rewInstr,newInstr]@instructions, newTemp::temps)end
                                  | false => (case isSrc of
                                                true =>
                                                  let 
                                                    val s' = map (fn n => if n = tmp then newTemp else n) s
                                                    val newInstr = OPER {assem="movq "^its(offset)^"(%'s0), %'d0\n",
                                                                         dst=[newTemp],src=[fp],jump=NONE}
                                                    val rewInstr = OPER {assem=a,dst=d,src=s',jump=j}
                                                    val (instructions, temps) = forEachSpilled(instr,tmp,offset)
                                                  in ([newInstr,rewInstr]@instructions, newTemp::temps)end
                                                | false => let
                                                             val (instructions, temps) = forEachSpilled(instr,tmp,offset)
                                                           in (i::instructions,temps) end))end)end)
        | MOVE {assem=a,dst=d,src=s} => 
          (let
             val isDst = (d = tmp)
              val isSrc = (s = tmp)
           in (case (isDst andalso isSrc) of
                 true => forEachSpilled(instr,tmp,offset)
                 | false => let
                              val newTemp = newtemp()
                            in (case isDst of
                                  true => let
                                            val rewInstr = MOVE {assem=a,dst=newTemp,src=s}
                                            val newInstr = OPER {assem="movq %'s0, "^its(offset)^"(%'s1)\n",dst=[],
                                                                 src=[newTemp,fp],jump=NONE}
                                            val (instructions, temps) = forEachSpilled(instr,tmp,offset)
                                          in ([rewInstr,newInstr]@instructions, newTemp::temps) end
                                  | false => (case isSrc of
                                                true => let
                                                          val newInstr = OPER {assem="movq "^its(offset)^"(%'s0), %'d0\n",
                                                                               dst=[newTemp],src=[fp],jump=NONE}
                                                          val rewInstr = MOVE {assem=a,dst=d,src=newTemp}
                                                          val (instructions, temps) = forEachSpilled(instr,tmp,offset)
                                                        in ([newInstr,rewInstr]@instructions, newTemp::temps)end
                                                | false => let
                                                             val (instructions, temps) = forEachSpilled(instr,tmp,offset)
                                                           in (i::instructions,temps) end))end)end)
        | _ => let
                 val (instructions, temps) = forEachSpilled(instr,tmp,offset)
               in (i::instructions,temps) end                                      
                            

  fun rewriteProgram(linstr, f, ltemp) =
    case length(!spilledNodes) of
      0 => (linstr,ltemp)
      | _ =>  let 
                val n = hd(!spilledNodes) 
                val _ = spilledNodes := tl(!spilledNodes)
                val InFrame k = allocLocal f true
                val (instructions, temps) = forEachSpilled(linstr,n,k)
              in rewriteProgram(instructions,f,ltemp@temps) end   
                            
  fun deleteCoalescedMoves(linstr) = 
    let
      fun f(n,instrs) = 
        let
          val move = buscoEnTabla(n,!natToInstr)
          fun g i = if tigerassem.equalInstr(i,move) then NONE else (SOME i)
          val maybeList = map g instrs
          fun filterNone m = m <> NONE
          fun maybeToA NONE = raise Fail ("Error deleteCoalescedMoves")
            | maybeToA (SOME a) = a
        in map maybeToA (List.filter filterNone maybeList) end 
    in foldl f linstr (!coalescedMoves) end
  
  fun makeWorkList (ini) =
    let
      val iniList = listItems ini
      
      val greaterEqualKSet = addList(emptyStr,List.filter (fn n => buscoEnTabla(n,!degree) >= K) iniList)
      val _ = spillWorkSet := greaterEqualKSet
      
      val lowerKSet = difference(ini,greaterEqualKSet)
      val lowerKList = listItems lowerKSet
      
      val lowerMoveRelSet = addList(emptyStr, List.filter (fn n => moveRelatedFun(n)) lowerKList)
      val _ = freezeWorkSet := lowerMoveRelSet
      val lowerNonMoveRelSet = difference (lowerKSet,lowerMoveRelSet)
      val _ = simplifyWorkSet := lowerNonMoveRelSet                
      val launion = union(!spillWorkSet,union(!simplifyWorkSet,!freezeWorkSet))
      val lainter = intersection(!spillWorkSet,intersection(!simplifyWorkSet,!freezeWorkSet))

      val _ = initial := emptyStr
    in () end                  
  
  fun initializeMoves() =
    let   
      val _ = workSetMoves := emptyInt
      val _ = coalescedMoves := emptyInt
      val _ = constrainedMoves := emptyInt
      val _ = frozenMoves := emptyInt
      val _ = activeMoves := emptyInt
    in () end 
  
  fun initializeNodes() =
    let 
      val _ = freezeWorkSet := emptyStr
      val _ = coalescedNodes := emptyStr
      val _ = coloredNodes := emptyStr
      val _ = selectStack := []
      val _ = spillWorkSet := emptyStr
      val _ = spilledNodes := []
      val _ = simplifyWorkSet := emptyStr
    in () end

  fun initializeTables() =
    let
      val _ = degree := tabNueva()
      val _ = color := tabNueva()
    in () end
              
  fun pintar n =
    case tabBusca(n,!color) of
      NONE => raise Fail ("Temporal sin color asignado "^n)
      | SOME c => c
                    
  fun colorear'(l,f,inicial,debug) = 
    let 
      val newList = updateProlEpil(f,l)      
      
      val _ = if debug then print("Colorear': "^tigerframe.nameViejo(f)^"\n") else ()
      
      val _ = (initializeMoves();initializeNodes();initializeTables())      
      val _ = tigerbuild.build(newList,debug) 
      val _  = invMoves()       
          
      val _ = initial := inicial
      val _ = degree := tabAAplica (id,Splayset.numItems,!interf)
      val _ = setOfAllTemps := addList (emptyStr, tabClaves (!interf))      
      val _ = color := fillColor(!precoloredList,!color)        
      val _ = makeWorkList(inicial)                     
      val _ = repeatUntil()       
      
      val _ = if debug then (print("\nSelectStack: (long ="^its(length(!selectStack))^")\n");printList(!selectStack);print("\n")) else ()

      val coloredNodes = assignColors(emptyStr, !selectStack)

      val _ = if debug then(print ("Coalesced:\n Nodo Alias\n"); Splayset.app  (fn n => print(n^" "^getAlias(n)^"\n")) (!coalescedNodes))
                       else ()
      val _ = if debug then print "REESCRIBIENDO PROGRAMA\n" else ()
      
      val (instructions, temps) = rewriteProgram(newList,f,[])
      val initial' = addList (union(coloredNodes,!coalescedNodes), temps)
      val _ = newTemps := addList(!newTemps,temps)
      
      val _ = if debug then (print("Temporales agregados en colorear' \n");List.app print temps;
                             print("\nSi es vacio no hizo spill\n")) else ()
      
    in if temps = [] then (pintar,deleteCoalescedMoves(instructions)) else colorear'(instructions,f, initial',debug) end  
    
  fun colorear (l,f,debug) = 
    let
      val _ = if debug then print("Colorear: "^tigerframe.nameViejo(f)^"\n") else ()
      
      val _ = (initializeMoves();initializeNodes();initializeTables())    
      val _ = tigerbuild.build(l,debug)  
      val _ = invDegree()    
      val _ = degree := tabAAplica (id,Splayset.numItems,!interf)
      val _ = setOfAllTemps := addList (emptyStr, tabClaves (!degree))      
      val inicial = difference(!setOfAllTemps,!precoloredSet)
      val _ = initial := inicial
      val _= newTemps := emptyStr 
      val _ = color := fillColor(!precoloredList,!color)            
      val _ = makeWorkList(inicial)
      val _ = repeatUntil()     
      
      val _ = if debug then (print("\nSelectStack: (long = "^its(length(!selectStack))^")\n");printList(!selectStack);print("\n"))
                       else ()
      
      val coloredNodes = assignColors(emptyStr, !selectStack)
      
      val _ = if debug then (print ("Coalesced:\n Nodo Alias\n"); Splayset.app (fn n => print(n^" "^getAlias(n)^"\n")) (!coalescedNodes)) else ()
      
      val (instructions, temps) = rewriteProgram(l,f,[])
      val initial' = addList (union(coloredNodes,!coalescedNodes), temps)
      val _ = newTemps := addList(emptyStr,temps)
      
      val _ = if debug then (print("Temporales agregados en colorear: ");List.app print temps;
                             print("\nSi es vacio no hizo spill\n")) else ()
                  
    in if temps = [] then ((pintar,deleteCoalescedMoves(instructions))) else colorear'(instructions,f, initial',debug) end  
end
