structure tigerassem = struct

  type reg = string
  type temp = tigertemp.temp
  type label = tigertemp.label

  datatype instr = OPER of {assem: string,
                            dst: temp list,
                            src: temp list,
                            jump: label list option}
                 | LABEL of {assem: string, lab: tigertemp.label}
                 | MOVE of {assem: string, 
                            dst: temp,
                            src: temp}

(* Esta función de igualdad se utiliza en tigercolor, únicamente para sacar los moves que hicieron coalesce
Compara una instrucción cualquier con un MOVE. En cualquier otro caso dará falso*)
  fun equalInstr(OPER _,_) = false
	 | equalInstr(LABEL _,_) = false
	 | equalInstr(MOVE{assem=a1,dst=d1,src=s1},MOVE{assem=a2,dst=d2,src=s2}) = a1 = a2 andalso d1 = d2 andalso s1=s2

  fun format saytemp =
    let 
    fun speak(assem,dst,src,jump) =
        let fun name(s,n) = s 
            val saylab = name    
        fun f(#"'":: #"s":: i::rest) = 
            (explode(saytemp(List.nth(src,ord i - ord #"0"))) @ f rest)
          | f( #"'":: #"d":: i:: rest) = 
            (explode(saytemp(List.nth(dst,ord i - ord #"0"))) @ f rest)
          | f( #"'":: #"j":: i:: rest) = 
            (explode((*saylab*)(List.nth(jump,ord i - ord #"0"))) @ f rest)
          | f( #"'":: #"'":: rest) = #"'" :: f rest
          | f( #"'":: _ :: rest) = raise Fail "bad Assem format"
          | f(c :: rest) = (c :: f rest)
          | f nil = nil
        in implode(f(explode assem))
        end
      in fn OPER{assem,dst,src,jump=NONE} => speak(assem,dst,src,nil)
          | OPER{assem,dst,src,jump=SOME j} => speak(assem,dst,src,j)
          | LABEL{assem,...} => assem
          | MOVE{assem,dst,src} => speak(assem,[dst],[src],nil)
     end

end

