signature tigerassem =
sig
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

	val equalInstr: (instr * instr) -> bool

	val format: (tigertemp.temp -> tigertemp.temp) -> instr -> string
end
