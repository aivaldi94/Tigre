#!/bin/bash
# ejecutar: bash pruebaAsm.sh 2> /dev/null si quiero evitar ver los segfault en terminal.

echo "HAGO MAKE" > resultado.txt
make 

for (( i=1; i<=24; i++))
	do
		# si es el test 20 (merge) lo ignoro porque hay que pasarle los argumentos
		if [ $i -ne 20 ];			
			then echo "TEST "$i; 			
				./tiger ../tests/good/test$i.tig -asm 2>&1 >/dev/null;			
				echo "**********************************************" >> resultado.txt;
				echo "TEST "$i >> resultado.txt;			
				gcc -s ../tests/TestAssm/prueba.s ../tests/TestAssm/runtime.o;					
				./a.out &>> resultado.txt; 
				if [ "$?" -eq 139 ];
				then echo "segfault" &>> resultado.txt;
				fi;
				echo " " >> resultado.txt;
			fi;
	done

echo "TERMINO!"
