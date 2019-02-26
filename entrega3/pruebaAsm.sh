#!/bin/bash

echo "HAGO MAKE" > resultado.txt
make 

for (( i=1; i<=19; i++))
	do
			echo "**********************************************" >> resultado.txt;
			echo "TEST "$i >> resultado.txt;
			./tiger ../tests/good/test$i.tig -asm 
			echo "**********************************************" >> resultado.txt; 
			gcc -s -no-pie ../tests/TestAssm/prueba.s ../tests/TestAssm/runtime.o
			echo " " >> resultado.txt;
			./a.out >> resultado.txt;
			echo " " >> resultado.txt;
	done

echo "HAGO MAKE" > resultado1.txt
for (( i=21; i<=24; i++))
	do
			echo "**********************************************" >> resultado1.txt;
			echo "TEST "$i >> resultado1.txt;
			./tiger ../tests/good/test$i.tig -asm
			echo "**********************************************" >> resultado1.txt; 
			gcc -s -no-pie ../tests/TestAssm/prueba.s ../tests/TestAssm/runtime.o
			echo " " >> resultado1.txt;
			./a.out >> resultado1.txt;
			echo " " >> resultado1.txt;
	done

echo "TERMINE"
