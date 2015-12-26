automaton: automaton.cmo program.cmo showGrid.cmo
	ocamlmktop -o automaton graphics.cma  automaton.cmo program.cmo showGrid.cmo


automaton.cmo: automaton.ml 
	ocamlc -c automaton.ml


program.cmo: program.ml 
	ocamlc -c program.ml

showGrid.cmo: showGrid.ml automaton.cmi program.cmi
	ocamlc -c showGrid.ml

clean:
	rm *.cmi *.cmo
