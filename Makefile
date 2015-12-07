automaton: automaton.cmo program.cmo showGrid.cmo
	ocamlc -o automaton automaton.cmo program.cmo showGrid.cmo


automaton.cmo: automaton.ml automaton.cmi
	ocamlc -c automaton.ml

automaton.cmi: automaton.mli
	ocamlc automaton.mli


program.cmo: program.ml automaton.cmi
	ocamlc -c program.ml

showGrid.cmo: showGrid.ml automaton.cmi program.cmi
	ocamlc -c showGrid.ml

clean:
	rm *.cmi *.cmo
