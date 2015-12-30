EXEC1 = automaton
EXEC2 = grid
all: type.cmo program.cmo  automaton.cmo showGrid.cmo
	ocamlc -o $(EXEC1) unix.cma graphics.cma type.cmo program.cmo  automaton.cmo
	ocamlc -o $(EXEC2) unix.cma graphics.cma type.cmo program.cmo  showGrid.cmo
type.cmo: type.ml
	ocamlc -c type.ml

program.cmo: program.ml type.cmo
	ocamlc -c program.ml

automaton.cmo: automaton.ml program.cmo type.cmo 
	ocamlc -c automaton.ml

showGrid.cmo: showGrid.ml program.cmo type.cmo
	ocamlc -c showGrid.ml

clean:
	rm *.cmi *.cmo $(EXEC1)
	rm *.cmi *.cmo $(EXEC2)	
