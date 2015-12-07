(*affichage
  interface graphique
  tests
*)
open automaton.ml
open program.ml

let (dim,gen,auto)= parse (open_in "testgen");;

contains_rule auto (Val('A'),Val('A'),Val('A'),Val('D'),Val('d'));;

show_generation gen;;

show_generation (next_generation gen auto);;

pos (Val('D'):state) 12;;

auto_to_formula auto dim 0 0;;


