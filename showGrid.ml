(*affichage
  interface graphoque
  tests
*)
open automaton.ml
open program.ml

let (dim,gen,auto)= parse (open_in "testgen");;

contains_rule auto (Val('A'),Val('A'),Val('A'),Val('D'),Val('d'));;

show_generation gen;;
