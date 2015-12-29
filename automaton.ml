
type state = Val of char;;

type generation = state array array;;

type rule = state*state*state*state*state;;
 
type automaton = rule array ;;

type 'a option = None | Some of 'a;;

let (dim,gen,auto)= parse (open_in (!file_name));;

(*contains_rule auto (Val('A'),Val('A'),Val('A'),Val('D'),Val('d'));;*)

(*show_generation gen;;

show_generation (next_generation gen auto);;*)


(*_________________________________________________________________________*)
(*____________________________tests________________________________________*)
(*_________________________________________________________________________*)

let (dim,gen,auto)= parse (open_in (!file_name));;
(*contains_rule auto (Val('A'),Val('A'),Val('A'),Val('D'),Val('d'));;*)

(*show_generation gen;;

show_generation (next_generation gen auto);;*)
create_dimacs (stables auto dim);;
(*negate_sol "1 2 3 4 0";;*)
(*show_stables ();;*)     
  
