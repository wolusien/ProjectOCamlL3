
type state = Val of char

type generation = state array array

type rule = state*state*state*state*state
 
type automaton = rule array 

type 'a option = None | Some of 'a

exception IncorrectFile

exception SyntaxFile

(*_________________________________________________________________________*)
  (*_____________________I.Initialization__________________________________*)
(*_________________________________________________________________________*)

(*Read file and store it on a list*)
val readf : in_channel -> string list = <fun>

(*Return the dimension of the grid*)
val get_dim : string list -> int = <fun>

(*Get the lines between debut and fin*)
val get_string_line : 'a list -> 'a -> int = <fun>

(*Get the lines between debut and fin*)
val get_lines : 'a list -> int -> int -> 'a list = <fun> 

(*Convert string to  state list*)
val string_to_state_list : string -> state list = <fun>

(*Convert string list to generation*)
val list_to_generation : string list -> generation = <fun>

(*Convert list of string to automaton*) 
val list_to_rules : string list -> automaton = <fun>

val parse : in_channel -> int * generation * automaton =<fun>

(*______________________________________________________________________*)
(*___________________________II.Display________________________________*)
(*______________________________________________________________________*)

val print : state -> unit = <fun>

(*show the state of the first generation*)
val show_generation : state array array -> unit = <fun>

(*______________________________________________________________________*)
(*___________________________III.Simulate________________________________*)
(*______________________________________________________________________*)

(*Getters for neighbors of a state*)
val right_cell : 'a array array -> int -> int -> 'a = <fun>
val left_cell : 'a array array -> int -> int -> 'a = <fun>
val north_cell : 'a array array -> int -> int -> 'a = <fun>
val south_cell : 'a array array -> int -> int -> 'a = <fun>

(*Convert Neighbors states and current state to a rule*)
val voisin : state array array -> int -> int -> rule = <fun>

(*Test if an automaton contains a rule*)
val contains_rule : 'a array -> 'a -> bool = <fun>


val  next_generation : state array array -> rule array -> generation = <fun>

(*______________________________________________________________________*)
(*_______________________IIII.Modeling__________________________________*)
(*______________________________________________________________________*)

type formule = Vrai | Faux
               |Var of string
               |Neg of formule 
               |Et of formule * formule 
               |Ou of formule * formule;;

(*Get the number of the case given by line l and column c*)
val index : int -> int * int -> int = <fun>

(*Get the index i and j which represent the line and the column of the case number*)
val indexation : int -> int -> int * int = <fun>

(*Getters for neighbors of a couple (i,j)*)
let right_index : int -> 'a -> int -> int * 'a = <fun>

(*gives the index of the north neighbor *)
val north_index : int -> int -> 'a -> int * 'a = <fun>

(*gives the index of the south neighbor*)
val south_index : int -> int -> 'a -> int * 'a = <fun>

(*gives the index of the left neighbor*)
val left_index : int -> int -> 'a -> int * 'a = <fun>

(*Translate the state on num*)
val neg_literal : state -> int -> formule = <fun>

(*Extract all the rules end by c*)
val extract_rules : automaton -> char -> automaton = <fun>

(*Generate all the rules such as the current cell has the state c*) 
val gen_rules : string -> string list = <fun>

(*Generate automaton complementaire*)
val complementaire : automaton -> automaton = <fun>

(*Create a formule for a  case of number num*)
val tradaux : automaton -> int -> int -> int -> formule list = <fun>

(*Add list of disjunction l2_form to l1_form*)
val fus : 'a list -> 'a list -> 'a list = <fun>

(*Create a list of disjunction prob ici*)
val stables : automaton -> int -> formule list = <fun>

(*______________________________________________________________________*)
(*______________________V.Find the stable generation____________________*)
(*______________________________________________________________________*)

val dimGrid : formule list -> int = <fun>

(*Translate a formule to string containing number instead of xnum*)  
val tradmin : formule list -> string list = <fun> 

(*Generate the dimacs file*)
val create_dimacs : formule list -> unit = <fun>

(*Convert string with good syntax to list of int*)
val tradsol : string -> int list = <fun>

(*Create the generation given by a list of int*)
val create_gen : int list -> state array array = <fun>

val negate_sol : string -> string = <fun>

(*Update the number of clauses in entree.dimacs*)
val modif_nb_clauses : string -> string = <fun>

(*Modif the dimacs file*)
val modif_dimacs : string list -> 'a -> unit = <fun>

(*Update entree.dimacs by adding the rule get by the solution of minisat*)
val modif_entree : unit -> unit = <fun>

val show_stables : unit -> unit = <fun>

