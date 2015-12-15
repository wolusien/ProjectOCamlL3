type state = Val of char

type generation = state array array

type rule = state*state*state*state*state
 
type automaton = rule array 

type 'a option = None | Some of 'a

exception IncorrectFile

exception SyntaxFile

(*Read file and store it on a list*)
val reado : in_channel -> string list = <fun>

(*Return the dimension of the grid*)
val get_dim : string list -> int = <fun>

(*Get the lines between debut and fin*)
val get_line : 'a list -> 'a -> int = <fun>

(*Get the lines between debut and fin*)
val get_lines : 'a list -> int -> int -> 'a list = <fun> 

(*Convert string to  state list*)
val string_to_state_list : string -> state list = <fun>

(*Convert string list to generation*)
val list_to_generation : string list -> generation = <fun>

(*Convert list of string to automaton*) 
val list_to_rules : string list -> automaton = <fun>

val parse : in_channel -> int * generation * automaton =<fun>

val print : state -> unit = <fun>

(*show the state of the first generation*)
val show_generation : state array array -> unit = <fun>

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

(*convert the number of the case to a postive or negative one *)
val pos : state -> int -> int  = <fun>




