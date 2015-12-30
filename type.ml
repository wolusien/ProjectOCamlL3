type state = Val of char;;

type generation = state array array;;

type rule = state*state*state*state*state;;
 
type automaton = rule array ;;

type 'a option = None | Some of 'a;;

exception IncorrectFile;;

exception SyntaxFile;;

type formule = Vrai | Faux
               |Var of string
               |Neg of formule 
               |Et of formule * formule 
               |Ou of formule * formule;;

let file_name= ref(Sys.argv.(1));;
