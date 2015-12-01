type state = Val of char;;

type generation = state array array;;

type rule = state*state*state*state*state;;
 
type automaton = rule array ;;

type 'a option = None | Some of 'a;;

exception IncorrectFile;;

(*Read file and store it on a list*)
let read file_desc = 
  let rec add list =
       let result = 
         try Some(input_line file_desc)
         with End_of_file -> None
       in match result with 
       |None -> close_in file_desc; list
       |Some(r) -> add (list@[r])
     in add []
;;

(*Return the dimension of the grid*)
let get_dim list = int_of_string (List.hd list);;

(*Get the number of line of str in a list of string*)
let get_string_line list str =
  let rec aux l s a = 
    match l with 
    |[] -> raise IncorrectFile
    |h::q -> if(h=s) then (a+1)
      else aux q s (a+1)
  in aux list str 0;;

(*Get the lines between debut and fin*)
let get_lines list debut fin =
  let rec aux l1 d f l2 a =
    if (a>(d-1) && (a+1)<f) then 
      aux l1 d f ((List.nth l1 a)::l2) (a+1) 
    else l2
  in let l=aux list debut fin [] debut
     in List.rev l;;

(*Convert string to  state list*)
let string_to_state_list string =
  let rec aux s n l1 =
    if(n<(String.length s)) then 
      if((String.get s n)=='A') then aux s (n+1) (l1@[Val('A')])
      else aux s (n+1) (l1@[Val('D')])
    else l1
  in aux string 0 []
;;

(*Convert string list to generation*)
let list_to_generation list = 
(*Get generation Zero*)   
  let deb = (get_string_line list "GenerationZero")
  and fin = List.length list
  in let l = get_lines list deb (fin+1)
     in let tab = (Array.init (List.length l) (fun x -> Array.init (List.length l) (fun x -> Val('A'))))
        in let rec aux1 l2 t n = (*create a state array *)
             match l2 with 
             |[] -> t
             |h::q -> t.(n) <- h; aux1 q t (n+1)
           in let aux2 l3 t1 = (*Fil the state array array with the generation Zero*) 
                for i=0 to ((List.length l3)-1) 
                do t1.(i) <- aux1 (string_to_state_list (List.nth l3 i)) (Array.init (List.length l3) (fun x -> Val('A'))) 0
                done;t1
              in ((aux2 l tab):generation)(*Return a generation*)
;;

(*Convert list of string to automaton*) 
let list_to_rules list = 
  let rec aux1 l2 t n = 
    match l2 with 
    |[] -> t
    |h::q -> t.(n) <- (Val(String.get h 0),Val(String.get h 1),Val(String.get h 2),Val(String.get h 3),Val(String.get h 4)); 
      aux1 q t (n+1)
  in let deb_generation = (get_string_line list "Regles")
  and fin = (get_string_line list "GenerationZero")
     in let l = get_lines list deb_generation fin
        in let tab = Array.init (List.length l) (fun x ->(Val('A'),Val('A'),Val('A'),Val('A'),Val('A')))
           in ((aux1 l tab 0):automaton)
;;

let parse fd =
  let list = read fd
  in ((get_dim list),
(list_to_generation list),
(list_to_rules list))
;;

let print state = 
  match state with 
  |Val(a) -> print_char a
;;

let show_generation tab = 
  for i=0 to ((Array.length tab)-1)
  do
    print_string "\n+";
    for k=0 to ((Array.length tab)-2)
    do print_string "--";
    done;
    print_string "-+\n";
    for j=0 to ((Array.length tab)-1)
    do print_string "|";print tab.(i).(j);
    done;print_string "|";
  done;
  print_string "\n+";
  for k=0 to ((Array.length tab)-2)
  do print_string "--";
  done;
  print_string "-+\n";
;;

(*Getters for neighbors of a state*)
let right_cell tab i j =
  if((j+1)<Array.length tab.(0)) then tab.(i).(j+1)
  else tab.(i).(0)
;;

let left_cell tab i j = 
  if((j-1)>=0) then tab.(i).(j-1)
  else tab.(i).((Array.length tab.(0))-1)
;;

let north_cell tab i j = 
  if((i-1)>=0) then tab.(i-1).(j)
  else tab.((Array.length tab)-1).(j)
;;

let south_cell tab i j = 
  if((i+1)<(Array.length tab)) then tab.(i+1).(j)
  else tab.(0).(j)
;;

(*Convert Neighbors states and current state to a rule*)
let voisin tab l c = 
  let cell = tab.(l).(c)(*cellule courante*)
  in (((north_cell tab l c),(right_cell tab l c),(south_cell tab l c),(left_cell tab l c),cell):rule)
;;

(*Test if an automaton contains a rule*)
let contains_rule tab rule =
  let list = Array.to_list tab
     in List.mem rule list
;;
  

let next_generation tab rules  = 
  let newtab = Array.init (Array.length tab) (fun x -> (Array.init (Array.length tab) (fun x -> Val('D'))))
  in for i=0 to ((Array.length tab)-1)
    do for j=0 to ((Array.length tab)-1)
      do if(contains_rule rules (voisin tab i j)) then newtab.(i).(j) <- Val('A')
        else ();
      done;
    done;
  (newtab:generation)
;;

let (dim,gen,auto)= parse (open_in "testgen");;

contains_rule auto (Val('A'),Val('A'),Val('A'),Val('D'),Val('d'));;

show_generation gen;;

show_generation (next_generation gen auto);;

type formule = Vrai | Faux
               |Var of string
               |Neg of formule 
               |Et of formule * formule 
               |Ou of formule * formule;;
