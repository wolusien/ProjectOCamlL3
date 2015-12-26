type state = Val of char

type generation = state array array

type rule = state*state*state*state*state
 
type automaton = rule array 

type 'a option = None | Some of 'a

type formule = Vrai | Faux
               |Var of string
               |Neg of formule 
               |Et of formule * formule 
               |Ou of formule * formule;;

exception IncorrectFile;;
exception SyntaxFile;;

(*_________________________________________________________________________*)
  (*______________________I.Initialization_________________________________*)
(*_________________________________________________________________________*)

(*Read file and store it on a list*)
let readf file_desc = 
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
      else if((String.get s n)=='D') then aux s (n+1) (l1@[Val('D')])
      else raise SyntaxFile
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
let list_to_rules l =
  let rec aux l1 l2 =
    match l1 with
    |[] -> l2
    |h::q ->aux q ((Val(String.get h 0),Val(String.get h 1),Val(String.get h 2),Val(String.get h 3),Val(String.get h 4))::l2)
  in ((Array.of_list (aux l [])):automaton)
;;

(*______________________________________________________________________*)
(*___________________________III.Simulate________________________________*)
(*______________________________________________________________________*)

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

(*______________________________________________________________________*)
(*_______________________IIII.Modeling__________________________________*)
(*______________________________________________________________________*)



(*Get the number of the case given by line l and column c*)
let index taille (l,c) = (l)*taille + (c+1);;

(*Get the index i and j which represent the line and the column of the case number*)
let indexation taille num = 
  if ((num mod taille)<>0) 
  then ((num/taille), ((num mod taille)-1))
(*correct the problem with num n*taille*)
  else if((num/taille)<taille) then (((num/taille)-1), (taille-1)) 
(*correct the problem with num taille*taille*) 
  else ((taille-1),(taille-1));;

(*Getters for index neighbors of a couple (i,j)*)
let right_index dim i j =
  if((j+1)<dim) then (i, (j+1))
  else (i,0)
;;

let left_index dim i j = 
  if((j-1)>=0) then (i,(j-1))
  else (i,(dim-1))
;;

let north_index dim i j = 
  if((i-1)>=0) then ((i-1),j)
  else ((dim-1),j)
;;

let south_index dim i j = 
  if((i+1)<dim) then ((i+1),j)
  else (0,j)
;;

(*______________________________________________________________________*)
(*______________________V.Find the stable generation____________________*)
(*______________________________________________________________________*)

let dimGrid l = 
  let rec aux f l1 =
    match f with
    |Vrai|Faux -> l1
    |Var(w)-> 
      if(not(List.exists (fun x ->(w=x)) l1)) then (w::l1)
      else l1
    |Neg(g) -> aux g l1
    |Ou(a,b) -> aux a (aux b l1)
    |Et(a,b) -> aux a (aux b l1)
  in 
  let rec aux1 l2 l3 =
    match l2 with
    |[] -> l3
    |h::q -> aux1 q (aux h l3)
  in let listo = aux1 l []
     in List.length listo
;;
(*Convert string with good syntax to list of int*)
let tradsol sol =
  let rec aux s l n =
    if(n<((String.length s)-2)) then 
      if((String.get s n)=' ') then let p = (String.index_from s (n+1) ' ') in let str = String.sub s (n+1) (p-n-1) in aux s (str::l) (n+1)
      else aux s l (n+1)
    else l
  in let first = (String.index_from sol 0 ' ') 
     in let l = aux sol [(String.sub sol 0 first)] 0
        in let rec aux1 l_str l2 =
             match l_str with 
             |[] -> l2
             |h::q -> aux1 q ((int_of_string h)::l2)
           in aux1 l []         
;;  


(*Create a list with the negation of the var in soluce*) 
let negate_sol s = 
  let list1 = tradsol s
  in let rec aux l1 l2 =
       match l1 with 
       |[] -> l2
       |h::q -> aux q ((-h)::l2)
     in let list2 = aux list1 []
           in let rec aux1 l l3 = 
                match l with
                |[] -> l3
                |h::q -> aux1 q ((string_of_int h)::l3)
              in let list3 = aux1 list2 []
                 in String.concat " " list3
;;
