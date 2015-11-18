type state = Val of char;;

type generation = Gen of state array array;;

type rule = state*state*state*state*state;;
 
type automaton = rule array ;;

type 'a option = None | Some of 'a;;

(*Lis un fichier et le stock dans une liste*)
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

(*Retourne la dimension de la grille*)
let get_dim list = int_of_string (List.hd list);;

(*Recupere la ligne d'une chaine de caractères*)
let get_string_line list string =
  let rec aux l s a = 
    match l with 
    |[] -> failwith("list vide")
    |h::q -> if(h=s) then (a+1)
      else aux q s (a+1)
  in aux list string 0;;

(*Recupere les lignes entre debut et fin*)
let get_lines list debut fin =
  let rec aux l1 d f l2 a =
    if (a>(d-1) && (a+1)<f) then 
      aux l1 d f ((List.nth l1 a)::l2) (a+1) 
    else l2
  in let l=aux list debut fin [] debut
     in List.rev l;;

(*convertion d'un string en  state list*)
let string_to_state_list string =
  let rec aux s n l1 =
    if(n<(String.length s)) then 
      if((String.get s n)=='A') then aux s (n+1) (l1@[Val('A')])
      else aux s (n+1) (l1@[Val('D')])
    else l1
  in aux string 0 []
;;



(*Translate string list to generation*)
let list_to_generation list = 
  let rec aux1 l2 t n = (*creation d'une ligne de state du tableau generation*)
    match l2 with 
    |[] -> t
    |h::q -> t.(n) <- h; aux1 q t (n+1)
  (*Récupération de la génération Zero*)   
  in let deb_generation = (get_string_line list "GenerationZero")
  and fin = List.length list
     in let l = get_lines list deb_generation (fin+1)(*Liste contenant la génération Zero*)
        (*Creation du tableau tab qui contiendra la génération Zero*)
        in let tab = (Array.init (List.length l) (fun x -> Array.init (List.length l) (fun x -> Val('A'))))
           (*Remplissage de tab avec la génération Zero*)
           in let aux2 l3 t1 = 
                for i=0 to (List.length l3) 
                do t1.(i) <- aux1 (string_to_state_list (List.nth l3 0)) (Array.init (List.length l3) (fun x -> Val('A'))) 0
                done
              in aux2 l tab;(*Retourne tab*) Gen(tab)
;;

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
           in (aux1 l tab 0)
;;


let parse fd =
  let list = read fd
  in ((get_dim list),
(list_to_generation list),
((list_to_rules list):automaton))
;;
