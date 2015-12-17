type state = Val of char;;

type generation = state array array;;

type rule = state*state*state*state*state;;
 
type automaton = rule array ;;

type 'a option = None | Some of 'a;;

exception IncorrectFile;;

exception SyntaxFile;;

let file_name = ref ("testgen");; 

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


let parse fd =
  let list = readf fd
  in let deb_generation = (get_string_line list "Regles")
  and fin = (get_string_line list "GenerationZero")
     in let l = get_lines list deb_generation fin
  in ((get_dim list),
(list_to_generation list),
(list_to_rules l))
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
  
(*Evolve a generation considering an automaton*)
let next_generation tab (rules:automaton)  = 
  let newtab = Array.init (Array.length tab) (fun x -> (Array.init (Array.length tab) (fun x -> Val('D'))))
  in for i=0 to ((Array.length tab)-1)
    do for j=0 to ((Array.length tab)-1)
      do if(contains_rule rules (voisin tab i j)) then newtab.(i).(j) <- Val('A')
        else ();
      done;
    done;
  (newtab:generation)
;;

let (dim,gen,auto)= parse (open_in (!file_name));;

(*contains_rule auto (Val('A'),Val('A'),Val('A'),Val('D'),Val('d'));;*)

(*show_generation gen;;

show_generation (next_generation gen auto);;*)

type formule = Vrai | Faux
               |Var of string
               |Neg of formule 
               |Et of formule * formule 
               |Ou of formule * formule;;

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

(*Translate the state on num*)
let neg_literal state num = 
  match state with 
  |Val(x) -> if(x='A') then Neg(Var("x"^(string_of_int num))) 
    else if(x='D') then Var("x"^(string_of_int num))
    else raise SyntaxFile
;;

(*Extract all the rules end by c*)
let extract_rules (auto:automaton) char = 
  let l = (Array.to_list auto)
  in let rec aux l1 l2 =
       match l1 with 
       |[] -> l2
       |(a,b,c,d,e)::q -> match e with
         |Val(x) -> if(x=char) then aux q ((a,b,c,d,e)::l2)
           else aux q l2
     in ((Array.of_list (aux l [])):automaton)
;;

(*Generate all the rules such as the current cell has the state c*) let gen_rules c  = 
  let aux list = List.flatten (List.map(fun x -> ["D"^x;"A"^x])  list) in aux (aux (aux (aux [c])));;

(*Generate automaton complementaire*)
let complementaire (auto:automaton) = 
  let auto1 = Array.to_list (list_to_rules (gen_rules "A"))
  in let rec aux autom l_auto2 l3 =
       match l_auto2 with
       |[] -> l3
       |h::q -> if(contains_rule autom h) then aux autom q l3
         else aux autom q (h::l3) 
     in ((Array.of_list (aux auto auto1 [])):automaton)
;;

(*Create a formule for a  case of number num*)
let tradaux (auto:automaton) dim l c =
  let au = Array.to_list auto in
  let aux ((a,b,x,y,t):rule) dim l c = 
       let f=(neg_literal a (index dim (north_index dim l c)))
       and g=(neg_literal b (index dim (right_index dim l c)))
       and h=(neg_literal x (index dim (south_index dim l c)))
       and j=(neg_literal y (index dim (left_index dim l c)))
       and k=(neg_literal t (index dim (l,c)))
       in Ou(Ou(Ou(Ou(f,g),h),j),k)
     in let rec aux2 l1 l2 = 
          match l1 with 
          |[] -> l2
          |h::q -> aux2 q ((aux h dim l c)::l2)
        in aux2 au []
;;

(*Create list disjunction between l1_form elements and l2_form elements*)
let rec disjunction l1_form l2_form =
  let rec aux f list list2 = 
    match list with
    |[] -> list2
    |h::q -> aux f q ((Ou(f,h))::list2)
  in let rec aux2 l1 l2 l3 = 
       match l1 with 
       |[] -> l3
       |h::q -> aux2 q l2 (aux h l2 l3)
     in aux2 l1_form l2_form []
;;

(*let auto1 = (tradaux (extract_rules auto 'D') 2 0 0);;

let auto2 = (tradaux (complementaire auto) 2 0 0);;

disjunction auto1 auto2;;*)


(*Create a list of disjunction*)
let stables auto dim =
  let l = ref []
  in for i=0 to (dim-1)
    do for j=0 to (dim-1)
      do l := ((disjunction (tradaux (complementaire auto) dim i j) (tradaux (extract_rules auto 'D') dim i j))::(!l))
      done;
    done; (List.flatten (!l))
;;

(*Translate a formule to string containing number instead of xnum*)  
let tradmin list_formule =
    let rec aux formule = 
      match formule with
      |Vrai -> " "
      |Faux -> " "
      |Var(x) -> String.sub x 1 ((String.length x)-1)
      |Neg(f) -> "-"^(aux f)
      |Ou(f1,f2) -> (aux f1)^" "^(aux f2)
      |Et(f1,f2) -> (aux f1)^"\n"^(aux f2)  
    in 
    let rec aux2 l1 l2 =
      match l1 with 
      |[] -> l2
      |h::q -> aux2 q ((aux h)::l2)
    in aux2 list_formule []
;;

(*Generate the dimacs file*)
let create_dimacs list dim =
  let n = List.length list 
  and fd = open_out("entree.dimacs")
  in let rec aux file_desc l =
       match l with 
       |[] -> close_out file_desc
       |h::q -> output_string file_desc (h^" 0\n"); aux file_desc q 
     in output_string fd ("p cnf "^string_of_int(dim*dim)^" "^string_of_int(n)^"\n"); aux fd list
;;

create_dimacs (tradmin (stables auto dim)) dim;;

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

(*Create the generation given by a list of int*)
let create_gen list taille  =
  let tab = (Array.init taille (fun x -> Array.init taille (fun x -> Val('D'))))
  in let rec aux l1 l2 =
       match l1 with 
       |[] -> l2
       |h::q -> if(h<0) then 
           let (l,c) = (indexation taille ((-1)*h)) 
           in aux q ((Val('D'),l,c)::l2)
         else let (l,c) = (indexation taille h) 
           in aux q ((Val('A'),l,c)::l2)
     in let list1 = aux list []
        in let rec aux1 list2 t =
             match list2 with 
             |[] -> ()
             |h::q -> let (n,l,c)=h 
                      in (*print_string "\nligne ";print_int l;print_string " colonne "; print_int c;*)
 t.(l).(c) <- n; aux1 q t
           in aux1 list1 tab; tab
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

(*negate_sol "1 2 3 4 0";;*)

(*Update entree.dimacs by adding the rule get by the solution of minisat*)
let modif_entree () = 
  let list = readf (open_in "entree.dimacs")
  in let l = readf (open_in "soluce.txt")
     in let sol = (List.nth l 1)
        in let list_finale = list@[(negate_sol sol)] 
           in create_dimacs list_finale dim
;;

let show_stables () =
  let rec restart str = 
    if(str="Oui") then 
      if((Sys.command "minisat entree.dimacs soluce.txt")=10) then 
        begin 
          print_string "Voici votre génération stable : \n";
          show_generation (create_gen (tradsol (List.nth (readf (open_in "soluce.txt")) 1)) dim);
          modif_entree ();
          print_string "Voulez-vous continuez : \nOui\tNon\n";
          restart (read_line ())
        end 
      else
        begin
          print_string "Il n'existe pas de génération stable pour votre automate !\n";
          restart "Non"
        end 
       else 
      begin 
        print_string "Fin de recherche de génération stable.\n"; 
        restart "Non"
      end 
  in let aux a = 
       ()
     in aux (restart "Oui")       
;;

show_stables ();;     
  
