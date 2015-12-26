
type state = Val of char;;

type generation = state array array;;

type rule = state*state*state*state*state;;
 
type automaton = rule array ;;

type 'a option = None | Some of 'a;;

(*_________________________________________________________________________*)
  (*_____________________I.Initialization__________________________________*)
(*_________________________________________________________________________*)

let file_name = ref ("testgen");; 

let parse fd =
  let list = readf fd
  in let deb_generation = (get_string_line list "Regles")
  and fin = (get_string_line list "GenerationZero")
     in let l = get_lines list deb_generation fin
  in ((get_dim list),
(list_to_generation list),
(list_to_rules l))
;;

(*______________________________________________________________________*)
(*___________________________II.Display________________________________*)
(*______________________________________________________________________*)

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

(*______________________________________________________________________*)
(*___________________________III.Simulate________________________________*)
(*______________________________________________________________________*)

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

(*______________________________________________________________________*)
(*_______________________IIII.Modeling__________________________________*)
(*______________________________________________________________________*)

type formule = Vrai | Faux
               |Var of string
               |Neg of formule 
               |Et of formule * formule 
               |Ou of formule * formule;;

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

(*Generate all the rules such as the current cell has the state c*) 
let gen_rules c  = 
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

(*Add list of disjunction l2_form to l1_form*)
let rec fus l1_form l2_form =
  match l1_form with
  |[] -> l2_form
  |h::q -> fus q (h::l2_form)
;;

(*Create a list of disjunction prob ici*)
let stables auto dim =
  let l = ref []
  in for i=0 to (dim-1)
    do for j=0 to (dim-1)
      do l := ((fus (tradaux (complementaire auto) dim i j) (tradaux (extract_rules auto 'D') dim i j))::(!l))
      done;
    done; (List.flatten (!l))
;; 

(*______________________________________________________________________*)
(*______________________V.Find the stable generation____________________*)
(*______________________________________________________________________*)


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
let create_dimacs liste  =
  let dim = dimGrid liste
  in let list = tradmin liste
     in let n = List.length list 
     and fd = open_out("entree.dimacs")
        in let rec aux file_desc l =
             match l with 
             |[] -> close_out file_desc
             |h::q -> output_string file_desc (h^" 0\n"); aux file_desc q 
           in output_string fd ("p cnf "^(string_of_int dim)^" "^(string_of_int n)^"\n"); aux fd list
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
(*Update the number of clauses in entree.dimacs*)
let modif_nb_clauses deb_dimacs =
    let rec aux s n =
      if(n>0) then 
        if((String.get s n)=' ') then n
        else aux s (n-1)
      else n
    in let firstspace = aux deb_dimacs ((String.length deb_dimacs)-1) 
       in let nbclauses = (int_of_string (String.sub deb_dimacs (firstspace +1) ((String.length deb_dimacs)-firstspace-1)))+1
          in let str = (String.sub deb_dimacs 0 (firstspace+1))
             in str^(string_of_int nbclauses)      
;;


(*Modif the dimacs file*)
let modif_dimacs list dim =
  let n = List.length list 
  and fd = open_out("entree.dimacs")
  in let rec aux file_desc l =
       match l with 
       |[] -> close_out file_desc
       |h::q -> output_string file_desc (h^"\n"); aux file_desc q 
     in aux fd list
;;

(*Update entree.dimacs by adding the rule get by the solution of minisat*)
let modif_entree () = 
   let list = readf (open_in "entree.dimacs")
   in let deb_dimacs = modif_nb_clauses (List.hd list)
      in let l = readf (open_in "soluce.txt")
         in let sol = (List.nth l 1)
            in let negsol = [((negate_sol sol)^" 0")]
               in let l1 = List.tl list
                  in let l2 = (deb_dimacs::l1)
                     in let list_finale = l2@negsol 
                        in modif_dimacs list_finale dim
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
          print_string "Il n'existe pas de génération stable pour votre automate !\n\n";
          restart "Non"
        end 
    else 
      begin 
        print_string "<Fin de recherche de génération stable>\n";
        exit(0)
      end 
  in let aux a = 
       ()
     in aux (restart "Oui")       
;;

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
  
