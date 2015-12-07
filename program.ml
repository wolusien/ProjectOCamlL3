open automaton.ml


let parse fd =
  let list = read fd
  in ((get_dim list),
(list_to_generation list),
(list_to_rules list))
;;

(*show the state of the first generation*)
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


type formule = Vrai | Faux
               |Var of string
               |Neg of formule 
               |Et of formule * formule 
               |Ou of formule * formule;;


(*Translate automaton for a couple (l,c) to (5-uplets int)list  *)
let traduce (auto:automaton) dim l c = 
  let list = ref [] in 
  for z=0 to ((Array.length auto)-1)  
  do
      let (nord,est,sud,ouest,cell)=(auto.(z))
      in  let f= pos nord (index dim (north_index dim l c))
      and g= pos est (index dim (right_index dim l c))
      and h= pos sud (index dim (south_index dim l c))
      and i= pos ouest (index dim (left_index dim l c))
      and j= pos cell (index dim (l,c))
          in list:=(f,g,h,i,j)::!list
  done;
  List.rev(!list)
;;


traduce auto dim 0 0;; 

(*Translate automaton for a couple (l,c) to (formula)list  *)
let auto_to_formula (auto:automaton) dim l c =
  let var_to_lit num =
    if(num<0) then Neg(Var("x"^string_of_int((-1)*num)))
    else Var("x"^string_of_int(num))
  in let ind_to_formula (a,b,c,d,e) = 
       let i= var_to_lit a
       and j= var_to_lit b
       and k= var_to_lit c
       and l= var_to_lit d
       and m= var_to_lit e
       in Neg(Et(i,(Et(Et(j,k),Et(l,m))))) 
     in let l = traduce auto dim l c
        in let rec aux l1 l2 =
          match l1 with
          |[] -> l2
          |h::q -> aux q ((ind_to_formula h)::l2)
           in List.rev(aux l [])
;;
