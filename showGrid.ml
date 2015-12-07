(*affichage
  interface graphique
  tests
*)
open automaton.ml
open program.ml

let (dim,gen,auto)= parse (open_in "testgen");;

contains_rule auto (Val('A'),Val('A'),Val('A'),Val('D'),Val('d'));;

show_generation gen;;

show_generation (next_generation gen auto);;

pos (Val('D'):state) 12;;

auto_to_formula auto dim 0 0;;


(*_________________________________________________________________*)

(* je n'ai pas encore fini je dois faire plein de modif pour que la grille soit au point **)
(*la j'arrive a afficher une grille *)

open Graphics;;
open_graph " 600x1000";;


type relief = Top | Bot | Flat ;;
type box_config =
  { x:int; y:int; w:int; h:int; bw:int; mutable r:relief ;
    b1_col : Graphics.color ;
    b2_col : Graphics.color ;
    b_col : Graphics.color} ;;

let draw_box_outline bcf col = 
  Graphics.set_color col ;
  draw_rect bcf.x bcf.y bcf.w  bcf.h ;;


let draw_box bcf = 
   let x1 = bcf.x and y1 = bcf.y in
   let x2 = x1+bcf.w and y2 = y1+bcf.h in 
   let ix1 = x1+bcf.bw and ix2 = x2-bcf.bw 
   and iy1 = y1+bcf.bw and iy2 = y2-bcf.bw in 
   let border1 g =
     Graphics.set_color g;
     Graphics.fill_poly 
       [| (x1,y1);(ix1,iy1);(ix2,iy1);(ix2,iy2);(x2,y2);(x2,y1) |] 
   in
   let border2 g = 
     Graphics.set_color g;
     Graphics.fill_poly 
       [| (x1,y1);(ix1,iy1);(ix1,iy2);(ix2,iy2);(x2,y2);(x1,y2) |]
   in
   Graphics.set_color bcf.b_col;
   ( match bcf.r with
         Top  -> 
           Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1) ;
           border1 bcf.b1_col ; 
           border2 bcf.b2_col
       | Bot  -> 
           Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1) ;
           border1 bcf.b2_col ; 
           border2 bcf.b1_col
       | Flat -> 
           Graphics.fill_rect x1 y1 bcf.w bcf.h ) ;
   draw_box_outline bcf Graphics.black ;;

let erase_box bcf = 
    Graphics.set_color bcf.b_col ; 
    Graphics.fill_rect (bcf.x+bcf.bw) (bcf.y+bcf.bw) 
                       (bcf.w-(2*bcf.bw)) (bcf.h-(2*bcf.bw)) ;;


type position = Left | Center | Right ;; 


let set_grey x =  (Graphics.rgb x x x) ;;
  let grey1= set_grey 255 and grey2= set_grey 255 and grey3= set_grey 255 ;;

  (* nb_col est le nombre de colonne
     n=nb_colone*nb_colonne -1
     sep: separateur entre deux box
     b:le point de debut, largeur , hauteur et
  *)
let rec create_grid nb_col n sep b  =
   if n < 0 then []
   else 
     let px = n mod nb_col and py = n / nb_col in
     let nx = b.x +sep + px*(b.w+sep)
     and ny = b.y +sep + py*(b.h+sep) in
     let b1 = {b with x=nx; y=ny} in
     b1::(create_grid nb_col (n-1) sep b) ;;


let vb = 
  let b =  {x=0; y=300; w=50;h=50; bw=0;
            b1_col=grey1; b2_col=grey3; b_col=grey2; r=Top} in 
  Array.of_list (create_grid 7 48 2  b) ;;


Array.iter draw_box vb ;

