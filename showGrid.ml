open Automaton;;
open Program;;
open Type;;
open Graphics;;


 open_graph " 600x600";; 
let background_color = (rgb 65 126 190);;

(*__________________________________*)
(*     initialize a grid             *)
(*__________________________________*)

let draw_grid () = 
  set_window_title "Automate cellulaire";
  set_color background_color;
  fill_rect 0 0 600 600;
  set_line_width 5;
  set_color black;
  moveto 53 547;
  lineto 53 53;
  lineto 547 53;
  lineto 547 547;
  lineto 53 547;
  set_line_width 2;
  let x = ref ((494/dim)+53) in
  for i=0 to (dim-1) do
    moveto !x 547;
    lineto !x 53;
    x := !x+(494/dim);
  done;
  x:= (547-(494/dim));
  for i=0 to (dim-1) do
    moveto 53 !x;
    lineto 547 !x;
     x := !x-(494/dim);
  done;
;;
draw_grid ();;
(*__________________________________*)
(* fill the grid with our generation*)
(*__________________________________*)

let fill_grid m dim  =
let  y = ref (547-(494/dim)) in     
  for  i=0 to (dim-1) do
     let x1 = ref 53 in 
    for j=0 to (dim-1) do
        begin      
	match m.(i).(j) with
	|Val('A') -> set_color (black) ;
          fill_rect (!x1+1) (!y+1) ((494/dim)-5) ((494/dim)-4);
          
	|Val('D') -> set_color (white);
         fill_rect (!x1+1) (!y+1) ((494/dim)-4) ((494/dim)-4);
	 
	end;
      x1:=! x1+(494/dim);
    done;
    y:=!y-(494/dim);
  done
;;

(*_______________________________buttons____________________________________*)
(*__________________________________________________________________________*)

set_color black;;
draw_rect 0 0 600 47;;
draw_rect 0 553 600 47;;

draw_rect 50 10 200 30;;
draw_rect 350 10 200 30;;
set_color red;;
moveto 100 20;;
draw_string "START";;
moveto 400 19;;
draw_string "NEXT";;
moveto 250 567;;
draw_string "AUTOMATE CELLULAIRE";; 

(*____________________________________________________________________*)
(*______________________________events________________________________*)
(*____________________________________________________________________*)

let tabRef=(Array.init dim (fun x -> Array.init dim (fun x-> Val('A'))));;
for i=0 to (dim-1) do
  for j=0 to (dim-1)do
    tabRef.(i).(j)<- Array.get (Array.get gen (i)) (j)
  done
done
;;
let a=ref (next_generation tabRef auto);;
let inversion()=
  for i=0 to (dim-1) do
    for j=0 to (dim-1)do
      tabRef.(i).(j)<- Array.get (Array.get !a (i)) (j)
    done
  done;
a:=(next_generation !a auto)
;;

let inversion1()=
  for i=0 to (dim-1) do
    for j=0 to (dim-1)do
      tabRef.(i).(j)<- Array.get (Array.get gen (i)) (j)
    done
  done
;;

let rec next()=
  let e= wait_next_event[Button_down] in 
  let x=e.mouse_x
  and  y=e.mouse_y in
  if (x>350)&&(x<550)&&(y>10)&&(y<40) then
begin
    inversion();(fill_grid tabRef dim)
end;
  if  (x>50)&&(x<250)&&(y>10)&&(y<40) then
begin
    inversion1();(fill_grid tabRef dim);a:=(next_generation tabRef auto)
end;
next();;

next();;
