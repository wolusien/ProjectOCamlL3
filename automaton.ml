open Type;;
open Program;; 
      
show_generation gen;;
let list_conjonctives = stables auto dim;;
create_dimacs list_conjonctives;;
show_stables();;







