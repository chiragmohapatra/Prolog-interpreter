open Assign5
open Printf

(* print the substitution *)
let rec print_sub (s:substitution) varls =
  match s with
  [] -> printf ""
| x::xs -> match x with
          (V(str),Node(str1,ts)) -> if(search varls str) then (printf "%s = %s  " str str1; print_sub xs varls)
                                    else print_sub xs varls
        | _ -> printf ""
  
let rec print_sub_ls1 (ss:substitution list) varls =
  match ss with
  [] -> printf "no.\n"; printf "\n?-"; flush stdout;
| x::xs -> print_sub x varls; flush stdout;
           let lexbuf = Lexing.from_channel stdin in
           let result = Parser.main Lexer.token lexbuf in
                match result with
                (Node("end",[]),[]) -> (); printf "\n?-"; flush stdout;
              | (Node("next",[]),[]) -> print_sub_ls1 xs varls; flush stdout;
              | _ -> printf "unrecognized.\n"; flush stdout;;

let rec print_sub_ls (ss:substitution list) varls =
  match varls with
  [] -> printf "yes.\n"; printf "\n?-"; flush stdout;
| _ -> print_sub_ls1 ss varls;;

let rec help_unify termls clausels = 
  let s = unifier_helper1 termls clausels clausels in
      match s with
      [] -> printf "no.\n"; printf "\n?-"; flush stdout;
    | [[]] -> printf "yes.\n"; printf "\n?-"; flush stdout;
    | _ -> print_sub_ls s (List.flatten (vars_ls termls));;

(* return table after reading from rules file *)
let get_table () =
  let filename = Sys.argv.(1) in
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  let rec createTable clause_ls =
    let result = Parser.main Lexer.token lexbuf in
      match result with
        (Node("end_of_file",[]),[]) -> clause_ls
      | _ -> createTable (result::clause_ls)
    in
    createTable []
;;

(* return variable list associated with table *)
let rec get_vars_list table =
  match table with
  [] -> []
| c::cls -> let (t,ts) = c in
            union_ls (List.flatten (vars_ls (t::ts))) (get_vars_list cls);;

(* returns substitution substituting variables with "0" , "1" , "2" etc *)
let form_sub_list varls =
  let rec form_sub_helper vls n =
        match vls with
        [] -> []
      | x::xs -> (V(x),V(string_of_int n))::(form_sub_helper xs (n+1))
  in
  form_sub_helper varls 0
;;

(* replace the variable names in rules such that they never interfere with user input variables , replace them with "0" , "1" , "2" etc*)
let modify_table_variables table =
  let sub1 = form_sub_list (get_vars_list table) in
    let rec modify_table_helper tab newtab sub =
      match tab with
      [] -> newtab
    | (t,ts)::cls -> modify_table_helper cls (((subst t sub),(subst_ls ts sub))::newtab) sub
    in
    modify_table_helper table [] sub1
;;

let _ = 
  printf "?-" ; flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  let clause_ls = modify_table_variables (get_table ()) in
  while true do
    let result = Parser.main Lexer.token lexbuf in
      match result with
        (Node("halt",[]),[]) -> exit(0)
      | (Node(str,ts),[]) -> help_unify [Node(str,ts)] clause_ls
      | (Node("goal",[]),termls) -> help_unify termls clause_ls 
      | _-> Printf.printf "INVALID INPUT GOAL\n";Printf.printf "\n?-"; flush stdout;
  done
;;

      

