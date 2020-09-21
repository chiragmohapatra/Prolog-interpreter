open List;;
open Printf;;

exception NOT_UNIFIABLE

type term = V of string | Node of string*(term list);;
type signature = (string*int) list;;

type clause = term * term list;;

let rec max_ls_helper ls x =
  match ls with
  [] -> x
| y::ys -> if(y > x) then max_ls_helper ys y
           else max_ls_helper ys x;;

(*Returns the max element of the list , return 0 for empty list*)
let max_ls ls = 
  match ls with
  [] -> 0
| x::xs -> max_ls_helper xs x;;

(*Returns true if y is present in list ls and false otherwise*)
let rec search ls y =
  match ls with
  [] -> false
| x::xs -> if(x=y) then true 
           else (search xs y);;

(*Returns union of the two lists ls1 and ls2*)
let rec union_ls ls1 ls2 =
  match (ls1,ls2) with
  ([],[]) -> []
| (ls,[]) -> ls
| ([],ls) -> ls
| (x::xs,ls) -> if(not(search ls x)) then x::(union_ls xs ls)
                else union_ls xs ls;;
 
(*Returns (true,arity) if str present in signature and (false,(-1)) if not*)
let rec get_arity (signature:signature) str =
  match signature with
  [] -> (false,(-1))
| x::xs -> let (s,arity) = x in
           if(s = str) then (true,arity)
           else (get_arity xs str);;

let rec check_sig_helper signature symbol_list =
  match signature with
  [] -> true
| x::xs -> let (str,arity) = x in
           if(arity < 0) then false (*Invalid arity*)
           else if(search symbol_list str) then false (*Repeated symbol*)
           else (check_sig_helper xs (str::symbol_list));;
        
(*Checks if the signature is valid*)
let check_sig (signature:signature) =
  check_sig_helper signature [];;


(*Checks if term is well-formed*)
let rec wfterm1 (t:term) (signature:signature): bool =
  match t with
  V(x) -> true
| Node(str,term_ls) -> let (found,arity) = (get_arity signature str) in
                       if(not(found)) then false (*Not present in signature*)
                       else if((length term_ls) <> arity) then false (*List length mismatch with arity*)
                       else (wfterm_list term_ls signature)

  and wfterm_list term_ls (signature:signature) = (*Return strue if all the terms in term_ls are well-formed*)
      match term_ls with
      [] -> true
    | y::ys -> (wfterm1 y signature) && (wfterm_list ys signature);;

let wfterm (t:term) (signature:signature): bool =
  if(not(check_sig signature)) then false
  else (wfterm1 t signature);;


let rec ht (t:term) = (*Return the height if term t*)
  match t with
  V(x) -> 0 (*Variable , height 0*)
| Node(str,[]) -> 0(*Constant , height 0*)
| Node(str,term_ls) -> 1 + (max_ls (ht_ls term_ls))

and ht_ls term_ls = (*Return a list containing the heights of all the terms of term_ls*)
    match term_ls with
    [] -> []
  | y::ys -> (ht y)::(ht_ls ys);;


let rec size (t:term) = (*Returns the size of term*)
  match t with
  V(x) -> 1
| Node(str,term_ls) -> 1 + (fold_left (fun acc x -> acc + x) 0 (size_ls term_ls))

and size_ls term_ls = (*Return a list containing the sizes of all the terms of term_ls*)
    match term_ls with
    [] -> []
  | y::ys -> (size y)::(size_ls ys);;


let rec vars (t:term) = (*Returns a set containing all the variables in the term*)
  match t with
  V(x) -> [x]
| Node(str,term_ls) -> (fold_left union_ls [] (vars_ls term_ls))

and vars_ls term_ls = (*Returns a list which contains lists of all the variables in the terms of term_ls*)
    match term_ls with
    [] -> []
  | y::ys -> (vars y)::(vars_ls ys);;

type substitution = (term*term) list;; (*first term represents a variable and variable is mapped to a term*)

(*Returns term associated wth x in substitution s2*)
let rec get_from_subst s2 x = 
  match s2 with
  [] -> V(x) (*Identity if not present*)
| y::ys -> let (str,t) = y in
           if(str = V(x)) then t
           else (get_from_subst ys x);;

(*Applies substitution s on term t*)
let rec subst (t:term) (s:substitution): term = 
  match t with
  V(x) -> (get_from_subst s x) (*Get the match of x in subst s*)
| Node(str,term_ls) -> Node(str,(subst_ls term_ls s))

  and subst_ls term_ls s =
    match term_ls with
    [] -> []
  | y::ys -> (subst y s)::(subst_ls ys s);;

let subst_rev (s:substitution) (t:term) = subst t s;;

let rec dummy_subst (s:substitution) (x,t):(term*term) = (x , subst t s);; (*Applies substitution s to term t*)

let rec composition_helper (s1:substitution) (s2:substitution): substitution =
  (map (dummy_subst s2) s1);; (*Apply substitution s2 to all the result terms in s1*)

(*Return the composition of substitutions s1 and s2 , It means that the resultant substitution s should be such that it is equivalent to applying substitution s1 followed by s2*)
let composition_substitution (s1:substitution) (s2:substitution): substitution =
  union_ls (composition_helper s1 s2) s2;;

(*Returns most general unifier of terms t1 and t2*)
let rec mgu (t1:term) (t2:term): substitution =
  match (t1,t2) with
  (V(x),V(y)) -> if(x <> y) then [(V(x),V(y))] else []
| (Node(str,term_ls),V(y)) -> if(search (vars (Node(str,term_ls))) y) then [(V("dummy"),V("not unifiable"))] (*occurs check*)
                              else [(V(y),Node(str,term_ls))]
| (V(x),Node(str,term_ls)) -> if(search (vars (Node(str,term_ls))) x) then [(V("dummy"),V("not unifiable"))] (*occurs check*)
                              else [(V(x),Node(str,term_ls))]
| (Node(str1,term_ls1),Node(str2,term_ls2)) -> 
    if(str1 <> str2) then [(V("dummy"),V("not unifiable"))]
    else if((length term_ls1) <> (length term_ls2)) then [(V("dummy"),V("not unifiable"))]
    else ((mgu_ls term_ls1 term_ls2)) (*concat condenses a list of lists into a single list*)

  and mgu_ls term_ls1 term_ls2 =
    match (term_ls1,term_ls2) with
    ([],[]) -> []
  | ([x],[y]) -> (mgu x y)
  | ((x1::x2::xs),(y1::y2::ys)) -> let s = (mgu x1 y1) in (*Find mgu of first terms of both lists and then apply mgu to the next term*)
                                   composition_substitution s (mgu_ls ((subst x2 s)::xs) ((subst y2 s)::ys));;

let rec unifier (t:term) (cs:clause list) s =
  match cs with
    [] -> []
  | (t1,[])::xs -> let sub = mgu t1 t in
                   if (search sub (V("dummy"),V("not unifiable"))) then (unifier t xs s)
                   else sub::(unifier t xs s)
  | (t1,ts)::xs -> let sub = mgu t1 t in
                   if (search sub (V("dummy"),V("not unifiable"))) then (unifier t xs s)
                   else (map (composition_substitution sub) (unifier_helper1 (map (subst_rev sub) ts) s s))@(unifier t xs s)

  and unifier_helper1 ts cs s =
        match ts with
        [] -> [[]]
      | x::xs -> unifier_helper2 (unifier x s s) s xs

  and unifier_helper2 ls s xs =
        match ls with
        [] -> []
      | y::ys -> (map (composition_substitution y) (unifier_helper1 (map (subst_rev y) xs) s s))@(unifier_helper2 ys s xs);;

let rec print_term (t:term) =
  match t with
  V(str) -> Printf.printf "Variable %s\n" str
| Node(str,ts) -> Printf.printf "Symbol %s Children:\n" str; print_termls ts

and print_termls (ts:term list) =
  match ts with
  [] -> Printf.printf "over\n"
| x::xs -> print_term x; print_termls xs

and print_clause (c:clause) =
  let (t,ts) = c in
  print_term t; Printf.printf "followed by";print_termls ts;;


(* test cases for the given pl file *)

(*

father(jack,susan).

father(ray,jack).

parent(jack,X).

grandmother(X,Y).

parent(susan,jack).

father(X,Y) , mother(Y,Z).

plays(A,B,golf) , parent(A,C).

plays(A,B,C).

*)


(* official test cases 
path(A,B).

*)


    

