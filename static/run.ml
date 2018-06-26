(* ocamlc -c -I +camlp4 -pp camlp4of.opt main.ml *)
(* camlp4 main.cmo *)

#use "Interpreter.ml";;
#use "AbstractSyntaxPrinter.ml";;
#use "Parser.ml";;

let parse_prog s =
   Gram.parse_string exp (Loc.mk "<string>") s;;

let rec fold_lines acc = 
  try 
    let s = read_line () in fold_lines (acc ^ s ^ "\n")
  with End_of_file -> acc
;;

let rec explode n = 
  if n / 10 = 0 then [n]
  else explode (n / 10) @ [n mod 10]
;;

let rec pad n l =
  if List.length l < n then
    0 :: pad (n - 1) l
  else l
;;

let max_size = List.length (explode Bigint.block) - 1;;

let rec string_of_eval = function
  | Evint i -> string_of_int i
  | Evbool b -> string_of_bool b 
  | Evbigint l -> ( match l with
    | [] -> "0"
    | hd :: tl -> string_of_int hd ^ 
      String.concat "" (
        List.flatten (
          List.map (List.map string_of_int) (
            List.map (pad max_size) (
              List.map explode (List.map abs tl)
            )
          )
        )
      )
  )
  | Evlist l -> "[" ^ String.concat "; " (List.map string_of_eval l) ^ "]"
  | Evfun (l, rho, e) -> "\nparams: " ^ String.concat " " l ^ "\n\n" ^ string_of_exp e
  | Evpair (f, s) -> "(" ^ string_of_eval f ^ ", " ^ string_of_eval s ^ ")"
  | Evnone -> failwith "None"
;;

let _ = 
  let sprog = fold_lines "" in
  let result = sem_static (parse_prog sprog) in
	  print_string (string_of_type (typeof result) ^ " : " ^ string_of_eval result ^ "\n\n")  
;;

