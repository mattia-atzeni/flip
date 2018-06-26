(* ocamlc -c -I +camlp4 -pp camlp4of.opt main.ml *)
(* camlp4 main.cmo *)

#use "Interpreter_dynamic.ml";;
#use "AbstractSyntaxPrinter.ml";;
#use "Parser.ml";;

let parse_prog s =
   Gram.parse_string exp (Loc.mk "<string>") s;;

let rec fold_lines acc = 
  try 
    let s = read_line () in fold_lines (acc ^ s ^ "\n")
  with End_of_file -> acc
;;

let _ = 
    let sprog = fold_lines ""
    in 
    print_string ("\n \n"^ string_of_exp (parse_prog sprog) ^"\n \n")
;;

