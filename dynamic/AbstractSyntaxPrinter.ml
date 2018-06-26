let rec string_of_exp = function
  | Eint e -> "Eint " ^ string_of_int e
  | Ebool e -> "Ebool " ^ string_of_bool e
  | Bigint e -> "Bigint [" ^ String.concat "; " (List.map string_of_int e) ^ "]"
  | Den x -> "Den " ^ "\"" ^ x ^ "\""
  | Castint e -> "Castint (" ^ string_of_exp e ^ ")"
	| Sum (e1, e2) -> "Sum (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^")"
	| Diff (e1, e2) -> "Diff (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^")"
	| Prod (e1, e2) -> "Prod (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^")"
	| Div (e1, e2) -> "Div (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^")"
	| Mod (e1, e2) -> "Mod (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^")"
	| Eq (e1, e2) -> "Eq (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^")"
	| Less (e1, e2) -> "Less (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^")"
	| And (e1, e2) -> "And (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^")"
	| Or (e1, e2) -> "Or (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^")"
	| Not e -> "Not (" ^ string_of_exp e ^ ")"
	| Iszero e -> "Iszero " ^ string_of_exp e 
	| Ifthenelse (cond, e1, e2) -> "Ifthenelse (" ^ string_of_exp cond ^ ", " ^string_of_exp e1 ^ ", " ^string_of_exp e2 ^ ")"
	| Let (dec, block) -> "Let (" ^ string_of_letlist dec ^ ", " ^ string_of_exp block ^ ")"
	| Fun (l, e) -> "Fun (" ^ string_of_formalParameters l ^ ", " ^ string_of_exp e ^ ")"
	| Apply (f, l) -> "Apply (" ^ string_of_exp f ^ ", " ^ string_of_actualParameters l ^ ")"
	| Emptylist -> "Emptylist"
	| Cons (e1, e2) -> "Cons (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^")"
	| Head e -> "Head " ^ string_of_exp e
	| Tail e -> "Tail " ^ string_of_exp e
	| Pair (e1, e2) -> "Pair (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^")"
	| Fst e -> "Fst " ^ string_of_exp e
	| Snd e -> "Snd " ^ string_of_exp e

and string_of_letlist l =   
  let rec aux = function
	    [] -> ""
    | [x, e] -> "\"" ^ x ^ "\"" ^ ", " ^ string_of_exp e
	  | (x, e) :: tail -> "\"" ^ x ^ "\"" ^ ", " ^ string_of_exp e ^ "; " ^ aux tail
  in "[" ^ aux l ^ "]"

and string_of_formalParameters l = 
  let rec aux = function
  | [] -> ""
  | [x] -> "\"" ^ x ^ "\""
  | head :: tail -> "\"" ^ head ^ "\"" ^ "; " ^ aux tail
  in "[" ^ aux l ^ "]"

and string_of_actualParameters l = 
  let rec aux = function
  | [] -> ""
  | [e] -> string_of_exp e
  | head :: tail -> string_of_exp head ^ "; " ^ aux tail
  in "[" ^ aux l ^ "]"
;;

