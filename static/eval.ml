#use "Bigint.ml";;

exception TypeMismatch of string;;

let rec string_of_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TBigint -> "bigint"
  | TPair (f, s) -> "(" ^ string_of_type f ^ " * " ^ string_of_type s ^ ")" 
  | TList l -> string_of_type l ^ " list"
  | TFun -> "fun"
  | TVar n -> "t" ^ string_of_int n
;;

let typeMismatchError found expected =
  let rec interleave separator = function
    | [] -> ""
    | [a] -> a
    | head :: tail -> head ^ separator ^ interleave separator tail
  in  
  let f = string_of_type found 
  and e = interleave " or " (List.map string_of_type expected) in
  let header = "An expression was found of type " ^ f in
  let message = (
    match expected with
      | [] -> 
          header ^ ", but an expression was expected of a different type"
      | _ ->
          header ^ ", but an expression was expected of type " ^ e 
  ) in raise (TypeMismatch message)
;;

let evalCastInt = function
  | Evint i -> Evbigint (Bigint.cast i)
  | Evbigint l -> Evbigint l
  | e -> typeMismatchError (typeof e) [TInt; TBigint]
;;

let evalOp fsmall fbig = function
  | Evint a, Evint b -> Evint (fsmall a b)
  | Evbigint a, Evbigint b -> Evbigint (fbig a b)
  | Evbigint big, Evint small -> Evbigint (fbig big (Bigint.cast small))
  | Evint small, Evbigint big -> Evbigint (fbig (Bigint.cast small) big)
  | Evint _, e | Evbigint _, e | e, _ -> typeMismatchError (typeof e) [TInt; TBigint] 
;;

let evalEq = function
  | Evint v1, Evint v2 -> Evbool (v1 = v2)
  | Evbool v1, Evbool v2 -> Evbool (v1 = v2)
  | Evlist v1, Evlist v2 -> Evbool (v1 = v2)
  | Evbigint a, Evbigint b -> Evbool (Bigint.equals a b)
  | Evbigint big, Evint small
  | Evint small, Evbigint big -> Evbool (Bigint.equals (Bigint.cast small) big)
  | Evint _, e | Evbigint _, e -> typeMismatchError (typeof e) [TInt; TBigint]
  | e1, e2 -> 
  	let t1 = string_of_type (typeof e1)
  	and t2 = string_of_type (typeof e2) in
  		raise (TypeMismatch ("You cannot compare values of type " ^ t1 ^ " and " ^ t2)); 
;;

let evalLess = function
  | Evint v1, Evint v2 -> Evbool (v1 < v2)
  | Evbigint a, Evbigint b -> Evbool (Bigint.lessThan a b)
  | Evbigint big, Evint small -> Evbool (Bigint.lessThan big (Bigint.cast small))
  | Evint small, Evbigint big -> Evbool (Bigint.lessThan (Bigint.cast small) big)
  | Evint _, e | Evbigint _, e | e, _ -> typeMismatchError (typeof e) [TInt; TBigint]
;;

let evalIsZero = function
  | Evint i -> Evbool (i = 0)
  | Evbigint l -> Evbool (Bigint.isZero l)
  | e -> typeMismatchError (typeof e) [TInt; TBigint]
;;

let evalBoolExp f = function
  | Evbool e1, Evbool e2 -> Evbool (f e1 e2)
  | Evbool _, e | e, _ -> typeMismatchError (typeof e) [TBool]  
;;

let evalNot = function
  | Evbool v -> Evbool (not v)
  | e -> typeMismatchError (typeof e) [TBool]
;;

let evalPairOp f = function
  | Evpair (first, second) -> f (first, second)
  | e -> typeMismatchError (typeof e) [TPair (TVar 0, TVar 1)]
;;

let evalCons = function
  | value, Evlist [] -> Evlist [value]
  | value, Evlist (head :: tail) ->
      if typeof value = typeof head then
        Evlist (value :: head :: tail)
      else typeMismatchError (typeof value) ([typeof head])
  | value, e -> typeMismatchError (typeof e) [TList (TVar 0)]
;;

let evalHead = function
  | Evlist [] -> failwith "You tried to access the head of an empty list"
  | Evlist (head :: tail) -> head
  | e -> typeMismatchError (typeof e) [TList (TVar 0)] 
;;

let evalTail = function
  | Evlist [] -> failwith "You tried to get the tail of an empty list"
  | Evlist (head :: tail) -> Evlist tail
  | e -> typeMismatchError (typeof e) [TList (TVar 0)] 
;;


let rec bindParameters = function
  | hd1 :: tl1, hd2 :: tl2, rho ->
      bindParameters (tl1, tl2, bind rho hd1 (dval_of_eval hd2))
  | [], [], rho -> rho
  | _ -> failwith "A different number of parameters was expected"
;;

#use "check.ml"