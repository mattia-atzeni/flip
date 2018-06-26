type ide = string;;

type exp =
    | Eint of int 
    | Ebool of bool 
    | Bigint of int list
    | Castint of exp
    | Emptylist
    | Cons of exp * exp
    | Head of exp
    | Tail of exp
    | Den of ide
    | Prod of exp * exp
    | Sum of exp * exp
    | Diff of exp * exp
    | Mod of exp * exp
    | Div of exp * exp
    | Less of exp * exp
    | Eq of exp * exp
    | Iszero of exp
    | Or of exp * exp
    | And of exp * exp
    | Not of exp
    | Pair of exp * exp
    | Fst of exp
    | Snd of exp
    | Ifthenelse of exp * exp * exp
    | Let of (ide * exp) list * exp    
    | Fun of ide list * exp
    | Apply of exp * exp list
;;

type eval = 
      Evint of int
    | Evbool of bool
    | Evbigint of int list
    | Evpair of eval * eval
    | Evlist of eval list
    | Evfun of ide list * exp
    | Evclosure of ide list * env * exp

and  dval = 
      Dint of int
    | Dbool of bool
    | Dbigint of int list
    | Dpair of eval * eval
    | Dlist of eval list
    | Dfun of ide list * exp
    | Dclosure of ide list * env * exp
    | Unbound

and env = Env of (ide -> dval);;

type typ = 
    | TInt 
    | TBool 
    | TBigint
    | TList of typ 
    | TPair of typ * typ
    | TFun
    | TVar of int
;; 

exception UnboundIde of ide;;

let emptyenv () = Env (fun x -> Unbound);;

let bind (Env rho) id dvalue = 
  Env (
    fun x -> 
      if x = id then dvalue 
      else rho x
  )
;;

let applyenv (Env rho) x = match rho x with
    Unbound -> raise (UnboundIde x)
  | dvalue -> dvalue
;;

exception NotExpressible of dval;;

let eval_of_dval = function
  | Dint i -> Evint i
  | Dbool b -> Evbool b
  | Dbigint l -> Evbigint l
  | Dpair (f, s) -> Evpair (f, s)
  | Dlist l -> Evlist l
  | Dfun (l, e) -> Evfun (l, e)
  | Dclosure (l, r, e) -> Evclosure (l, r, e)
  | dvalue -> raise (NotExpressible (dvalue))
;;

let dval_of_eval = function
  | Evint i -> Dint i
  | Evbool b -> Dbool b
  | Evbigint l -> Dbigint l
  | Evpair (f, s) -> Dpair (f, s)
  | Evlist l -> Dlist l
  | Evfun (l, e)  -> Dfun (l, e)
  | Evclosure (l, r, e) -> Dclosure (l, r, e)
;;

let rec typeof = function
  | Evint _ -> TInt
  | Evbigint _ -> TBigint
  | Evbool _ -> TBool
  | Evpair (f, s) -> TPair (typeof f, typeof s)
  | Evlist [] -> TList (TVar 0) 
  | Evlist (head :: tail) -> TList (typeof head)
  | Evfun _ | Evclosure _ -> TFun
;;

#use "eval.ml";;

let rec checkExp e rho context = match e with
  | Eint i -> AInt
  | Ebool b -> ABool
  | Bigint l -> ABigint
  | Den id -> (try checkDen id rho context with UnboundIde _ -> AVar 0)
  | Castint e -> checkCast (checkExp e rho context)
  | Sum (e1, e2)  
  | Diff (e1, e2)
  | Prod (e1, e2)
  | Div (e1, e2)
  | Mod (e1, e2) -> checkOp (checkExp e1 rho context) (checkExp e2 rho context)
  | Less (e1, e2) -> checkLess (checkExp e1 rho context) (checkExp e2 rho context)
  | Eq (e1, e2) -> checkEq (checkExp e1 rho context) (checkExp e2 rho context)
  | And (e1, e2)
  | Or (e1, e2) -> checkBoolExp (checkExp e1 rho context) (checkExp e2 rho context)
  | Not e -> checkNot (checkExp e rho context)
  | Iszero e -> checkIszero (checkExp e rho context)
  | Pair (e1, e2) -> APair (checkExp e1 rho context, checkExp e2 rho context)
  | Fst e -> checkPairOp fst (checkExp e rho context)
  | Snd e -> checkPairOp snd (checkExp e rho context)
  | Emptylist -> AList (AVar 0)
  | Cons (e1, e2) -> checkCons (checkExp e1 rho context) (checkExp e2 rho context)
  | Head e -> checkHead (checkExp e rho context)
  | Tail e -> checkTail (checkExp e rho context)
  | Ifthenelse (e0, e1, e2) -> checkIf (checkExp e0 rho context) (checkExp e1 rho context) (checkExp e2 rho context)
  | Fun (formalParameters, e) -> 
      let _ = checkExp e (emptyenv ()) (addToContext context formalParameters) in AFun
  | Apply (f, actualParameters) -> checkApply (checkExp f rho context)
  | Let (dec, block) -> 
      let (idelist, explist) = List.split dec in
      let alist = checkList explist rho context in
        checkExp block rho ((List.combine idelist alist) @ context)

and checkList l rho context = match l with
  | head :: tail -> (checkExp head rho context) :: (checkList tail rho context)
  | [] -> []
;;

let check e rho = checkExp e rho [];;

let rec evalExp e rho = match e with
  | Eint i -> Evint i, rho
  | Ebool b -> Evbool b, rho
  | Bigint l -> Evbigint l, rho
  | Den id -> eval_of_dval (applyenv rho id), rho
  | Castint e -> evalCastInt (eval e rho), rho
  | Sum (e1, e2) -> evalOp (+) Bigint.sum (eval e1 rho, eval e2 rho), rho
  | Diff (e1, e2) -> evalOp (-) Bigint.subtract (eval e1 rho, eval e2 rho), rho
  | Prod (e1, e2) -> evalOp ( * ) Bigint.karatsuba (eval e1 rho, eval e2 rho), rho
  | Div (e1, e2) -> evalOp (/) Bigint.divide (eval e1 rho, eval e2 rho), rho
  | Mod (e1, e2) -> evalOp (mod) Bigint.modulo (eval e1 rho, eval e2 rho), rho
  | Less (e1, e2) -> evalLess (eval e1 rho, eval e2 rho), rho
  | Eq (e1, e2) -> evalEq (eval e1 rho, eval e2 rho), rho
  | And (e1, e2) -> evalBoolExp (&&) (eval e1 rho, eval e2 rho), rho
  | Or (e1, e2) -> evalBoolExp (||) (eval e1 rho, eval e2 rho), rho
  | Not e -> evalNot (eval e rho), rho
  | Iszero e -> evalIsZero (eval e rho), rho
  | Pair (e1, e2) -> Evpair (eval e1 rho, eval e2 rho), rho
  | Fst e -> evalPairOp fst (eval e rho), rho
  | Snd e -> evalPairOp snd (eval e rho), rho
  | Emptylist -> Evlist [], rho
  | Cons (e1, e2) -> evalCons (eval e1 rho, eval e2 rho), rho
  | Head e -> evalHead (eval e rho), rho
  | Tail e -> evalTail (eval e rho), rho
  | Ifthenelse (e0, e1, e2) -> (
  	match eval e0 rho with
		  | Evbool b ->
		  	let t1 = check e1 rho and t2 = check e2 rho in 
		      if not (repl_annotations t1 t2) then
		      	typeMismatchError (type_of_annotation t2) [type_of_annotation t1]
		      else 
		      	if b then eval e1 rho
		      	else eval e2 rho
		  | e -> typeMismatchError (typeof e) [TBool]
		), rho
  | Fun (fp, e) -> let _ = check (Fun (fp, e)) rho in Evfun (fp, e), rho
  | Apply (f, actualParameters) -> evalApply (eval f rho) (evalList actualParameters rho) rho, rho
  | Let (dec, block) -> 
      let rec bindList dec rho = match dec with
          [] -> rho
        | (id, exp) :: tail -> bindList tail (bind rho id (dval_of_eval (eval exp rho)))
      in let rho' = bindList dec rho
      in evalExp block rho'

and evalList l rho = match l with
  | [] -> []
  | head :: tail -> 
      let evalue = match eval head rho with
        | Evfun (l, e) ->  print_string ("closed\n"); Evclosure (l, rho, e)
        | evalue-> evalue
      in evalue :: (evalList tail rho) 

and evalApply f al rho = 
  	let (e, rho') = ( 
			match f with
      	| Evfun (fl, e) -> e, bindParameters (fl, al, rho)
      	| Evclosure (fl, r, e) -> e, bindParameters (fl, al, r)
      	| e -> typeMismatchError (typeof e) [TFun] 
    ) in match evalExp e rho' with
    | Evfun (l, e), rho -> print_string ("closed\n"); Evclosure (l, rho, e)
    | evalue, _ -> evalue  

and eval e rho = fst (evalExp e rho)

let sem_dynamic e = eval e (emptyenv ());;