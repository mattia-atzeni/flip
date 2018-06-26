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
    | LetRec of ide * exp * exp   
    | Fun of ide list * exp
    | Apply of exp * exp list
    | Try of exp * ide * exp
    | Raise of ide
;;

type eval = 
      Evint of int
    | Evbool of bool
    | Evbigint of int list
    | Evpair of eval * eval
    | Evlist of eval list
    | Evfun of ide list * env * exp
    | Evnone

and  dval = 
      Dint of int
    | Dbool of bool
    | Dbigint of int list
    | Dpair of eval * eval
    | Dlist of eval list
    | Dfun of ide list * env * exp
    | DfunRec of ide list * env * exp
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

type state = Raised of ide | Ok;;

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
  | Dfun (l, rho, e) -> Evfun (l, rho, e)
  | dvalue -> raise (NotExpressible (dvalue))
;;

let dval_of_eval = function
  | Evint i -> Dint i
  | Evbool b -> Dbool b
  | Evbigint l -> Dbigint l
  | Evpair (f, s) -> Dpair (f, s)
  | Evlist l -> Dlist l
  | Evfun (l, rho, e)  -> Dfun (l, rho, e)
  | Evnone -> failwith "Evnone"
;;

let rec typeof = function
  | Evint _ -> TInt
  | Evbigint _ -> TBigint
  | Evbool _ -> TBool
  | Evpair (f, s) -> TPair (typeof f, typeof s)
  | Evlist [] -> TList (TVar 0) 
  | Evlist (head :: tail) -> TList (typeof head)
  | Evfun _ -> TFun
  | Evnone -> failwith "Evnone"
;;

#use "eval.ml";;

let rec checkExp e rho context = match e with
  | Eint i -> AInt
  | Ebool b -> ABool
  | Bigint l -> ABigint
  | Den id -> (try checkDen id rho context with NotExpressible _ -> AVar 0)
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
      let _ = checkExp e rho (addToContext context formalParameters) in AFun
  | Apply (f, actualParameters) -> checkApply (checkExp f rho context)
  | Let (dec, block) -> 
      let (idelist, explist) = List.split dec in
      let alist = checkList explist rho context in
        checkExp block rho ((List.combine idelist alist) @ context)
  | LetRec (id, e1, e2) -> AVar 0
  | Try (e0, id, e1) -> choose (checkExp e0 rho context) (checkExp e1 rho context)
  | Raise e -> AVar 0

and checkList l rho context = match l with
  | head :: tail -> (checkExp head rho context) :: (checkList tail rho context)
  | [] -> []
;;

let check e rho = checkExp e rho [];;

let makePair (a, b) = Evpair (a, b);;

let wrap f = function
  | _, Raised e -> Evnone, Raised e
  | evalue, Ok -> f evalue, Ok 
;;

let wrapPair f = function
  | (_, Raised e), _ -> Evnone, Raised e
  | (a, Ok), (b, Raised e) -> Evnone, Raised e
  | (a,Ok), (b, Ok) -> f (a, b), Ok
;; 

let rec evalExp e rho = match e with
  | Eint i -> Evint i, Ok
  | Ebool b -> Evbool b, Ok
  | Bigint l -> Evbigint l, Ok
  | Den id -> ( 
      match (applyenv rho id) with
        | DfunRec (l, rho, e) -> Evfun (l, bind rho id (DfunRec (l, rho, e)), e)
        | dvalue -> eval_of_dval dvalue
    ), Ok
  | Castint e -> wrap evalCastInt (evalExp e rho)
  | Sum (e1, e2) -> wrapPair (evalOp (+) Bigint.sum) (evalExp e1 rho, evalExp e2 rho)
  | Diff (e1, e2) -> wrapPair (evalOp (-) Bigint.subtract) (evalExp e1 rho, evalExp e2 rho)
  | Prod (e1, e2) -> wrapPair (evalOp ( * ) Bigint.karatsuba) (evalExp e1 rho, evalExp e2 rho)
  | Div (e1, e2) -> wrapPair (evalOp (/) Bigint.divide) (evalExp e1 rho, evalExp e2 rho)
  | Mod (e1, e2) -> wrapPair (evalOp (mod) Bigint.modulo) (evalExp e1 rho, evalExp e2 rho)
  | Less (e1, e2) -> wrapPair evalLess (evalExp e1 rho, evalExp e2 rho)
  | Eq (e1, e2) -> wrapPair evalEq (evalExp e1 rho, evalExp e2 rho)
  | And (e1, e2) -> wrapPair (evalBoolExp (&&)) (evalExp e1 rho, evalExp e2 rho)
  | Or (e1, e2) -> wrapPair (evalBoolExp (||)) (evalExp e1 rho, evalExp e2 rho)
  | Not e -> wrap evalNot (evalExp e rho)
  | Iszero e -> wrap evalIsZero (evalExp e rho)
  | Pair (e1, e2) -> wrapPair makePair (evalExp e1 rho, evalExp e2 rho)
  | Fst e -> wrap (evalPairOp fst) (evalExp e rho)
  | Snd e -> wrap (evalPairOp snd) (evalExp e rho)
  | Emptylist -> Evlist [], Ok
  | Cons (e1, e2) -> wrapPair evalCons (evalExp e1 rho, evalExp e2 rho)
  | Head e -> wrap evalHead (evalExp e rho)
  | Tail e -> wrap evalTail (evalExp e rho)
  | Ifthenelse (e0, e1, e2) -> (
      match evalExp e0 rho with
        | Evbool b, Ok -> (*
            match evalExp e1 rho, evalExp e2 rho with
              | (e1, Ok), (e2, Ok) ->
                if typeof e1 <> typeof e2 then
                  typeMismatchError (typeof e2) [typeof e1]
                else 
                  if b then e1, Ok
                  else e2, Ok
              | e1, e2 -> if b then e1 else e2 
          *)
          let _ = choose (check e1 rho) (check e2 rho) in 
          if b then evalExp e1 rho else evalExp e2 rho
        | _, Raised e -> Evnone, Raised e
        | e, _ -> typeMismatchError (typeof e) [TBool]
    )
  | Fun (fp, e) -> let _ = check (Fun (fp, e)) rho in Evfun (fp, rho, e), Ok
  | Apply (f, actualParameters) -> evalApply (evalExp f rho) (evalList actualParameters rho)
  | Let (dec, block) -> 
      let (idlist, explist) = List.split dec in
      let elist = evalList explist rho in (
        match scanList elist with
          | Ok -> 
              let rho' = bindParameters (idlist, List.map fst elist, rho) in
                evalExp block rho'
          | Raised e -> Evnone, Raised e
      )
  | LetRec (id, e1, e2) -> ( 
      match e1 with
          Fun (formalParameters, e) -> 
            evalExp e2 (bind rho id (DfunRec (formalParameters, rho, e)))
        | Raise e -> Evnone, Raised e
        | _ -> failwith "Recursion not allowed for this expression" 
    )
  | Try (e1, ide, e2) -> evalTry (e1, ide, e2) rho
  | Raise e -> Evnone, Raised e

and evalList l rho = match l with
  | [] -> []
  | head :: tail -> (evalExp head rho) :: (evalList tail rho) 

and evalApply f al = match f with
  | Evfun (fl, rho, e), Ok -> ( 
      match scanList al with
        | Ok -> 
            let rho' = bindParameters (fl, List.map fst al, rho) in
              evalExp e rho'
        | Raised e -> Evnone, Raised e
    )
  | _, Raised e -> Evnone, Raised e
  | e, _ -> typeMismatchError (typeof e) [TFun]

and scanList = function
  | (evalue, Ok) :: tail -> scanList tail
  | (_, Raised e) :: tail -> Raised e
  | [] -> Ok

and evalTry (e1, ide, e2) rho = 
  let _ = choose (check e1 rho) (check e2 rho) in
  match evalExp e1 rho with
    | evalue, Ok -> evalue, Ok
    | _, Raised i ->
        if i = ide then evalExp e2 rho
        else Evnone, Raised i
;;

let sem_static e = 
  match evalExp e (emptyenv ()) with
    | evalue, Ok -> evalue
    | _, Raised j -> failwith ("Uncaught exception: " ^ j)
;;
