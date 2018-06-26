type annotation = 
    | AInt
    | ABigint
    | ABool
    | AList of annotation
    | APair of annotation * annotation
    | AFun
    | AUnsp
    | AVar of int
;;

let rec annotation_of_type = function
  | TInt -> AInt
  | TBigint -> ABigint
  | TBool -> ABool
  | TList t -> AList (annotation_of_type t)
  | TPair (t1, t2) -> APair (annotation_of_type t1, annotation_of_type t2)
  | TFun -> AFun
  | TVar i -> AVar i
;;

let rec type_of_annotation = function
  | AInt -> TInt
  | ABigint -> TBigint
  | ABool -> TBool
  | AList (c) -> TList (type_of_annotation c)
  | APair (c1, c2) -> TPair (type_of_annotation c1, type_of_annotation c2)
  | AFun -> TFun
  | AUnsp -> TInt
  | AVar i -> TVar i
;;

let addToContext context l = 
  (List.map (fun x -> (x, AVar 0)) l) @ context
;;

let rec repl_annotations a1 a2 = 
  (a1 = a2) || (
    match a1, a2 with
      | AVar _, _ | _, AVar _ -> true
      | AUnsp, AInt | AUnsp, ABigint 
      | AInt, AUnsp | ABigint, AUnsp -> true
      | AList a1, AList a2 -> repl_annotations a1 a2
      | APair (a1, a2), APair (a1', a2') -> repl_annotations a1 a1' && repl_annotations a2 a2'
      | _, _ -> false
  ) 
;;

let rec checkDen id rho context = match context with
  | [] -> annotation_of_type (typeof (eval_of_dval (applyenv rho id)))
  | (x, t)  :: tail ->
      if x = id then t
      else checkDen id rho tail
;; 

let checkCast t = 
  if repl_annotations t AInt || repl_annotations t ABigint then ABigint
  else typeMismatchError (type_of_annotation t) [TInt; TBigint]
;;

let checkOp t1 t2 = match t1, t2 with
  | AInt, AInt -> AInt
  | ABigint, t | t, ABigint ->
      if repl_annotations ABigint t || repl_annotations AInt t then ABigint
      else typeMismatchError (type_of_annotation t) [TInt; TBigint]
  | t1, t2 ->
      if repl_annotations t1 AInt then (
        if repl_annotations t2 AInt then AUnsp
        else typeMismatchError (type_of_annotation t2) [TInt; TBigint]
      ) else typeMismatchError (type_of_annotation t1) [TInt; TBigint]
;;

let checkEq t1 t2 = match t1, t2 with
  | AFun, t | t, AFun -> raise (TypeMismatch ("You cannot compare functions"))
  | APair (a, b), t
  | t, APair (a, b) -> raise (TypeMismatch ("You cannot compare expressions of type pair"))
  | AInt, ABigint | ABigint, AInt -> ABool
  | t1, t2 -> 
      if repl_annotations t1 t2 then ABool
      else typeMismatchError (type_of_annotation t2) [type_of_annotation t1]
;;

let checkLess t1 t2 = 
  if repl_annotations t1 AInt || repl_annotations ABigint t1 then (
    if repl_annotations t2 AInt || repl_annotations ABigint t2 then	ABool
    else typeMismatchError (type_of_annotation t2) [TInt; TBigint]
  ) else typeMismatchError (type_of_annotation t1) [TInt; TBigint]
;;

let checkBoolExp t1 t2 = 
  if repl_annotations t1 ABool then (
    if repl_annotations t2 ABool then	ABool
    else typeMismatchError (type_of_annotation t2) [TBool]
  ) else typeMismatchError (type_of_annotation t1) [TBool]
;;

let checkNot t =
  if repl_annotations t ABool then ABool
  else typeMismatchError (type_of_annotation t) [TBool]
;;

let checkIszero t = 
  if repl_annotations t AInt || repl_annotations t ABigint then ABool
  else typeMismatchError (type_of_annotation t) [TInt; TBigint] 
;;

let checkPairOp f = function
  | APair (c1, c2) -> f (c1, c2)
  | AVar i -> AVar i
  | t -> typeMismatchError (type_of_annotation t) [TPair (TVar 0, TVar 1)]
;;

let checkHead = function
  | AList t -> t
  | AVar i -> AVar i
  | t -> typeMismatchError (type_of_annotation t) [TList (TVar 0)]
;;

let checkTail = function
  | AList t -> AList t
  | AVar i -> AVar i
  | t -> typeMismatchError (type_of_annotation t) [TList (TVar 0)]
;;

let choose t1 t2 = 
  if not (repl_annotations t1 t2) then
    typeMismatchError (type_of_annotation t2) [type_of_annotation t1]
  else 
    match t1, t2 with
      | AVar _, t | t, AVar _ -> t
      | AUnsp, t | t, AUnsp -> t
      | t1, t2 -> t1
;;

let checkCons t1 t2 = match t1, t2 with
  	| t1, AList t2 -> AList (choose t1 t2)
  	| t1, AVar _ -> AList t1
  	| _, t -> typeMismatchError (type_of_annotation t) [TList (TVar 0)]
;;

let checkIf e0 e1 e2 = match e0 with
    ABool ->  choose e1 e2
  | t -> typeMismatchError (type_of_annotation t) [TBool]
;;

let checkApply f = 
  if repl_annotations f AFun then AVar 0
  else raise (TypeMismatch "You tried to apply a non-functional object") 
;;

let checkDen id rho context = match context with
  | [] -> annotation_of_type (typeof (eval_of_dval (applyenv rho id)))
  | (x, t)  :: tail ->
      if x = id then t
      else checkDen id rho tail
;; 

let addToContext context l = 
  (List.map (fun x -> (x, AVar 0)) l) @ context
;;
