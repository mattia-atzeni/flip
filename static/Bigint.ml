module type Bigint_type = 
sig
  val block : int
  val cast : int -> int list
  val sum : int list -> int list -> int list
  val karatsuba : int list -> int list -> int list
  val subtract : int list -> int list -> int list
  val divide : int list -> int list -> int list
  val modulo : int list -> int list -> int list
  val equals : int list -> int list -> bool
  val lessThan : int list -> int list -> bool
  val isZero : int list -> bool
  val opposite : int list -> int list
  exception DivisionByZero
end;; 

module Bigint : Bigint_type = 
struct

  let rec order n = 
    if n < 100 then 1
    else 10 * order (n / 100)

  let block = order max_int

  let rec explode n = 
    if n / block = 0 then [n]
    else explode (n / block) @ [n mod block]

  let rec trim = function
    | [] -> []
    | [a] -> [a]
    | head :: tail as l -> 
        if head = 0 then trim tail
        else l

  let rec shift p n = 
    let rec zeros = function
      | 0 -> []
      | n -> 0 :: zeros (n - 1)
    in p @ zeros n

  let rec pad n l =
    if List.length l < n then
      0 :: pad (n - 1) l
    else l

  let rec split n = function
    | [] -> [], []
    | head :: tail as l ->
        if n > 0 then let (l1, l2) = split (n - 1) tail in head :: l1, l2 
        else [], l

  let normalize l = match trim l with
    | [] -> [0]
    | head :: tail as l -> 
        let sign = if head > 0 then 1 else (-1) in
        let (res, carryover) = 
          List.fold_right (
            fun a (acc, carryover) ->
              let a = (a + carryover) in
              let digit = a mod block and carryover = a / block in
                if sign * digit >= 0 then digit :: acc, carryover
                else (digit + sign * block) :: acc, carryover - sign
          ) l ([], 0)
        in trim (carryover :: res)

  let sum_anorm l1 l2 = 
    let n = max (List.length l1) (List.length l2) in
      (List.map2 (+) (pad n l1) (pad n l2))

  let opposite l = List.map (fun a -> (-a)) (normalize l)

  let opposite_anorm l = List.map (fun a -> (-a)) (l)

  let sub_anorm l1 l2 = sum_anorm l1 (opposite_anorm l2)

  let rec karatsuba_anorm a b = match a, b with
    | [], _ | _, [] | [0], _ | _, [0] -> [0]
    | [a], l | l, [a] -> List.map (( * ) a) l
    | x, y ->
        let lenx = List.length x and leny = List.length y in
        let n = max lenx leny in 
        let m = n / 2 in
        let x1, x2 = split (lenx - m) x 
        and y1, y2 = split (leny - m) y in
        let z1 = karatsuba_anorm x1 y1 in
        let z3 = karatsuba_anorm x2 y2 in
        let z2 = sub_anorm (karatsuba_anorm (sum_anorm x1 x2) (sum_anorm y1 y2)) (sum_anorm z1 z3) in
          sum_anorm (shift z1 (2 * m)) (sum_anorm (shift z2 m) z3)

  type sign = Pos | Neg | Zero

  let rec getSign l = match trim l with
    | [] | [0] -> Zero
    | head :: tail ->
        if head > 0 then Pos
        else Neg

  let isZero l = match getSign l with
    | Zero -> true
    | _ -> false

  let isPositive l = match getSign l with
    | Pos -> true
    | _ -> false

  let isNegative l = match getSign l with
    | Neg -> true
    | _ -> false

  let lessThan a b = 
    let a = normalize a and b = normalize b in
    let rec lt sign l1 l2 = 
      let len1 = List.length l1 and len2 = List.length l2 in
        if len1 > len2 then (sign = Neg)
        else if len1 < len2 then (sign = Pos)
        else 
          match l1, l2 with
              [], [] -> false
            | hd1 :: tl1, hd2 :: tl2 -> 
                if hd1 < hd2 then true
                else if hd1 = hd2 then lt sign tl1 tl2
                else false
            | _ -> failwith "[lessThan]"
    in match getSign a, getSign b with
        Neg, Neg -> lt Neg a b 
      | Pos, Pos -> lt Pos a b 
      | _, Pos -> true
      | _, Neg -> false
      | Neg, Zero -> true
      | _, Zero -> false

  let equals a b = (normalize a) = (normalize b)

  exception DivisionByZero

  let sum a b = normalize (sum_anorm a b)
  let subtract a b = normalize (sub_anorm a b)
  let karatsuba a b = normalize (karatsuba_anorm a b)

  let rec division n d = 
    let rec aux q r = 
      match getSign n, getSign d with
          _, Zero -> raise DivisionByZero
        | Pos, Neg -> let (q, r) = division n (opposite d) in opposite q, r
        | Neg, Pos -> let (q, r) = division (opposite n) d in opposite q, opposite r
        | Zero, _ -> [0], [0]
        | Neg, Neg -> let (q, r) = division (opposite n) (opposite d) in q, opposite r
        | Pos, Pos -> 
            let difference = subtract r d in
              if not (isNegative difference) then
                aux (sum q [1]) difference
              else (q, r)
    in aux [0] n

  let cast = explode
  let divide n d = normalize (fst (division n d))
  let modulo n d = normalize (snd (division n d))

end;;

(******* TEST *************
         let rec repeat n f x = 
         if n = 0 then x
         else repeat (n - 1) f (f x)
         ;;

         Random.self_init ();;

         repeat 1000000 (
         fun acc ->
         let a_sign = if (Random.bool ()) then (-1) else (1)
         and b_sign = if (Random.bool ()) then (-1) else (1)
         in
         let a = a_sign * Random.int 100000000 and b = b_sign * (Random.int (100000000)) in
         try 
         if (Bigint.cast (a * b)) <> (Bigint.karatsuba (Bigint.cast a) (Bigint.cast b)) then 
         "Failed: a = " ^ string_of_int a ^ " | b = " ^ string_of_int b
         else acc
         with Bigint.DivisionByZero -> "DivisionByZero: " ^ string_of_int b
         ) "OK!"
         ;;
 *****************************)