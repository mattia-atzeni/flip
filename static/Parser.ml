open Camlp4.PreCast ;;

module Gram = MakeGram(Lexer) ;;

let exp  = Gram.Entry.mk "exp";;
let letlist = Gram.Entry.mk "letlist";;
let formalParameters = Gram.Entry.mk "formalParameters";;
let actualParameters = Gram.Entry.mk "actualParameters";;
let lst  = Gram.Entry.mk "lst";;

EXTEND Gram
	
	exp:
	[ "Try"
		[ "try"; e1 = exp; "catch"; id = STRING; "->"; e2 = exp -> Try (e1, id, e2) ]
	| "Let"
		[ "let"; dec = letlist; "in"; block = exp -> Let (dec, block) 
		| "let"; "rec"; `LIDENT x; "="; e1 = exp; "in"; e2 = exp -> LetRec (x, e1, e2) ]
	| "Fun"
		[ "fun"; l = formalParameters; "->"; e = exp -> Fun (l, e) ]
	| "IfThenElse"
		[ "if"; cond = exp; "then"; e1 = exp; "else"; e2 = exp -> Ifthenelse (cond, e1, e2) ]
	| "Pair"
		[ e1 = exp; ","; e2 = exp -> Pair (e1, e2) ]
	| "Or"
		[ e1 = exp; "||"; e2 = exp ->  Or (e1, e2) ]
	| "And" LEFTA
		[ e1 = exp; "&&"; e2 = exp -> And (e1, e2) ]
	| "Not" RIGHTA
		[ "not"; e = exp -> Not e ]	 
	| "Relational" LEFTA
		[ e1 = exp; "="; e2 = exp -> Eq (e1, e2) 
		| e1 = exp; "<"; e2 = exp -> Less (e1, e2) ]
	| "Arithmetic1" LEFTA 
		[ e1 = exp; "+"; e2 = exp -> Sum (e1, e2) 
		| e1 = exp; "-"; e2 = exp -> Diff (e1, e2) ]
	| "Arithmetic2" LEFTA
		[ e1 = exp; "*"; e2 = exp -> Prod (e1, e2) 
		| e1 = exp; "/"; e2 = exp -> Div (e1, e2) 
		| e1 = exp; "mod"; e2 = exp -> Mod (e1, e2) ]	
	| "List"
		[ e1 = exp; "::"; e2 = exp -> Cons (e1, e2) 
		| "["; "]" -> Emptylist
		| "["; l = lst; "]" -> l ]
	| "Apply" RIGHTA
	 	[ e1 = exp; "("; e2 = LIST0 [x = exp -> x]; ")" -> Apply (e1, e2) ]
 	| "Dot"
		[ e = exp; "."; "fst" -> Fst e
		| e = exp; "."; "snd" -> Snd e 
		| e = exp; "."; "head" -> Head e
		| e = exp; "."; "tail" -> Tail e 
		| e = exp; "."; "iszero" -> Iszero e ]
	| "Parenthesis"
		[ "("; e = exp; ")" -> e ]
	| "Cast" RIGHTA
		[ "bigint"; e = exp -> Castint e ]
	| "Raise" RIGHTA
		[ "raise"; id = STRING -> Raise id ]
	| "Values"
		[ `INT(i,_) -> Eint i 
		| "true" -> Ebool true
		| "false" -> Ebool false]
	| "Den" 
		[ `LIDENT x -> Den x ]
	| "Opposite"
		[ "-"; `INT(i,_) -> Eint (-i) ]
	];


	letlist:
	[
	[ `LIDENT x; "="; e = exp -> [(x, e)]
	| l1 = letlist; "and"; l2 = letlist -> l1 @ l2 ]
	];
	  
	formalParameters:
	[
	[ `LIDENT x -> [x]
	| "("; ")" -> []
	| l1 = formalParameters; l2 = formalParameters -> l1 @ l2 ]
	];
	  
	actualParameters:
	[
	[ e = exp; l = actualParameters -> e :: l
	| e = exp -> [e]
	]
	];
		
	lst:
	[ 
	[ e1 = exp; ";"; e2 = lst -> Cons (e1, e2)
	| e = exp -> Cons (e, Emptylist) ]
	];

END ;;
