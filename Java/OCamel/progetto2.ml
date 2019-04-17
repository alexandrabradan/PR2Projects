type ide = string;;
type exp = Eint of int | Ebool of bool | Den of ide | Prod of exp * exp | Sum of exp * exp | Diff of exp * exp |
	Eq of exp * exp | Minus of exp | IsZero of exp | Or of exp*exp | And of exp*exp | Not of exp |
	Ifthenelse of exp * exp * exp | Let of ide * exp * exp | Fun of ide * exp | FunCall of exp * exp |
	Letrec of ide * exp * exp| ETree of tree| ApplyOver of exp * exp | Update of (ide list) * exp * exp |
    Select of (ide list) * exp * exp
and tree = Empty | Node of ide * exp * tree * tree;;

(*ambiente polimorfo*)
type 't env = ide -> 't;;
let emptyenv (v : 't) = function x -> v;;
let applyenv (r : 't env) (i : ide) = r i;;
let bind (r : 't env) (i : ide) (v : 't) = function x -> if x = i then v else applyenv r x;;

(*tipi esprimibili*)
type evT = Int of int | Bool of bool | Unbound | FunVal of evFun | RecFunVal of ide * evFun | Tree of evTree 
and evFun = ide * exp * evT env
and evTree = EmptyVal | NodeVal of ide * evT * evT * evT;;

(*rts*)
(*type checking*)
let typecheck (s : string) (v : evT) : bool = match s with
	"int" -> (match v with
		Int(_) -> true 
		|_ -> false
	) 
	|"bool" -> (match v with
		Bool(_) -> true 
		|_ -> false
	) 
	|_ -> failwith("not a valid type");;

(*funzioni primitive*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n*u))
	else failwith("Type error");;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n+u))
	else failwith("Type error");;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n-u))
	else failwith("Type error");;

let eq x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Bool(n=u))
	else failwith("Type error");;

let minus x = if (typecheck "int" x) 
	then (match x with
	   	Int(n) -> Int(-n))
	else failwith("Type error");;

let iszero x = if (typecheck "int" x)
	then (match x with
		Int(n) -> Bool(n=0))
	else failwith("Type error");;

let vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		(Bool(b),Bool(e)) -> (Bool(b||e)))
	else failwith("Type error");;

let et x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		(Bool(b),Bool(e)) -> Bool(b&&e))
	else failwith("Type error");;

let non x = if (typecheck "bool" x)
	then (match x with
		Bool(true) -> Bool(false) 
	   |Bool(false) -> Bool(true)
	)
	else failwith("Type error");;

(*funzione cha valuta se dato un albero ed un cammino, quest'ultimo e' un cammino dell'albero*)
let rec validPath (path : ide list) (t : exp) : bool = 
	match path with 
		[] -> true (*il cammino vuoto e' un cammino valido*)
		|[h] -> (*se il cammino ha un unico identificatore*)
			(match t with
				ETree(Empty) -> false (*non e' un cammino valido per l'albero vuoto*)
				(*e' un cammino valido se il tag del nodo corrente e' uguale all'unico identificatore del cammino*)
				|ETree(Node(tag, data, sx, dx)) -> 
					if (compare tag h = 0)
						then true
					else false
				|_ -> failwith ("no tree expression")
			)
		|h::g::rest -> (*se il cammino ha due o piu identificatori*)
			(match t with
			| ETree(Empty) -> false (*non e' un cammino valido per l'albero vuoto*)
			(*e' un cammino valido se il tag del nodo corrente e' uguale al primo identificatore del cammino ed il
			  nodo successivo del sottoalbero sx o dx ha il tag uguale al secondo identificatore del cammino*)
			| ETree(Node(tag, data, sx, dx)) -> 
				if (compare tag h = 0) 
					then ( 
						   match (sx, dx) with
							(Empty, Empty) -> false
							|(Empty, Node(tagdx, datadx, sxdx, dxdx)) ->
								if (compare tagdx g = 0) 
                					then validPath (g::rest) (ETree(Node(tagdx, datadx, sxdx, dxdx))) 
								else false
							|(Node(tagsx, datasx, sxsx, dxsx), Empty) ->
								if (compare tagsx g = 0) 
               						then  validPath (g::rest) (ETree(Node(tagsx, datasx, sxsx, dxsx))) 
								else false
							| (Node(tagsx, datasx, sxsx, dxsx), Node(tagdx, datadx, sxdx, dxdx)) ->
								if (compare tagsx g = 0) 
               						 then validPath (g::rest) (ETree(Node(tagsx, datasx, sxsx, dxsx))) 
								else if (compare tagdx g = 0) 
                						then validPath (g::rest) (ETree(Node(tagdx, datadx, sxdx, dxdx))) 
							 		 else false
	
						)
			 	else false	
			|_ -> failwith ("no path expression")
			);;	
	 

(*interprete*)
let rec eval (e : exp) (r : evT env) : evT = match e with
	Eint n -> Int n 
	|Ebool b -> Bool b 
	|IsZero a -> iszero (eval a r) 
	|Den i -> applyenv r i 
	|Eq(a, b) -> eq (eval a r) (eval b r) 
	|Prod(a, b) -> prod (eval a r) (eval b r) 
	|Sum(a, b) -> sum (eval a r) (eval b r) 
	|Diff(a, b) -> diff (eval a r) (eval b r) 
	|Minus a -> minus (eval a r) 
	|And(a, b) -> et (eval a r) (eval b r) 
	|Or(a, b) -> vel (eval a r) (eval b r) 
	|Not a -> non (eval a r) 
	|Ifthenelse(a, b, c) -> 
		let g = (eval a r) in
			if (typecheck "bool" g) 
				then (if g = Bool(true) then (eval b r) else (eval c r))
				else failwith ("nonboolean guard") 
	|Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r)) 
	|Fun(i, a) -> FunVal(i, a, r) 
	|FunCall(f, eArg) -> 
		let fClosure = (eval f r) in
			(match fClosure with
				FunVal(arg, fBody, fDecEnv) -> 
					eval fBody (bind fDecEnv arg (eval eArg r)) 
				|RecFunVal(g, (arg, fBody, fDecEnv)) -> 
					let aVal = (eval eArg r) in
						let rEnv = (bind fDecEnv g fClosure) in
							let aEnv = (bind rEnv arg aVal) in
								eval fBody aEnv 
				|_ -> failwith("non functional value")
			) 
    |Letrec(f, funDef, letBody) ->
        		(match funDef with
            		Fun(i, fBody) -> let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) in
                         			 eval letBody r1 
            		|_ -> failwith("non functional def")
            	)
    |ETree (t) -> 
    	(match t with 
					Empty -> Tree(EmptyVal) (*albero vuoto valutato*)
					|Node (tag, data, sx, dx) -> 
						let v = (eval data r) in (*valuto il valore del nodo corrente*)
						let sxVal = (eval (ETree(sx)) r) in (*valuto sottoalbero sx*)
						let dxVal = (eval (ETree(dx)) r) in (*valuto sottoalbero dx*)
						Tree(NodeVal(tag, v, sxVal, dxVal)) (*albero valutato*)
		) 
    |ApplyOver(f, t)->  
    	(match t with 
		    ETree(Empty) -> Tree(EmptyVal) (*non applico f all'albero vuoto, ma mi limito a valutarlo*)
			|ETree(Node (tag, data, sx, dx)) -> 
					let v = (eval (FunCall (f, data)) r) in (*applico f al valore del nodo corrente*)
					let sxVal = (eval (ApplyOver (f, ETree(sx))) r) in (*applico f al sottoalbero sx*)
					let dxVal = (eval (ApplyOver (f, ETree(dx))) r) in (*appilo f al sottoalbero dx*)
					Tree(NodeVal(tag, v, sxVal, dxVal)) (*albero a cui ho applicato f*)
			|_ -> failwith ("no tree expression")
		)
    |Update (path, f, t) -> 
    	if (validPath path t = true) (*se ho un cammino valido*)
        	then (match path with
					[] -> (eval t r) (*se il cammino e' vuoto, valuto l'albero*)
					|[h] -> (*se il cammino ha un unico identificatore*)
				 		(match t with
							ETree(Empty) -> Tree(EmptyVal) (*se l'albero e' vuoto restituisco l'albero vuoto valutato*)
							|ETree(Node (tag, data, sx, dx)) -> 
								(*se il tag del nodo corrente e' uguale all'unico identificatore del cammino,
								   applico la f al nodo corrente e valuto sottoalbero sx e dx*)
								if (compare tag h = 0)
									then let v = (eval (FunCall(f, data)) r) in 
									Tree(NodeVal(tag, v, (eval (ETree(sx)) r), (eval (ETree(dx)) r))) 
								(*se il tag del nodo corrente e' diverso all'unico identificatore del cammino,
								  mi limito a valutare l'albero*)
								else (eval t r)
							|_ -> failwith("non tree expression")
				        )
					|h::g::rest -> 
						(match t with
							ETree(Empty) -> Tree(EmptyVal) (*se l'albero e' vuoto, lo valuto*)
							|ETree(Node (tag, data, sx, dx)) -> 
								(*se il tag del nodo corrente e' uguale al primo identificatore del cammino*)
								if (compare tag h = 0) 
									then
									(*verifico se ho un cammino valido, verificando se il secondo tag del cammino
									  e' uguale al nodo successivo del sottoalbero sx o dx. Se esiste tale nodo, applico la f 
									  al nodo corrente e reiitero procedimento su tale nodo, altrimenti valuto l'albero*)
										( match (sx, dx) with
									 			(Empty, Empty) -> (eval t r)
									 			|(Empty, Node(tagdx, datadx, sxdx, dxdx)) -> 
									 				if (compare tagdx g = 0) 
									 					then let v = (eval (FunCall(f, data)) r) in 
									 					Tree(NodeVal(tag, v, Tree(EmptyVal), (eval (Update(g::rest, f,  ETree(Node(tagdx, datadx, sxdx, dxdx)))) r)))
									 				else Tree(EmptyVal) 
									 			|(Node(tagsx, datasx, sxsx, dxsx), Empty) -> 
									 				if (compare tagsx g = 0)
									 					then let v = (eval (FunCall(f, data)) r) in 
									 					Tree(NodeVal(tag, v, (eval (Update(g::rest, f,  ETree(Node(tagsx, datasx, sxsx, dxsx)))) r), Tree(EmptyVal)))
									 				else Tree(EmptyVal) 
									 			|(Node(tagsx, datasx, sxsx, dxsx), Node(tagdx, datadx, sxdx, dxdx)) ->
									 				if (compare tagsx g = 0)
									 					then let v = (eval (FunCall(f, data)) r) in 
									 					Tree(NodeVal(tag, v, (eval (Update(g::rest, f,  ETree(Node(tagsx, datasx, sxsx, dxsx)))) r), (eval (ETree(dx)) r)))
									 				else if (compare tagdx g = 0)
									 					then let v = (eval (FunCall(f, data)) r) in 
									 					Tree(NodeVal(tag, v, (eval (ETree(sx)) r), (eval (Update(g::rest, f,  ETree(Node(tagdx, datadx, sxdx, dxdx)))) r)))
									 				else (eval t r)
    									)
    							(*se il tag del nodo corrente e' diverso dal primo identificatore del cammino, valuto l'albero*)
					   		    else (eval t r)
							|_ -> failwith ("no tree expression")
						)
    			)
		else failwith("Wrong path") (*cammino non valido*)
    |Select (path, f, t) -> 
    	if (validPath path t = true) (*se il cammino e' valido*)
    		then (match path with
					[] -> Tree(EmptyVal) (*se il cammino e' vuoto, restituisco sottoalbero vuoto*)
					|[h] -> (*se cammino ha un unico identificatore*)
				 		(match t with
							ETree(Empty) -> Tree(EmptyVal) (*se l'albero e' vuoto, restituisco sottoalbero vuoto*)
							|ETree(Node (tag, data, sx, dx)) -> 
								(*se il tag del nodo corrente e' uguale all'unico identificatore del cammino e se applicando la f al valore del 
								  nodo corrente, il risultato e' true*)
								if ((compare tag h = 0) &&  ((eval (FunCall(f, data)) r) = Bool(true))) 
									then let v = (eval data r) in (*valuto il valore del nodo corrente*)
								 		 let sxVal = (eval (ETree(sx)) r) in (*valuto sottoalbero sx*)
								 		 let dxVal = (eval (ETree(dx)) r) in (*valuto sottoalbero dx*)
									 	 Tree(NodeVal(tag, v, sxVal, dxVal)) (*restituisco sottoalbero radicato nel nodo corrente, valutato*)
								(*se il tag del nodo corrente e' diverso dall'unico identificatore del cammino  o se applicando la f al valore del
								  nodo corrente, il risultato e' false*)
								else Tree(EmptyVal) (*restituisco sottoalbero vuoto*)
							|_ -> failwith("non tree expression")
						)
					|h::g::rest -> 
						(match t with
							ETree(Empty) -> Tree(EmptyVal) (*se l'albero e' vuoto, restituisco sottoalbero vuoto*)
							|ETree(Node (tag, data, sx, dx)) -> 
								(*se il tag del nodo corrente e' uguale al primo identificatore del cammino*)
								if (compare tag h = 0) 
									(*se applicando la f al valore del nodo corrente, il risultato e' true*)
									then ( if ((eval (FunCall(f, data)) r) = Bool(true)) 
											then let v = (eval data r) in (*valuto il valore del nodo corrente*)
									    		 let sxVal = (eval (ETree(sx)) r) in (*valuto sottoalbero sx*)
									     		 let dxVal = (eval (ETree(dx)) r) in (*valuto sottoalbero dx*)
										 		 Tree(NodeVal(tag, v, sxVal, dxVal)) (*restituisco sottoalbero radicato nel nodo corrente, valutato*)
								  			 else
									 		(*se applicando la f al valore del nodo corrente, il risultato e' false, verifico se esiste un nodo del 
									 		  sottoalbero sx o dx tale per cui esso appartiene al cammino ed applicando la f al valore di tale nodo,  
									 		  il risultato e' true. Se tale nodo esiste, restituisco il sottoalbero valutato e radicato in tale nodo
									 		  altrimenti restituisco il sottoalbero vuoto*)
									 		( match (sx, dx) with
									 			(Empty, Empty) -> Tree(EmptyVal)
									 			|(Empty, Node(tagdx, datadx, sxdx, dxdx)) -> 
									 				if (compare tagdx g = 0) 
									 					then eval (Select(g::rest, f,  ETree(Node(tagdx, datadx, sxdx, dxdx)))) r 
									 				else Tree(EmptyVal) 
									 			|(Node(tagsx, datasx, sxsx, dxsx), Empty) -> 
									 				if (compare tagsx g = 0)
									 					then eval (Select(g::rest, f,  ETree(Node(tagsx, datasx, sxsx, dxsx)))) r 
									 				else Tree(EmptyVal) 
									 			|(Node(tagsx, datasx, sxsx, dxsx), Node(tagdx, datadx, sxdx, dxdx)) ->
									 				if (compare tagsx g = 0)
									 					then eval (Select(g::rest, f,  ETree(Node(tagsx, datasx, sxsx, dxsx)))) r 
									 				else if (compare tagdx g = 0)
									 						then eval (Select(g::rest, f,  ETree(Node(tagdx, datadx, sxdx, dxdx)))) r 
									 				else Tree(EmptyVal)
									 		)
							     		)
								(*se il tag del nodo corrente e' diverso dal primo identificatore del cammino, restituisco sottoalbero vuoto*)
					   			 else Tree(EmptyVal)
							|_ -> failwith ("no tree expression")
						)
    			)
else failwith("Wrong path") (*cammino non valido*);; 
						
  					
(* =============================  TESTS  ================= *)

(* basico: no let *)
let env0 = emptyenv Unbound;;




let e1 = ApplyOver(Fun("x", Sum(Den "x", Eint 1)), ETree(Empty));;

eval e1 env0;;

let e1 = Update([], Fun("x", Sum(Den "x", Eint 1)), ETree(Empty));;

eval e1 env0;;

let e1 = Update(["a"], Fun("x", Sum(Den "x", Eint 1)), ETree(Empty));;

eval e1 env0;;

let e1 = Update(["a"; "b"], Fun("x", Sum(Den "x", Eint 1)), ETree(Empty));;

eval e1 env0;;

let e1 = Select([], Fun("x", Eq(Den "x", Eint 1)), ETree(Empty));;

eval e1 env0;;

let e1 = Select(["a"], Fun("x", Eq(Den "x", Eint 1)), ETree(Empty));;

eval e1 env0;;

let e1 = Select(["a"; "b"], Fun("x", Eq(Den "x", Eint 1)), ETree(Empty));;

eval e1 env0;;



let e2 = ApplyOver(Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Empty, Empty)));;

eval e2 env0;;

let e2 = Update([], Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Empty, Empty)));;

eval e2 env0;;

let e2 = Update(["a"], Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Empty, Empty)));;

eval e2 env0;;

let e2 = Update(["b"], Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Empty, Empty)));;

eval e2 env0;;

let e2 = Update(["a"; "b"], Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Empty, Empty)));; 
eval e2 env0;;

let e2 = Select([], Fun("x", Eq(Den "x", Eint 1)), ETree(Node("a", Eint 1, Empty, Empty)));;

eval e2 env0;;

let e2 = Select(["a"], Fun("x", Eq(Den "x", Eint 1)), ETree(Node("a", Eint 1, Empty, Empty)));;

eval e2 env0;;

let e2 = Select(["a"], Fun("x", Eq(Den "x", Eint 100)), ETree(Node("a", Eint 1, Empty, Empty)));;

eval e2 env0;;

let e2 = Select(["b"], Fun("x", Eq(Den "x", Eint 1)), ETree(Node("a", Eint 1, Empty, Empty)));;

eval e2 env0;;

let e2 = Select(["a", "b"], Fun("x", Eq(Den "x", Eint 1)), ETree(Node("a", Eint 1, Empty, Empty)));;

eval e2 env0;;



let e3 = ApplyOver(Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Empty))));;

eval e3 env0;;

let e3 = Update([], Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Empty))));;

eval e3 env0;;

let e3 = Update(["a"], Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Empty))));;

eval e3 env0;;

let e3 = Update(["b"], Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Empty))));;

eval e3 env0;;

let e3 = Update(["a"; "b"], Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Empty))));;

eval e3 env0;;

let e3 = Update(["a"; "c"], Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Empty))));;

eval e3 env0;;

let e3 = Update(["a"; "d"], Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Empty))));;

eval e3 env0;; 

let e3 = Select([], Fun("x", Eq(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Empty))));;

eval e3 env0;;

let e3 = Select(["a"], Fun("x", Eq(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Empty))));;

eval e3 env0;;

let e3 = Select(["a"], Fun("x", Eq(Den "x", Eint 100)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Empty))));;

eval e3 env0;;

let e3 = Select(["b"], Fun("x", Eq(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Empty))));;

eval e3 env0;; 

let e3 = Select(["a"; "b"], Fun("x", Eq(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Empty))));;

eval e3 env0;;

let e3 = Select(["a";"b"], Fun("x", Eq(Den "x", Eint 2)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Empty))));;

eval e3 env0;;

let e3 = Select(["a"; "b"], Fun("x", Eq(Den "x", Eint 100)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Empty))));;

eval e3 env0;;

let e3 = Select(["a"; "d"], Fun("x", Eq(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Empty))));;

eval e3 env0;; 




let e4 = ApplyOver(Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Node("d", Eint 4, Node("e", Eint 5, Empty, Empty), Empty)))));;

eval e4 env0;;

let e4 = Update([], Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Node("d", Eint 4, Node("e", Eint 5, Empty, Empty), Empty)))));;

eval e4 env0;;

let e4 = Update(["a"], Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Node("d", Eint 4, Node("e", Eint 5, Empty, Empty), Empty)))));;

eval e4 env0;;

let e4 = Update(["b"], Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Node("d", Eint 4, Node("e", Eint 5, Empty, Empty), Empty)))));;

eval e4 env0;; 

let e4 = Update(["a"; "c"; "d"; "e"], Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Node("d", Eint 4, Node("e", Eint 5, Empty, Empty), Empty)))));;

eval e4 env0;;

let e4 = Update(["a"; "d"], Fun("x", Sum(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Node("d", Eint 4, Node("e", Eint 5, Empty, Empty), Empty)))));;

eval e4 env0;; 

let e4 = Select([], Fun("x", Eq(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Node("d", Eint 4, Node("e", Eint 5, Empty, Empty), Empty)))));;

eval e4 env0;;

let e4 = Select(["a"], Fun("x", Eq(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Node("d", Eint 4, Node("e", Eint 5, Empty, Empty), Empty)))));;

eval e4 env0;;

let e4 = Select(["a"], Fun("x", Eq(Den "x", Eint 100)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Node("d", Eint 4, Node("e", Eint 5, Empty, Empty), Empty)))));;

eval e4 env0;;

let e4 = Select(["b"], Fun("x", Eq(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Node("d", Eint 4, Node("e", Eint 5, Empty, Empty), Empty)))));;

eval e4 env0;; 

let e4 = Select(["a"; "c"], Fun("x", Eq(Den "x", Eint 3)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Node("d", Eint 4, Node("e", Eint 5, Empty, Empty), Empty)))));;

eval e4 env0;;

let e4 = Select(["a"; "c"; "d"], Fun("x", Eq(Den "x", Eint 4)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Node("d", Eint 4, Node("e", Eint 5, Empty, Empty), Empty)))));;

eval e4 env0;;

let e4 = Select(["a"; "c"; "d"; "e"], Fun("x", Eq(Den "x", Eint 5)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Node("d", Eint 4, Node("e", Eint 5, Empty, Empty), Empty)))));;

eval e4 env0;;

let e4 = Select(["a"; "d"], Fun("x", Eq(Den "x", Eint 1)), ETree(Node("a", Eint 1, Node("b", Eint 2, Empty, Empty), Node("c", Eint 3, Empty, Node("d", Eint 4, Node("e", Eint 5, Empty, Empty), Empty)))));;

eval e4 env0;; 
