(* Instructions of the CAM *)

open Miniml;;

type instr =
  PrimInstr of primop
| Cons
| Push
| Swap
| Return
| Quote of value
| Cur of code
| App
| Branch of code * code
(* new for recursive calls *)
| Call of var
| AddDefs of (var * code) list
| RmDefs of int
and value =
  NullV 
| VarV of Miniml.var
| IntV of int
| BoolV of bool
| PairV of value * value
| ClosureV of code * value
and code = instr list
  
type stackelem = Val of value | Cod of code

let rec exec = function
   (PairV(x,y), PrimInstr(UnOp(Fst))::c, st) -> exec(x,c,st)

   | (PairV(x,y), PrimInstr(UnOp(Snd))::c, st) -> exec(y,c,st)

   | (x, Cons::c,(Val y)::d) -> exec(PairV(y,x), c, d)

   | (x, Push::c, d) -> exec(x, c, (Val x)::d)

   | (x, Swap::c,(Val y)::d) -> exec(y, c, (Val x)::d)

   | (t, (Quote v)::c, d) -> exec(v, c, d)

   | (PairV(ClosureV(x,y),z), (App::c) ,d )-> exec( PairV(y,z),x , (Cod c)::d)

   | ((BoolV b), Branch(c1, c2)::c, (Val x)::d) ->
                exec(x, (if b then c1 else c2) ,(Cod c)::d)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BArith(BAadd)))::c,d) ->
                exec(IntV (m + n), c , d)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BArith(BAsub)))::c,d) ->
                exec(IntV (m - n), c , d)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BArith(BAmul)))::c,d) ->
                exec(IntV (m * n), c , d)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BArith(BAdiv)))::c,d) ->
                exec(IntV (m / n), c , d)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BArith(BAmod)))::c,d) ->
                exec(IntV (m mod n), c , d)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BCompar(BCeq)))::c,d) ->
                exec(BoolV (m == n), c , d)

   | (x, (Cur c1)::c,d)  -> exec(ClosureV(c1 ,x), c , d)

   | (x, Return::c ,(Cod cc)::d) -> exec(x, cc , d)

   | cfg -> cfg;;


let rec access v env = match env with
	[] -> failwith "liste vide"
	|z::liste -> if z = v then [PrimInstr(UnOp(Snd))]
			else (PrimInstr(UnOp(Fst)))::(access v liste);;
	

let rec compile = function
	|(env,Bool(b)) -> [Quote(BoolV(b))]
	|(env,Int(i)) -> [Quote(IntV(i))]
	|(env,Var(v)) -> access v env
	|(env, Fn(v,e)) -> [Cur(compile(v::env, e)@[Return])]
	|(env, App(f,a)) -> [Push]@(compile(env,f))@[Swap]@(compile(env,a))@[Cons;App];;

let compile_prog = function
	Prog(t, exp) -> compile([], exp);;



