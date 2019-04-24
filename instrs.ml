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
and code = instr list;;
  
type stackelem = Val of value | Cod of code;;

type envelem = EVar of var | EDef of var list;;

let rec chop n fds =
    if n = 0 then fds
    else 
        (match fds with
            def::defs -> chop (n-1) defs
            | [] -> []);;
        
let rec exec = function
   (PairV(x,y), PrimInstr(UnOp(Fst))::c, st, fds) -> exec(x,c,st, fds)

   | (PairV(x,y), PrimInstr(UnOp(Snd))::c, st, fds) -> exec(y,c,st, fds)

   | (x, Cons::c,(Val y)::st, fds) -> exec(PairV(y,x), c, st, fds)

   | (x, Push::c, st, fds) -> exec(x, c, (Val x)::st, fds)

   | (x, Swap::c,(Val y)::st, fds) -> exec(y, c, (Val x)::st, fds)

   | (t, (Quote v)::c, st, fds) -> exec(v, c, st, fds)

   | (PairV(ClosureV(x,y),z), (App::c) ,st , fds)-> exec( PairV(y,z),x , (Cod c)::st, fds)

   | (x, (Cur c1)::c,st, fds)  -> exec(ClosureV(c1 ,x), c , st, fds)

   | (x, Return::c ,(Cod cc)::st, fds) -> exec(x, cc , st, fds)

   | (t, (Call(f))::c, st, fds) -> exec(t,(List.assoc f fds)@c, st, fds)

   | (t, (AddDefs(defs))::c, st, fds) -> exec(t,c,st,defs@fds)

   | (t, (RmDefs(n))::c, st, fds) -> (t,c,st,(chop n fds))

   | ((BoolV b), Branch(c1, c2)::c, (Val x)::st, fds) ->
                exec(x, (if b then c1 else c2) ,(Cod c)::st, fds)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BArith(BAadd)))::c,st, fds) ->
                exec(IntV (m + n), c , st, fds)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BArith(BAsub)))::c,st, fds) ->
                exec(IntV (m - n), c , st, fds)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BArith(BAmul)))::c,st, fds) ->
                exec(IntV (m * n), c , st, fds)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BArith(BAdiv)))::c,st, fds) ->
                exec(IntV (m / n), c , st, fds)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BArith(BAmod)))::c,st, fds) ->
                exec(IntV (m mod n), c , st, fds)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BCompar(BCeq)))::c,st, fds) ->
                exec(BoolV (m == n), c , st, fds)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BCompar(BCge)))::c,st, fds) ->
                exec(BoolV (m >= n), c , st, fds)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BCompar(BCgt)))::c,st, fds) ->
                exec(BoolV (m > n), c , st, fds)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BCompar(BCle)))::c,st, fds) ->
                exec(BoolV (m <= n), c , st, fds)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BCompar(BClt)))::c,st, fds) ->
                exec(BoolV (m < n), c , st, fds)

   | (PairV((IntV m), (IntV n)), PrimInstr(BinOp(BCompar(BCne)))::c,st, fds) ->
                exec(BoolV (m <> n), c , st, fds)

   | cfg -> cfg;;

let execute = function 
    config -> exec(NullV, config, [], []);;

(*Parcourt la liste des fonctions définies*)
let rec access_defs v env = match env with
    [] -> failwith "liste vide"
    |def::liste -> if def=v then [Call def]
                    else (access_defs v liste);;

(*
On place la liste des Fst/Snd dans la variable res. 
Si on trouve la EVar de valeur v, alors on retourn res avec le Snd à la fin.
Sinon, on parcourt la liste de fonctions définies
*)
let rec access_aux v env res= match env with
    [] -> failwith "liste vide"
    | (EVar e)::liste -> if e=v then res@[PrimInstr(UnOp(Snd))]
                        else (access_aux v liste ((PrimInstr(UnOp(Fst)))::res))
    | (EDef l)::liste -> (access_defs v l);;

let access v env = access_aux v env [];;
	

let rec compile = function
	|(env,Bool(b)) -> [Quote(BoolV(b))]
	|(env,Int(i)) -> [Quote(IntV(i))]
	|(env,Var(v)) -> access v env
	|(env, Fn(v,e)) -> [Cur(compile(v::env, e)@[Return])]
    |(env, App(PrimOp(p), e)) -> (compile(env,e))@[PrimInstr(p)]
	|(env, App(f,a)) -> [Push]@(compile(env,f))@[Swap]@(compile(env,a))@[Cons;App]
    |(env, Pair(e1,e2)) -> [Push]@(compile(env,e1))@[Swap]@(compile(env,e2))@[Cons]
    |(env, Cond(i,t,e)) -> [Push]@(compile(env,i))@[Branch((compile(env,t))@[Return], (compile(env,e))@[Return])];;


let compile_prog = function
	Prog(t, exp) -> compile([], exp);;

(*let rec print_gen_class_to_java_aux = function
	 
let print_gen_class_to_java = 1;;*)
(*
exec(NullV, (compile_prog (parse "Tests/test.ml"),[});;
*)


