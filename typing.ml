(* Typechecking of source programs *)

open Lang
open Analyses

(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     globalvar: (vname * tp) list; 
     returntp: tp;
     funbind: fundecl list}


(* TODO: put your definitions here *)
let tp_prog (Prog (gvds, fdfs)) =
  Prog([],
       [Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)])
;;

exception ErrorOfType;;
exception FunctionUnknown;;
exception Error;;
(* Exercices 1,2 -> TP_EXPR *)
(* retrieve the type of vname in the environment*)
let retrieve_type x = function env ->
    let rec retrieve_type_aux x = function 
     	[] -> raise Error
     	|(a, b)::q -> if x=a then b else retrieve_type_aux x q
    in let localvar = env.localvar in retrieve_type_aux x localvar ;;	
(* val retrieve_type : vname -> environment -> tp = <fun> *)


(* check if a function is in the environment *)
let rec check_function name = function env ->
	let rec check_function_aux name = function
		[] -> raise FunctionUnknown
		|Fundecl(t, n, []) ::l-> if name=n then Fundecl(t,n,[]) else check_function_aux name l
		|Fundecl(t, n, Vardecl(a,b)::q)::l-> if name=n then Fundecl(t,n, Vardecl(a,b)::q) else check_function_aux name l
	in let func= env.funbind in check_function_aux name func;;
(*  val check_function : fname -> environment -> fundecl = <fun>*)

(* transform a value in a type *)
let verifConst = function
	BoolV _ -> BoolT
	|IntV  _ -> IntT
	|VoidV -> VoidT;;
(* val verifConst :: value -> tp = <fun> *)

(* retrieve a vname with a var, suppress the binding (Local or Global) and return the value ("x", "y", "n", ...) *)
let retrieve_var = function 
	Var(_, v) -> v;;
(* val retrieve_var : var -> vname = <fun> *)

(* retrieve the type of a expression and search any errors of type  *)
let rec retrieve_val env = function
	Const(_, v) -> verifConst v
	|VarE (_, v) -> retrieve_type(retrieve_var v) env
	|IfThenElse(a, vIf, vThen, vElse) -> let valIf = retrieve_val env vIf in let valThen = retrieve_val env vThen in let valElse = retrieve_val env vElse in 
											if valIf = BoolT && valThen = BoolT && valElse = BoolT then BoolT else
												if valIf = BoolT && valThen = IntT && valElse = IntT then IntT else raise ErrorOfType
	|BinOp(a, binop, exp1, exp2) -> match binop with
									BArith x -> let val1 = retrieve_val env exp1 in let val2 = retrieve_val env exp2 in 
										if val1=IntT && val2=IntT then IntT else raise ErrorOfType
									|BCompar x -> let val1 = retrieve_val env exp1 in let val2 = retrieve_val env exp2 in 
										if val1 = val2 then BoolT else raise ErrorOfType
									|BLogic x-> let val1 = retrieve_val env exp1 in let val2 = retrieve_val env exp2 in 
										if val1 = BoolT && val2 = BoolT then BoolT else raise ErrorOfType;;
(* val retrieve_val : environment -> 'a expr -> tp = <fun> *)


(* check the arguments of the CallE and the Vardecl of the Fundecl *)
let rec check_function_type = function
	Fundecl(t,_,[]), [] -> t
	|Fundecl(_,_,_), [] -> raise Error
	|Fundecl(_,_,[]), _ -> raise Error
	|(Fundecl(t,n,Vardecl(a,b)::q)), (Vardecl(c,d)::l) -> if a=c then check_function_type(Fundecl(t,n, q), l) else raise Error;;
(* val check_function_type : fundecl * vardecl list -> tp = <fun> *)



(* transform a constant in a Varcecl *)
let rec transform_del_vardecl env = function
	[] -> []
	|(Const(a,IntV b))::q -> Vardecl(IntT, string_of_int b)::transform_del_vardecl env q
	|(Const(a,BoolV b))::q -> Vardecl(BoolT, string_of_bool b)::transform_del_vardecl env q;;
(* val transform_del_vardecl : 'a -> 'b expr list -> vardecl list = <fun> *)


(* check the type of Const in a list *)
let rec check_function_type_list env = function
	[] -> []
	|Const(a,b)::q -> Const(verifConst(b), b)::check_function_type_list env q;;
(* val check_function_type_list : 'a -> 'b expr list -> tp expr list = <fun> *)


(* retrieve the type of expressions *)
let rec tp_expr env = function
	Const(0, v) -> Const(verifConst v, v)
	|VarE (0,v)-> VarE((retrieve_type(retrieve_var v) env), v)
	|BinOp (0, binop, exp1, exp2)-> BinOp((retrieve_val env (BinOp (0, binop, exp1, exp2))), binop, tp_expr env exp1, tp_expr env exp2)
	|IfThenElse(0, vIf, vThen, vElse) -> IfThenElse(retrieve_val env(IfThenElse(0,vIf, vThen, vElse)), tp_expr env vIf, tp_expr env vThen, tp_expr env vElse)
	|CallE(0, name, Const(a,b)::q) -> let func = check_function name env in let varD= transform_del_vardecl env (Const(a,b)::q) in 
										CallE((check_function_type(func, varD)), name, check_function_type_list env (Const(a,b)::q));;
(* val tp_expr : environment -> int expr -> tp expr = <fun> *)

(* Exercice 5 *)
(* Examples of expressions *)
let expr1 = BinOp (0, BArith BAadd , VarE (0, Var (Local , "k")), Const (0, IntV 1));; (* example of BAaad *)
let expr2 = BinOp (0, BLogic BLor , Const (0, BoolV true), Const(0, BoolV false));;	   (* example of BLor  *)
let expr3 = IfThenElse(0, Const(0, BoolV true), Const(0, IntV 1),Const(0, IntV 0));;
let expr4 = CallE (0, "f", [ Const (0, IntV 3); Const (0, IntV 4)]);;
let expr5 = BinOp (0, BCompar BCeq , VarE (0, Var (Local , "n")),
					BinOp (0, BArith BAadd , VarE (0, Var (Local , "k")), Const (0, IntV 1)));;
let expr6 = IfThenElse(0,(BinOp(0, BCompar BCeq, Const(0, IntV 3), Const(0, IntV 3))), Const(0, IntV 1), Const(0, IntV 0));;
let expr7 = IfThenElse(0,(BinOp(0, BCompar BCeq, Const(0, IntV 3), Const(0, IntV 3))), expr1, Const(0, IntV 0));;
(* examples of environments *)

let env1 = { localvar = [("n", IntT ); ("k", IntT );("t", IntT)]; globalvar = []; returntp = VoidT ; funbind = []};;
let env2 = { localvar = [("n", IntT ); ("k", IntT )]; globalvar = []; returntp = VoidT ; funbind = [Fundecl (IntT , "f", [ Vardecl (IntT , "n"); Vardecl (IntT , "k")])]};;


(* Part two *)
(* Exercice 6 *)
let rec tp_CallC env = function
	[] -> []
	|expr1::exprs -> (tp_expr env expr1)::(tp_CallC env exprs);;
  
(* tp_smtp, type an instruction with VoidT *)
let rec tp_stmt env = function
	 Skip -> Skip
	|Assign (tp, var, expr) -> Assign (VoidT, var, tp_expr env expr)
	|Seq (stmt1, stmt2) -> Seq ((tp_stmt env stmt1), (tp_stmt env stmt2))
	|Cond (expr, stmt1, stmt2) -> 	if tp_of_expr(tp_expr env expr) = BoolT then 
                                    	Cond ((tp_expr env expr), (tp_stmt env stmt1), (tp_stmt env stmt2))
                                	else raise Error
	|While (expr, stmt) -> 	if tp_of_expr(tp_expr env expr) = BoolT then
                            	While ((tp_expr env expr), (tp_stmt env stmt))
                        	else raise Error
	|CallC (fname, exprs) -> CallC (fname, (tp_CallC env exprs))
	|Return (expr) -> Return (tp_expr env expr) ;;
(* val tp_stmt : environment -> smtm -> tp stmt = <fun> *)


(* reverse  a (tp * vname) in (vname *tp) *)
let rec reverse = function
		[] -> []
		|a::q -> (name_of_vardecl(a), tp_of_vardecl(a))::reverse(q);;
(* val reverse : vardecl list -> (vname * tp) list = <fun>*)

(* recup global Variables of an Fundecl *)
let rec recupGlobalVar = function
	Fundecl (_, _, []) -> []
	|Fundecl(t, name, (Vardecl(a,b))::q) -> (b,a)::recupGlobalVar (Fundecl(t, name, q));;
(* val recupGlobalVar : fundecl -> (vname * tp) list = <fun> *) 


(* complete an environment with localVar, globarVar and return tp *)
let completeEnv = function
	env, fundecl, (a::q) -> let lvar = reverse (a::q)
								in let gvar = recupGlobalVar fundecl 
									in let t = check_function_type (fundecl, (a::q))
										in 	{localvar = lvar;
					 						globalvar = gvar;
					 						returntp = t;
					 						funbind = (env.funbind)@[fundecl]};;
(* val completeEnv : environment * fundecl * vardecl list -> environment = <fun> *)


(* tp_fdefn : check a definition of a function with an environment *)
let tp_fdefn env = function
	(fundecl, a::q, stmt) -> let newEnv = completeEnv (env, fundecl, a::q) in tp_stmt newEnv stmt;;
(* val tp_fdefn : environment -> fundecl * vardecl list * stmt -> tp stmt = <fun> *)


(* examples of stmt *)
let stmt = Skip;;
let stmt2 = Assign (0, Var (Local , "k"), expr1);;
let stmt3 = Seq (stmt, stmt2);;
let stmt4 = Cond(expr2, stmt, stmt2);;
let stmt5 = While(expr2, stmt2);;
let stmt6 = CallC ("fun1", [expr1;expr2]);;
let stmt7 = Return(expr1);;

(* examples of definitions of functions *)
let fund = Fundecl (IntT , "f", [ Vardecl (IntT , "n"); Vardecl (BoolT , "b")]), [ Vardecl (IntT , "k"); Vardecl (BoolT , "x")], stmt2;;
let fund2 = Fundecl (IntT , "g", [ Vardecl (IntT , "m"); Vardecl (BoolT , "p")]), [ Vardecl (IntT , "k"); Vardecl (BoolT , "x")], stmt6;;

let buildEnv = function
	Vardecl(t, name)::b -> let newEnv = {
							localvar = [];
	 						globalvar = [(name, t)];
	 						returntp = VoidT;
	 						funbind = []
							} in let rec aux env = function
									Vardecl(t,name)::c -> {localvar = [];
														globalvar = [(name, t)]@env.globalvar;
														returntp = VoidT;
														funbind = []} 
													in aux newEnv b;;
(* Lang.vardecl list -> Typing.environment = <fun> *)
(*let rec tp_prog = function
	Prog(vardecl::a, fundefn::b) -> let newEnv = buildEnv (vardecl::a) in 
												let rec aux env = function
													[] -> []
													|Fundefn(f,v,st)::q -> Fundefn(f,v, (tp_fdefn env (f,v,st)))::aux env q

												in Prog(vardecl::a, aux newEnv (fundefn::b));;
(* - : int Lang.prog -> Lang.tp Lang.prog = <fun> *)

*)
let fundefn = Fundefn(Fundecl (IntT , "f", [ Vardecl (IntT , "n"); Vardecl (BoolT , "b")]), [Vardecl (IntT , "k"); Vardecl (BoolT , "x")], stmt2);;
let vardecl =  [ Vardecl (IntT , "n"); Vardecl (BoolT , "b")];;

let prog  = Prog(vardecl, [fundefn]);;
