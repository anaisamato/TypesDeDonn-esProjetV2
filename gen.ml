(* Compilation functions *)

open Lang;;
open Analyses;;
open Instrs;;
open Typing;;
(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)



(* ************************************************************ *)
(* **** Compilation of methods / programs                  **** *)
(* ************************************************************ *)

(* find the position of x in a list *)
let rec position x = function
  	[] -> failwith"Not in the list"
  	|a::q -> if a=x then 0 else 1+position x q;;
(*  val position : 'a -> 'a list -> int = <fun> *)


(* take a Var and return it value *)
let varAux = function
  Var(_, a) -> a;;
(* val varAux  var -> vname = <fun> *)
 
let recupCompar = function
  BinOp(_, BCompar c, _, _) -> c;;

let rec listTps = function
  [] -> []
  |Const(a,b)::q -> a::listTps q;; 
(* generate bytecode *)
let rec gen_exp compt liste = function
   Const(t, v)-> [Label ([compt]) ;Loadc (t ,v)]
  |VarE (t,v)  -> [Label ([compt]); Loadv (t, position(varAux v) liste)]
  |BinOp (t, binop, exp1, exp2)-> (gen_exp compt liste exp1)@(gen_exp (compt+1) liste exp2)@([Label([compt+2]);Bininst(t, binop)])
   |IfThenElse(t, exprIf, exprThen, exprElse) ->  let compar = recupCompar exprIf in match exprIf with
                                                                                      BinOp(_,_, a,b) -> let genA = gen_exp (compt+1) liste a
                                                                                                in let genB = gen_exp (compt+2) liste b
                                                                                              in
                                             genA@genB@([Label([compt+3]);If(compar, [compt+4])])@
                                                  (gen_exp (compt+4) liste exprThen)@
                                                 (gen_exp (compt+5) liste exprElse)
  |CallE(t, name, tps) -> [Label([compt]);Invoke(t, name, listTps tps)] ;;
(* val gen_exp : vname list -> tp expr -> instr list = <fun> *)

(* Gen_expr Automitisation *)
let couple = function
  (a,_) -> a;;
(* val couple 'a * 'b -> 'a = <fun> *)

(* count the variables of a environment *)
let nbVarEnv = function
  env -> let rec aux = function 
        [] -> []
        |a::q -> IntT::aux q in aux env.localvar;;
(* val nbVarEnv : environment -> tp list = <fun> *)

(* create a list with the variables of an environment *)
let listOfVar = function
  env -> let rec aux = function 
        [] -> []
        |a::q -> couple(a)::aux q in aux env.localvar;;
(* val listOfVar : environment -> vname list = <fun> *)

(* Test gen_exp and return the result, generate Even.J with the expression in bytecode *)
let gen_prog (Prog (gvds, fdfs)) = 
  JVMProg ([], 
           [Methdefn (Methdecl (IntT, "even", (nbVarEnv env2)), 
                      Methinfo (10, 10), (* limit stack et limit local *)
                      ((gen_exp 0 (listOfVar env2) (tp_expr env2 expr7))@[ReturnI (tp_of_expr(tp_expr env2 expr7))]) (* test an expression of typing.ml with an environment *)
                      (* here, test the expr1 in typing.ml with the env1 *)
          )]);;