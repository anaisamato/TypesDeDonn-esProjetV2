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

let recupCompt = function
  l::q::(Label([compt])::_) -> compt+1
  |l::(Label([compt])::_) -> compt
  |(Label([compt])::_) -> compt;;
  
(* generate bytecode *)
let rec gen_exp compt liste = function
   Const(t, v)-> [Label ([compt]) ;Loadc (t ,v)]
  |VarE (t,v)  -> [Label ([compt]); Loadv (t, position(varAux v) liste)]
  |BinOp (t, binop, exp1, exp2)-> let gen1 = (gen_exp compt liste exp1) 
                                    in let compt1 = (recupCompt gen1)+1
                                      in let gen2 = (gen_exp (compt1) liste exp2)
                                        in let compt2 = (recupCompt gen2)+1
                                          in gen1@gen2@[Label([compt2]);Bininst(t, binop)]
  |CallE(t, name, tps) -> [Label([compt]);Invoke(t, name, (listTps tps))]
  |IfThenElse(t, exprIf, exprThen, exprElse) ->  let compar = recupCompar exprIf in match exprIf with
                                                                                      BinOp(_,_, a,b) -> let gen1 = gen_exp (compt) liste a
                                                                        in let compt1 = (recupCompt gen1)+1
                                                                          in let gen2 = gen_exp compt1 liste b
                                                                            in let compt2 = (recupCompt gen2)+1
                                                                              in let genThen =  (gen_exp (compt2+1) liste exprThen)
                                                                                in let compt3 = (recupCompt genThen)+1
                                                                                  in let genElse =  gen_exp compt3 liste exprElse
                                                                                    in let compt4 = recupCompt genElse
                                            in gen1@gen2@([Label([compt2]);If(compar, [compt4])])@genThen@genElse;;
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
                      (* here, test the expr4 in typing.ml with the env2 *)
          )]);;