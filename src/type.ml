open Ast
open Tast
open Format

(* First part. *)

let type1_func env f =
	() (* TODO *)

let rec type1_expr env e = match snd e with
	(* Add the global variables to the environement. *)
	| Call (_, (e1 :: b)) | Par (e1 :: b) ->
		type1_block (type1_expr env e1) b
	| Lval (_, Field (e1, _)) | Not e1 | Return (Some e1) | While (e1, _) ->
		type1_expr env e1
	| Affect ((_, Field (e1, _)), e2) | Binop (e1, _, e2) | For (_, e1, e2, _) ->
		type1_expr (type1_expr env e1) e2
	| Affect ((_, Var (_, var)), e1) ->
		let var_type =
			try Env.type_of var env
			with Not_found -> Typ.Any
		in
		Env.add_var var var_type (type1_expr env e1)
	| If (e1, b, eb) -> type1_else_block (type1_block env (e1 :: b)) eb
	| _ -> env

and type1_else_block env eb = match snd eb with
	(* Add the global variables to the environement. *)
	| End					-> env
	| Else b				-> type1_block env b
	| Elseif (e, b, eb1)	-> type1_else_block (type1_block env (e :: b)) eb1

and type1_block env =
	(* Add the global variables to the environement. *)
	List.fold_left type1_expr env

let rec type1 env = function
	(* Check the structure definitions.
	Check the function definitions (but not the bodies).
	Keep track of all the function and structure definition.
	Add the global variables to the environement. *)
	| Expr e :: l	-> type1 (type1_expr env e) l
	| Func f :: l	-> type1_func env f; type1 env l
	| []			-> env

(* Second part. *)

let type2 env = function
	(* Construct the local environement of each function/while loop/for loop.
	Verify that all return instructions on a function have a type compatible with the return type.
	Verify that all function/while loop/for loop bodies are well typed.
	Verify that global expressions are well typed.
	Verify that variables might be defined before being used. *)
	| _ -> [] (* TODO *)

(* Part 1 & part 2. *)

let typing ast =
	let genv = type1 Env.bindings ast in
	(*
	TODO
	let tast = type2 genv ast in
	*)
	let tast = [] in
	genv, tast

