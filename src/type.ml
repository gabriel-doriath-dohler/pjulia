open Ast
open Tast
open Format

(* First part. *)

let type1_func f =
	(* Check the structure definitions.
	Check the function definitions (but not the bodies). *)

	let name = snd f.f_name in
	let func_or_struc = if f.f_is_constructor then "structure" else "function" in
	let arg_or_field = if f.f_is_constructor then "field" else "argument" in

	(* Verify that the name isn't div, print or println. *)
	if List.mem name [ "div"; "print"; "println" ] then
		Typ.type_error
			(fst f.f_name)
			(sprintf "A %s can't be named %s." func_or_struc name);

	(* Verify that the name of the arguments are distinct from each other.
	Verify that the types of the arguments are well defined. *)
	let arg_set:((string, unit) Hashtbl.t) = Hashtbl.create 16 in
	let verify_param p =
		if Hashtbl.mem arg_set (snd p.p_name) then
			Typ.type_error (fst p.p_name) (sprintf
				"The %s %s has two %ss called %s."
				func_or_struc name arg_or_field (snd p.p_name));
		Hashtbl.add arg_set (snd p.p_name) ();
		if not (Env.is_type_defined p.p_type ||
			(f.f_is_constructor && p.p_type = Typ.Struct name)) then
			Typ.type_error (fst p.p_name) (asprintf
				"The type of the %s %s (%a) isn't defined yet."
				arg_or_field (snd p.p_name) Typ.print p.p_type)
	in List.iter verify_param f.f_params;

	(* Structure. *)
	if f.f_is_constructor then begin
		(* Verify that no structure or function as the same name. *)
		if Env.is_function_defined name then
			Typ.type_error
				f.f_loc
				(sprintf
					"The structure %s has multiple definitions or a function has the same name."
					name);
		(* Declare the type defined by the structure. *)
		Env.declare_type (Typ.Struct name);
		(* Verify that the fields are not already used. *)
		let verify_field p =
			if Env.is_field_defined (snd p.p_name) then
				Typ.type_error (fst p.p_name) (sprintf
					"The field %s is already used." (snd p.p_name))
		in List.iter verify_field f.f_params;
		(* Add the structure and its fields. *)
		Env.add_structure f;

	(* Function. *)
	end else begin
		(* Verify that no structure as the same name. *)
		if Env.is_structure_defined name then
			Typ.type_error f.f_loc (sprintf
				"A structure has already the name %s. Functions can't have the same name as a structure."
				name);
		(* Declare the function. *)
		Env.declare_function name;
	end

let rec type1_expr env e = match snd e with
	(* Add the global variables to the environment. *)
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
		Env.add_variable var var_type (type1_expr env e1)
	| If (e1, b, eb) -> type1_else_block (type1_block env (e1 :: b)) eb
	| _ -> env

and type1_else_block env eb = match snd eb with
	(* Add the global variables to the environment. *)
	| End					-> env
	| Else b				-> type1_block env b
	| Elseif (e, b, eb1)	-> type1_else_block (type1_block env (e :: b)) eb1

and type1_block env =
	(* Add the global variables to the environment. *)
	List.fold_left type1_expr env

let rec type1 env = function
	(* Check the structure definitions.
	Check the function definitions (but not the bodies).
	Keep track of all the function and structure definition.
	Add the global variables to the environment. *)
	| Expr e :: l	-> type1 (type1_expr env e) l
	| Func f :: l	-> type1_func f; type1 env l
	| []			-> env

(* Second part. *)

let type2 env = function
	(* Construct the local environment of each function/while loop/for loop.
	Verify that all return instructions on a function have a type compatible with the return type.
	Verify that all function/while loop/for loop bodies are well typed.
	Verify that global expressions are well typed. *)
	| _ -> [] (* TODO *)

(* Part 1 & part 2. *)

let typing ast =
	(* Construct the typed ast, the global environment, the local environments and type check. *)
	let genv = type1 Env.bindings ast in
	let tast = type2 genv ast in
	genv, tast

