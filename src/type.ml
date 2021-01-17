open Ast
open Tast
open Format

(* TODO comment *)
let global = ref true

(* First part. *)

let type1_func f =
	(* Check the structure definitions.
	Check the function definitions (but not the bodies). *)

	let name = snd f.f_name in
	let func_or_struc = if f.f_is_constructor then "structure" else "function" in
	let arg_or_field = if f.f_is_constructor then "field" else "argument" in

	(* Verify that the name isn't typeof, div, print or println. *)
	if List.mem name [ "typeof"; "div"; "print"; "println" ] then
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
		(* Verify that the fields are not already used. *)
		let verify_field p =
			if Env.is_field_defined (snd p.p_name) then
				Typ.type_error (fst p.p_name) (sprintf
					"The field %s is already used." (snd p.p_name))
		in List.iter verify_field f.f_params;
		(* Add the structure, its type and its fields. *)
		Env.add_structure name f.f_params f.f_mutable;

	(* Function. *)
	end else begin
		(* Verify that no structure as the same name. *)
		if Env.is_structure_defined name then
			Typ.type_error f.f_loc (sprintf
				"A structure has already the name %s. Functions can't have the same name as a structure."
				name);
		(* Add the function. *)
		Env.add_function name f.f_params f.f_type f.f_mutable
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
			try
				if !global then
					Env.type_of var env
				else
					Env.local_type_of var env
			with Not_found -> Typ.Any
		in
		if !global then
			Env.add_global_variable var var_type (type1_expr env e1)
		else
			Env.add_local_variable var var_type (type1_expr env e1)
	| If (e1, b1, b2) -> type1_block (type1_block env (e1 :: b1)) b2
	| _ -> env

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

let empty_texpr =
	{ te_loc = { l_start = -1; l_end = -1; c_start = -1; c_end = -1; };
	te_e = TPar { block_type = Typ.Nothing; block_b = [] };
	te_type = Typ.Nothing; }

let type2_div l targs = (match targs with
	| [ a; b; ] ->
		Typ.assert_compatible a.te_loc a.te_type Typ.Int64;
		Typ.assert_compatible b.te_loc b.te_type Typ.Int64
	| _			->
		Typ.type_error l (sprintf
			"The function div takes 2 arguments but %d where given." (List.length targs)));
	[], Typ.Int64

let type2_typeof l targs = (match targs with
	| [ a; ] -> ()
	| _			->
		Typ.type_error l (sprintf
			"The function typeof takes one argument but %d where given." (List.length targs)));
	[], Typ.Int64


let rec type2_expr env e =
	let te, typ = match snd e with
		(* Constants. *)
		| Int n 	-> TInt n, Typ.Int64
		| Str s		-> TStr s, Typ.Str
		| Bool b	-> TBool b, Typ.Bool

		(* Expressions with parentheses. *)
		| Par b -> let tb = type2_block env b in TPar tb, tb.block_type
		| Call ((l, name), args) ->
			let targs = List.map (type2_expr env) args in
			let f_list, t_ret = (match name with
				| "div"		-> type2_div l targs
				| "print"	-> [], Typ.Nothing
				| "typeof"	-> type2_typeof l targs
				| _			->
					let t_list = List.map (fun x -> x.te_type) targs in
					let f_list = Env.compatible_functions name t_list in
					if List.length f_list = 0 then
						Typ.type_error l (sprintf "There is no function or constructor named %s." name);
					f_list, match f_list with
						| [(_, t)]	-> t
						| _			-> Typ.Any)
			in TCall ((l, name), targs, f_list), t_ret

		(* Operations. *)
		| Not e ->
			let te = type2_expr env e in
			Typ.assert_compatible te.te_loc te.te_type Typ.Bool;
			TNot te, Typ.Bool
		| Binop (e1, op, e2) ->
			let te1 = type2_expr env e1 in
			let te2 = type2_expr env e2 in
			let t1 = te1.te_type in
			let t2 = te2.te_type in
			let l1 = te1.te_loc in
			let l2 = te2.te_loc in
			(TBinop (te1, op, te2), match op with
				| Eq | Neq -> Typ.Bool
				| Add | Sub | Mul | Mod | Pow ->
					Typ.assert_compatible l1 t1 Typ.Int64;
					Typ.assert_compatible l2 t2 Typ.Int64;
					Typ.Int64
				| L | Leq | G | Geq ->
					if t1 <> Typ.Any && t1 <> Typ.Int64 && t1 <> Typ.Bool then
						Typ.type_error l1 (asprintf
							"This expression as type %a but an expression was expected of type Any, Int64 or Bool."
							Typ.print t1);
					if t2 <> Typ.Any && t2 <> Typ.Int64 && t2 <> Typ.Bool then
						Typ.type_error l2 (asprintf
							"This expression as type %a but an expression was expected of type Any, Int64 or Bool."
							Typ.print t2);
					Typ.Bool
				| And | Or ->
					Typ.assert_compatible l1 t1 Typ.Bool;
					Typ.assert_compatible l2 t2 Typ.Bool;
					Typ.Bool)

		| Lval (l_lval, Var (l_var, var)) ->
			Env.assert_variable_defined l_var var env;
			let t = Env.type_of var env in
			TLval { lvalue_loc = l_lval;
				lvalue_type = t;
				lvalue_lvalue = TVar (l_var, var); }, t
		| Lval (l_lval, Field (e1, (l_field, field))) ->
			let te1 = type2_expr env e1 in
			Env.assert_field_defined l_field field;
			let s = Env.struct_name_of_field field in
			let t = Env.type_of_field field in
			Typ.assert_compatible te1.te_loc te1.te_type (Typ.Struct s);
			TLval { lvalue_loc = l_lval;
				lvalue_type = t;
				lvalue_lvalue = TField (te1, (l_field, field)); }, t
		| Affect ((l_lval, Var (l_var, var)), e1) ->
			let t = Env.type_of var env in
			let te1 = type2_expr env e1 in
			Typ.assert_subtype te1.te_loc te1.te_type t;
			TAffect ({ lvalue_loc = l_lval;
				lvalue_type = t;
				lvalue_lvalue = TVar (l_var, var); }, te1), te1.te_type
		| Affect ((l_lval, Field (e1 , (l_field, field))), e2) ->
			let te1 = type2_expr env e1 in
			let te2 = type2_expr env e2 in
			Env.assert_field_defined l_field field;
			let s = Env.struct_name_of_field field in
			let t = Env.type_of_field field in
			Typ.assert_compatible te2.te_loc te2.te_type t;
			Typ.assert_compatible te1.te_loc te1.te_type (Typ.Struct s);
			Env.assert_mutable l_field field s;
			TAffect ({ lvalue_loc = l_lval;
				lvalue_type = t;
				lvalue_lvalue = TField (te1, (l_field, field)); }, te2), t
		| Return None -> TReturn None, Typ.Any
		| Return (Some e1) ->
			let te1 = type2_expr env e1 in
			TReturn (Some te1), Typ.Any

		(* Control structures. *)
		| For (idx, e1, e2, b) ->
			let te1 = type2_expr env e1 in
			let te2 = type2_expr env e2 in
			Typ.assert_compatible te1.te_loc te1.te_type Typ.Int64;
			Typ.assert_compatible te2.te_loc te2.te_type Typ.Int64;
			let env_idx = Env.add_local_variable (snd idx) Typ.Int64 env in
			let lenv = type1_block env_idx b in
			let tb = type2_block lenv b in
			TFor { for_loc = fst e;
				for_expr = (idx, te1, te2, tb);
				for_env = lenv; }, Typ.Nothing
		| While ((l, e1), b) ->
			let te1 = type2_expr env (l, e1) in
			Typ.assert_compatible l te1.te_type Typ.Bool;
			let lenv = type1_block env b in
			let tb = type2_block lenv b in
			TWhile { while_loc = l;
				while_expr = (te1, tb);
				while_env = lenv; }, Typ.Nothing
		| If (e1, b1, b2) ->
			let te1 = type2_expr env e1 in
			let tb1 = type2_block env b1 in
			let tb2 = type2_block env b2 in
			Typ.assert_compatible te1.te_loc te1.te_type Typ.Bool;
			if tb1.block_type = tb2.block_type then
				TIf (te1, tb1, tb2), tb1.block_type
			else
				TIf (te1, tb1, tb2), Typ.Any

	in { te_loc = fst e; te_e = te; te_type = typ; }

and type2_block env = function
	| []		-> { block_b = []; block_type = Typ.Nothing; }
	| e :: b	->
		let te = type2_expr env e in
		let tb = type2_block env b in
		if List.length tb.block_b = 0 then
			{ block_b = [te]; block_type = te.te_type; }
		else
			{ block_b = te :: tb.block_b; block_type = tb.block_type; }

let assert_return_type tbody typ =
	let assert_return_type_expr te = match te.te_e with
		| TReturn None			-> Typ.assert_compatible te.te_loc Typ.Nothing typ
		| TReturn (Some te1)	-> Typ.assert_compatible te1.te_loc te1.te_type typ
		| _						-> ()
	in List.iter assert_return_type_expr tbody.block_b

let type2_func env f =
	(* Structure. *)
	if f.f_is_constructor then
		{ tf_name = f.f_name;
		tf_loc = f.f_loc;
		tf_params = f.f_params;
		tf_type = f.f_type;
		tf_body = { block_type = Typ.Nothing; block_b = []};
		tf_is_constructor = f.f_is_constructor;
		tf_mutable = f.f_mutable;
		tf_env = Env.empty_env;
		tf_fpmax = 0; }

	(* Function. *)
	else begin
		let env_param = List.fold_left
			(fun e -> fun p -> Env.add_local_variable ~param:true (snd p.p_name) p.p_type e)
			env (List.rev f.f_params) in
		let lenv = type1_block env_param f.f_body in
		let tb = type2_block lenv f.f_body in
		Typ.assert_compatible f.f_loc tb.block_type f.f_type; (* TODO loc *)
		assert_return_type tb f.f_type;
		let tf =
			{ tf_name = f.f_name;
			tf_loc = f.f_loc;
			tf_params = f.f_params;
			tf_type = f.f_type;
			tf_body = tb;
			tf_is_constructor = f.f_is_constructor;
			tf_mutable = f.f_mutable;
			tf_env = lenv;
			tf_fpmax = 0; } in
		Env.add_tfunction tf;
		tf
	end

let rec type2 env = function
	(* Construct the local environment of each function/while loop/for loop.
	Verify that all return instructions on a function have a type compatible with the return type.
	Verify that all function/while loop/for loop bodies are well typed.
	Verify that global expressions are well typed. *)
	| []			-> []
	| Func f :: l	-> TFunc (type2_func env f) :: type2 env l
	| Expr e :: l	-> TExpr (type2_expr env e) :: type2 env l (* /!\ Its not a tail recursive call. *)

(* Part 1 & part 2. *)

let typing ast =
	(* Construct the typed ast, the global environment, the local environments and type check. *)
	let genv = type1 Env.empty_env ast in
	global := false;
	let tast = type2 genv ast in
	genv, tast

