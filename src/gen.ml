open Ast
open Tast
open X86_64
open Format

let rec repeat n c = match n with
	| 0 -> nop
	| _ -> c ++ repeat (n - 1) c

let popn n =
	if n = 0 then nop
	else addq (imm n) !%rsp

let pushn n =
	repeat n (pushq (imm 1) ++ pushq (imm 1))

let func_name name = sprintf "func_%s" name

(* Code for the type of an object. *)
let t_undef = -1
let t_nothing = 0
let t_int = 1
let t_str = 2
let t_bool = 3

(* Add a number to the label so that it is unique. *)
let distinct_label =
	let h_label_number:((string, int) Hashtbl.t) = Hashtbl.create 16 in
	(fun s ->
		let n = try 1 + Hashtbl.find h_label_number s with Not_found -> 1 in
		let name = sprintf "%s_%d" s n in
		Hashtbl.replace h_label_number s n;
		name)

(* Associate each string with a label in data. *)
let h_string_number:((string, int) Hashtbl.t) = Hashtbl.create 16

let string_number =
	let nb_total_string = ref 0 in
	(fun s ->
		try Hashtbl.find h_string_number s
		with Not_found ->
			incr nb_total_string;
			Hashtbl.replace h_string_number s !nb_total_string;
			!nb_total_string)

let distinct_string s =
	sprintf ".string_%d" (string_number s)

(* Error handeling. *)

(* Associate each error message wich a label in data. *)
let h_error_number:((string, int) Hashtbl.t) = Hashtbl.create 16

let error_number =
	let nb_total_error = ref 0 in
	(fun s ->
		try Hashtbl.find h_error_number s
		with Not_found ->
			incr nb_total_error;
			Hashtbl.replace h_error_number s !nb_total_error;
			!nb_total_error)

let error jump error_msg =
	let n = error_number (sprintf "@.Error: %s@." error_msg) in
	jump (sprintf ".code_error_%d" n)

let code_error_n n =
	(* Add the error printing function for error n. *)
	label (sprintf ".code_error_%d" n) ++

	(* Restore the original value of %rsp and %rbp *)
	movq !%r12 !%rbp ++
	movq !%r15 !%rsp ++

	(* Print the error message. *)

	movq (ilab (sprintf ".error_%d" n)) !%rsi ++
	(* fprintf requires %rax to be set to zero. *)
	xorq !%rax !%rax ++
	(* Print on stderr. *)
	movq X86_64.stderr !%rdi ++
	call "fprintf" ++

	(* Return with code 1. *)
	movq (imm 1) !%rax ++
	ret

let assert_is_defined addr =
	testq (imm 1) !%addr ++
	error jnz "Variable undefined."

(* Set rdi with rsi.*)
let set typ value =
	movq (imm 16) !%rdi ++
	pushq !%typ ++
	pushq !%value ++
	call "malloc" ++
	popq value ++
	popq typ ++
	movq !%typ (ind ~ofs:0 rax) ++
	movq !%value (ind ~ofs:(8) rax) ++
	movq !%rax !%rdi

let set_int =
	movq (imm 16) !%rdi ++
	pushq !%rsi ++
	pushq !%rsi ++
	call "malloc" ++
	popq rsi ++
	popq rsi ++
	movq (imm t_int) (ind ~ofs:0 rax) ++
	movq !%rsi (ind ~ofs:(8) rax) ++
	movq !%rax !%rdi

let set_str s =
	movq (imm 16) !%rdi ++
	call "malloc" ++
	movq (imm t_str) (ind ~ofs:0 rax) ++
	movq (ilab (distinct_string s)) (ind ~ofs:8 rax) ++
	movq !%rax !%rdi

let set_bool =
	movq (imm 16) !%rdi ++
	pushq !%rsi ++
	pushq !%rsi ++
	call "malloc" ++
	popq rsi ++
	popq rsi ++
	movq (imm t_bool) (ind ~ofs:0 rax) ++
	movq !%rsi (ind ~ofs:8 rax) ++
	movq !%rax !%rdi

let set_nothing =
	movq (imm 16) !%rdi ++
	call "malloc" ++
	movq (imm t_nothing) (ind ~ofs:0 rax) ++
	movq (imm 0) (ind ~ofs:8 rax) ++
	movq !%rax !%rdi

(* Get. *)
let get_int reg =
	assert_is_defined rdi ++
	movq (ind ~ofs:0 rdi) !%rbx ++
	cmpq (imm t_int) !%rbx ++
	error jnz "Type error: the expression should have type int." ++
	movq (ind ~ofs:8 rdi) reg

let get_bool reg =
	assert_is_defined rdi ++
	movq (ind ~ofs:0 rdi) !%rbx ++
	cmpq (imm t_bool) !%rbx ++
	error jnz "Type error: the expression should have type bool." ++
	movq (ind ~ofs:8 rdi) reg

let get_from_pointer pointer reg_type reg_value =
	movq pointer !%r8 ++
	assert_is_defined r8 ++
	movq (ind ~ofs:8 r8) reg_value ++
	movq (ind ~ofs:0 r8) reg_type

let get reg_type reg_value =
	assert_is_defined rdi ++
	movq (ind ~ofs:8 rdi) reg_value ++
	movq (ind ~ofs:0 rdi) reg_type

(* Implementation of print. *)
let printf s =
	(* %rsi = data to print. *)
	movq (ilab (".Sprint_" ^ s)) !%rdi ++
	(* printf requires %rax to be set to zero. *)
	xorq !%rax !%rax ++
	call "printf"

let print_nothing =
	(* %rsi = data to print. *)
	label ".print_nothing" ++
	printf "nothing" ++
	jmp ".print_loop"

let print_int =
	(* %rsi = data to print. *)
	label ".print_int" ++
	printf "int" ++
	jmp ".print_loop"

let print_str =
	(* %rsi = data to print. *)
	label ".print_str" ++
	movq !%rsi !%rdi ++
	(* printf requires %rax to be set to zero. *)
	xorq !%rax !%rax ++
	call "printf" ++
	jmp ".print_loop"

let print_bool =
	(* %rsi = data to print. *)
	label ".print_bool" ++
	testq !%rsi !%rsi ++
	jz ".print_false" ++
	printf "true" ++
	jmp ".print_loop" ++

	label ".print_false" ++
	printf "false" ++
	jmp ".print_loop"

let print =
	(* rsi = numbers of arguments to print. *)
	label ".print" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	movq !%rsi !%r14 ++


	label ".print_loop" ++
	testq !%r14 !%r14 ++
	jz ".print_end" ++

	movq !%r14 !%r9 ++
	addq !%r14 !%r9 ++
	get_from_pointer (ind ~ofs:8 ~index:r9 ~scale:8 rbp) !%rdi !%rsi ++
	decq !%r14 ++

	cmpq (imm t_nothing) !%rdi ++
	jz ".print_nothing" ++
	cmpq (imm t_int) !%rdi ++
	jz ".print_int" ++
	cmpq (imm t_str) !%rdi ++
	jz ".print_str" ++
	cmpq (imm t_bool) !%rdi ++
	jz ".print_bool" ++

	(* jmp ".print_int" ++ *)
	error jmp "Print cannot print this type." ++

	label ".print_end" ++
	leave ++
	ret

(* stdlib *)
let pow =
	(* rdx <- rax^rcx
	rcx = power counter
	rsi = tmp for tests on the power counter
	rbx = res1
	rdx = res2
	res = res1 * res2 (in rdx) *)
	label ".pow" ++
	movq (imm 1) !%rbx ++
	movq (imm 1) !%rdx ++

	cmpq (imm 0) !%rcx ++
	error js "Negative power." ++

	label ".pow_loop" ++
	testq !%rcx !%rcx ++
	jz ".pow_end" ++
	cmpq (imm 1) !%rcx ++
	jz ".pow_1" ++

	movq !%rcx !%rsi ++
	andq (imm 1) !%rsi ++
	jz ".pow_even" ++
	imulq !%rax !%rdx ++
	label ".pow_even" ++
	imulq !%rax !%rax ++
	imulq !%rbx !%rbx ++
	shrq (imm 1) !%rcx ++

	jmp ".pow_loop" ++

	label ".pow_1" ++
	imulq !%rax !%rdx ++
	label ".pow_end" ++
	imulq !%rbx !%rdx ++
	ret

let code_stdlib =
	print ++ print_nothing ++ print_int ++ print_str ++ print_bool ++
	pow

let data_stdlib =
	(* Data for the print functions. *)
	(label ".Sprint_nothing" ++ string "nothing") ++
	(label ".Sprint_int" ++ string "%ld") ++
	(label ".Sprint_true" ++ string "true") ++
	(label ".Sprint_false" ++ string "false")

(* Global variables. *)
let label_type_from_gvar var =
	sprintf "gvar_type_%s" var

let label_value_from_gvar var =
	sprintf "gvar_value_%s" var

(* Compilation. *)

let rec compile_expr env te = match te.te_e with
	(* Constants. *)
	| TInt n	-> movq (imm64 n) !%rsi ++ set_int
	| TStr s	-> set_str s
	| TBool b	-> movq (imm (if b then 1 else 0)) !%rsi ++ set_bool

	(* Expressions with parentheses. *)
	| TPar tb -> compile_bloc env tb
	| TCall ((_, name), args, f_list) ->
		let nb_args = List.length args in

		(* Compile the arguments and put them on the stack. *)
		List.fold_left (fun code arg -> code ++ compile_expr env arg ++ pushq !%rdi ++ pushq (imm 1)) nop args ++

		(* Compile the body of the function. *)
		(match name with
			| "print" ->
				movq (imm nb_args) !%rsi ++
				call ".print" ++
				set_nothing
			| "div" ->
				let v1 = !%rax in
				let t1 = !%rbx in
				let v2 = !%rcx in
				let t2 = !%rdx in
				get_from_pointer (ind ~ofs:8 rsp) t2 v2 ++
				get_from_pointer (ind ~ofs:24 rsp) t1 v1 ++
				(* Type check. *)
				movq (imm nb_args) !%r8 ++
				cmpq (imm 2) !%r8 ++
				error jnz "Div takes two arguments." ++

				cmpq (imm t_int) t1 ++
				error jnz "Type error: Div's first argument should be an int." ++
				cmpq (imm t_int) t2 ++
				error jnz "Type error: Div's second argument should be an int." ++
				testq v2 v2 ++
				error jz "Division by zero." ++
				(* Divide. *)
				cqto ++
				idivq v2 ++
				movq v1 !%rsi ++
				set_int
			| "typeof" ->
				(* Type check. *)
				movq (imm nb_args) !%r8 ++
				cmpq (imm 1) !%r8 ++
				error jnz "Typeof takes one argument." ++
				movq (ind ~ofs:0 rdi) !%rsi ++
				set_int
			| _ -> call (func_name name) ) ++

		(* Deallocate the arguments. *)
		popn (16 * nb_args)

	(* Operations. *)
	| TNot te	->
		compile_expr env te ++
		get_bool !%rsi ++

		xorq (imm 1) !%rsi ++
		set_bool
	| TBinop (te1, Or, te2) ->
		(* Lazy evaluation. *)
		let v1 = !%rax in
		let v2 = !%rbx in
		let or_true = distinct_label ".or_true" in
		let or_end = distinct_label ".or_end" in
		(* Evaluate te1. *)
		compile_expr env te1 ++
		get_bool v1 ++

		xorq !%rsi !%rsi ++

		(* Test if v1 is true. *)
		testq v1 v1 ++
		jnz or_true ++

		(* Evaluate te2. *)
		compile_expr env te2 ++
		get_bool v2 ++

		xorq !%rsi !%rsi ++

		(* Test if v2 is false. *)
		testq v2 v2 ++
		jz or_end ++

		label or_true ++
		movq (imm 1) !%rsi ++

		label or_end ++
		set_bool
	| TBinop (te1, And, te2) ->
		(* Lazy evaluation. *)
		let v1 = !%rax in
		let v2 = !%rbx in
		let and_false = distinct_label ".and_false" in
		let and_end = distinct_label ".and_end" in

		(* Evaluate te1. *)
		compile_expr env te1 ++
		get_bool v1 ++

		movq (imm 1) !%rsi ++

		(* Test if v1 is false. *)
		testq v1 v1 ++
		jz and_false ++

		(* Evaluate te2. *)
		compile_expr env te2 ++
		get_bool v2 ++

		movq (imm 1) !%rsi ++

		(* Test if v2 is true. *)
		testq v2 v2 ++
		jnz and_end ++

		label and_false ++
		xorq !%rsi !%rsi ++

		label and_end ++
		set_bool
	| TBinop (te1, op, te2) ->
		let v1 = !%rax in
		let t1 = !%rbx in
		let v2 = !%rcx in
		let t2 = !%rdx in
		compile_expr env te1 ++
		get t1 v1 ++
		pushq t1 ++
		pushq v1 ++

		compile_expr env te2 ++
		get t2 v2 ++
		popq rax ++
		popq rbx ++

		(* Type check. *)
		(match op with
			| Add | Sub | Mul | Mod | Pow ->
				cmpq (imm t_int) t1 ++
				error jnz "Type error: Arithmetic operations take an int as a first argument." ++
				cmpq (imm t_int) t1 ++
				error jnz "Type error: Arithmetic operations take an int as a second argument."
			| And | Or -> failwith "And and Or are compiled separately."
			| Eq | Neq ->
				(* Transform bool to int.
				Idea from Samuel and Constantin (used with permission). *)
				movq (imm t_int) !%r8 ++
				cmpq (imm t_bool) t1 ++
				cmovzq !%r8 t1 ++
				cmpq (imm t_bool) t2 ++
				cmovzq !%r8 t2
			| L | Leq | G | Geq ->
				let cmp_arg1_type_ok = distinct_label ".cmp_arg1_type_ok" in
				let cmp_arg2_type_ok = distinct_label ".cmp_arg2_type_ok" in
				cmpq (imm t_bool) t1 ++
				jz cmp_arg1_type_ok ++
				cmpq (imm t_int) t1 ++
				error jnz "Type error: Comparisons take a bool or an int as a first argument." ++

				label cmp_arg1_type_ok ++
				cmpq (imm t_bool) t2 ++
				jz cmp_arg2_type_ok ++
				cmpq (imm t_int) t2 ++
				error jnz "Type error: Comparisons take a bool or an int as a second argument." ++
				label cmp_arg2_type_ok) ++

		(* Compile the operation. *)
		(match op with
			| Add	-> addq v2 v1
			| Sub	-> subq v2 v1
			| Mul	-> imulq v2 v1
			| Mod	->
				testq v2 v2 ++
				error jz "Division by zero." ++
				cqto ++
				idivq v2
			| Pow	-> call ".pow"
			| And | Or -> failwith "And and Or are compiled separately."
			| Eq	->
				xorq !%r9 !%r9 ++
				movq (imm 1) !%r8 ++
				cmpq t1 t2 ++
				cmovnzq !%r9 !%r8 ++
				cmpq v1 v2 ++
				cmovnzq !%r9 !%r8
			| Neq	->
				xorq !%r9 !%r9 ++
				movq (imm 1) !%r8 ++
				cmpq t1 t2 ++
				cmovnzq !%r9 !%r8 ++
				cmpq v1 v2 ++
				cmovnzq !%r9 !%r8 ++
				xorq (imm 1) !%r8
			| L		->
				xorq !%r8 !%r8 ++
				cmpq v2 v1 ++
				setl !%r8b
			| Leq	->
				xorq !%r8 !%r8 ++
				cmpq v2 v1 ++
				setle !%r8b
			| G		->
				xorq !%r8 !%r8 ++
				cmpq v2 v1 ++
				setg !%r8b
			| Geq	->
				xorq !%r8 !%r8 ++
				cmpq v2 v1 ++
				setge !%r8b) ++

		(* Save the result. *)
		(match op with
			| Add | Sub | Mul -> movq v1 !%rsi ++ set_int
			| And | Or -> failwith "And and Or are compiled separately."
			| Mod | Pow -> movq !%rdx !%rsi ++ set_int
			| Eq | Neq | L | Leq | G | Geq  -> movq !%r8 !%rsi ++ set_bool)

	| TLval { lvalue_loc = _; lvalue_type = _; lvalue_lvalue = TVar (l, var); } ->
		if Env.is_global var env then
			(* Verify that the variable is defined. *)
			movq (lab (label_type_from_gvar var)) !%rbx ++
			cmpq (imm t_undef) !%rbx ++
			error jz (sprintf "Variable %s undefined." var) ++
			movq (lab (label_value_from_gvar var)) !%rcx ++

			set rbx rcx
		else begin
			let ofs = Env.offset_of var env in
			get_from_pointer (ind ~ofs:ofs rbp) !%rbx !%rcx ++
			set rbx rcx
		end
	| TAffect ({ lvalue_loc = _; lvalue_type = _; lvalue_lvalue = TVar (l, var); }, te1) ->
		compile_expr env te1 ++
		get !%rdx !%rcx ++
		(if Env.is_global var env then
			movq !%rcx (lab (label_value_from_gvar var)) ++
			movq !%rdx (lab (label_type_from_gvar var))
		else
			let ofs = Env.offset_of var env in
			set rdx rcx ++
			movq !%rdi (ind ~ofs:ofs rbp))

	(* Control structures. *)
	| TIf (cond, tb, teb) ->
		let else_label = distinct_label "else_label" in
		let if_end = distinct_label "if_end" in
		compile_expr env cond ++
		get_bool !%rax ++

		testq !%rax !%rax ++
		jz else_label ++

		compile_bloc env tb ++
		jmp if_end ++

		label else_label ++
		compile_bloc env teb ++

		label if_end
	| TFor tfor ->
		let (_, idx), inf, sup, tb = tfor.for_expr in
		let ofs = Env.offset_of idx tfor.for_env in
		let for_end = distinct_label "for_end" in
		let for_loop = distinct_label "for_loop" in
		Imap.iter (fun name ofs -> Format.printf "@.%s: %s = %d@.@?" idx name ofs) tfor.for_env.ofs; (* TODO *)
		(* Keep r13. *)
		pushq !%r13 ++
		pushq !%r13 ++

		(* Lower bound. *)
		compile_expr env inf ++

		(* Init index. *)
		movq !%rdi (ind ~ofs:ofs rbp) ++

		(* Upper bound. *)
		compile_expr env sup ++

		get_int !%r13 ++
		(* For loop logic. *)
		label for_loop ++
		movq (ind ~ofs:ofs rbp) !%rdi ++
		get_int !%r11 ++
		cmpq !%r11 !%r13 ++
		jl for_end ++

		(* Keep r11. *)
		pushq !%r11 ++
		pushq !%r11 ++
		compile_bloc tfor.for_env tb ++
		(* Restore r11. *)
		popq r11 ++
		popq r11 ++

		incq !%r11 ++
		movq !%r11 !%rsi ++
		set_int ++
		movq !%rdi (ind ~ofs:ofs rbp) ++

		jmp for_loop ++
		label for_end ++
		(* Restore r13. *)
		popq r13 ++
		popq r13 ++
		set_nothing


		(*
		let _, inf, sup, tb = tfor.for_expr in
		let nb_lvar = (Imap.cardinal tfor.for_env.l) - 1 in
		let for_end = distinct_label "for_end" in
		let for_loop = distinct_label "for_loop" in
		pushq !%r13 ++
		pushq !%r12 ++
		compile_expr env inf ++
		get_int !%r13 ++

		compile_expr env sup ++
		get_int !%r12 ++

		pushq !%rbp ++
		pushq !%rbp ++
		movq !rsp !rbp ++
		(* Allocate the local variables. *)
		pushn nb_lvar ++

		(* For loop logic. *)
		label for_loop ++
		cmpq !%r13 !%r12 ++
		jl for_end ++
		incrq !%r13 ++

		(* Compile the body. *)
		compile_bloc tfor.for_env tb ++
		jmp for_loop ++

		label for_end ++
		(* Deallocate the local variables. *)
		popn (16 * nb_lvar) ++
		popq rbp ++
		leave ++
		(* Deallocate the arguments. *)
		popn (16 * 2) ++
		popq r12 ++
		popq r13 ++
		set_nothing *)
	| _ -> failwith "Not implemented."

and compile_bloc env tb = compile_expr_list env tb.block_b

and compile_expr_list env = function
	| []		-> set_nothing
	| [te]		-> compile_expr env te
	| te :: tb	-> compile_expr env te ++ compile_expr_list env tb

let compile_func tf =
	let nb_lvar = Imap.cardinal tf.tf_env.l in
	let nb_args = List.length tf.tf_params in
	Imap.iter (fun name ofs -> Format.printf "@.%s = %d@.@?" name ofs) tf.tf_env.ofs; (* TODO *)
	label (func_name (snd tf.tf_name)) ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	(* Allocate the local variables. *)
	pushn (nb_lvar - nb_args) ++
	(* Compile the body. *)
	compile_bloc tf.tf_env tf.tf_body ++
	(* Deallocate the local variables. *)
	popn (16 * (nb_lvar - nb_args)) ++
	(* Return. *)
	leave ++
	ret

let compile_stmt env (code_func, code) = function
	| TExpr texpr -> code_func, code ++ compile_expr env texpr
	| TFunc tfunc -> code_func ++ compile_func tfunc, code

let gen genv tast ofile =
	(* Code generation. *)
	let code_func, code = List.fold_left (compile_stmt genv) (nop, nop) tast in

	(* Add the global variables to the data segment. *)
	let data_gvar = Imap.fold
		(fun var _ c ->
			label (label_type_from_gvar var) ++
			dquad [t_undef] ++
			label (label_value_from_gvar var) ++
			dquad [0] ++ c)
		genv.g nop in

	(* Add the strings to the data segment. *)
	let data_string = Hashtbl.fold
		(fun s nb c -> c ++ label (sprintf ".string_%d" nb) ++ string s)
		h_string_number nop in

	(* Add the error messages to the data segment. *)
	let data_error = Hashtbl.fold
		(fun s nb c -> c ++ label (sprintf ".error_%d" nb) ++ string s)
		h_error_number nop in

	(* Add the error printing functions. *)
	let code_error = Hashtbl.fold
		(fun _ n c -> c ++ code_error_n n)
		h_error_number nop in

	let p =
		{ text =
			globl "main" ++ label "main" ++
			(* Keep rbp in r12 and rsp in r15 for error handeling. *)
			movq !%rbp !%r12 ++
			movq !%rsp !%r15 ++
			movq !%rsp !%rbp ++

			(* Define nothing. *)
			xorq !%rax !%rax ++
			movq (imm t_nothing) !%rbx ++
			movq !%rax (lab (label_value_from_gvar "nothing")) ++
			movq !%rbx (lab (label_type_from_gvar "nothing")) ++

			(* Alloc. *)
			pushn 2 ++ (* TODO *)
			pushq !%rbp ++
			pushq !%rbp ++
			movq !%rsp !%rbp ++

			comment "Code of the program." ++
			code ++

			comment "Exit 0." ++
			(* Restore the original value of %rsp and %rbp *)
			movq !%r12 !%rbp ++
			movq !%r15 !%rsp ++

			(* Exit with code 0. *)
			xorq !%rax !%rax ++
			ret ++

			comment "Code of functions." ++
			code_func ++
			comment "Stdlib code." ++
			code_stdlib ++
			comment "Code for error handeling." ++
			code_error;
		data =
			comment "Data for global variables." ++
			data_gvar ++
			comment "Data for strings." ++
			data_string ++
			comment "Data for stdlib." ++
			data_stdlib ++
			comment "Data for error handeling." ++
			data_error; }
	in

	(* Write the program. *)
	X86_64.print_in_file ~file:ofile p

