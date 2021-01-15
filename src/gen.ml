open Tast
open X86_64
open Format

let popn n =
	if n = 0 then nop
	else addq (imm n) !%rsp

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
	movq !%r14 !%rbp ++
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
	(* TODO Align. *)
	(* rsi = numbers of arguments to print. *)
	label ".print" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	movq !%rsi !%r12 ++
	xorq !%r13 !%r13 ++

	label ".print_loop" ++
	cmpq !%r12 !%r13 ++
	jz ".print_end" ++
	movq !%r13 !%r9 ++
	addq !%r9 !%r9 ++

	movq (ind ~ofs:24 ~index:r9 ~scale:8 rsp) !%rdi ++
	movq (ind ~ofs:16 ~index:r9 ~scale:8 rsp) !%rsi ++
	incq !%r13 ++

	cmpq (imm t_nothing) !%rdi ++
	jz ".print_nothing" ++
	cmpq (imm t_int) !%rdi ++
	jz ".print_int" ++
	cmpq (imm t_str) !%rdi ++
	jz ".print_str" ++
	cmpq (imm t_bool) !%rdi ++
	jz ".print_bool" ++

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

(* Compilation. *)
let rec compile_expr te = match te.te_e with
	(* Constants. *)
	| TInt n	-> pushq (imm t_int) ++ movq (imm64 n) !%rax ++ pushq !%rax
	| TStr s	-> pushq (imm t_str) ++ pushq (ilab (distinct_string s))
	| TBool b	-> pushq (imm t_bool) ++ pushq (imm (if b then 1 else 0))

	(* Expressions with parentheses. *)
	| TPar tb -> (* TODO *)
		List.fold_left (fun c e -> c ++ compile_expr e) nop tb.block_b ++
		popq rax ++
		popq rbx ++
		popn (16 * (List.length tb.block_b - 1)) ++
		pushq !%rbx ++
		pushq !%rax
	| TCall ((_, name), args, f_list) ->
		let nb_args = List.length args in

		(* Compile the arguments and put them on the stack. *)
		List.fold_left (fun code arg -> compile_expr arg ++ code) nop args ++

		(* Compile the body of the function. *)
		(match name with
			| "print" ->
				movq (imm nb_args) !%rsi ++
				call ".print" ++
				popn (16 * nb_args) ++
				pushq (imm t_nothing) ++
				pushq (imm 0)
			| _ -> popn (16 * nb_args)) (* TODO *)

	(* Operations. *)
	| TNot te	->
		compile_expr te ++
		popq rax ++ (* Value. *)
		popq rbx ++ (* Type. *)

		(* Type check. *)
		cmpq (imm t_bool) !%rbx ++
		error jnz "Type error: Not takes a bool." ++

		xorq (imm 1) !%rax ++
		pushq !%rbx ++
		pushq !%rax
	| TBinop (te1, Or, te2) ->
		(* Lazy evalutation. *)
		let v1 = !%rax in
		let t1 = !%rbx in
		let v2 = !%rcx in
		let t2 = !%rdx in
		let or_true = distinct_label ".or_true" in
		let or_end = distinct_label ".or_end" in
		xorq !%r8 !%r8 ++

		(* Evaluate te1. *)
		compile_expr te1 ++
		popq rax ++ (* Value 1. *)
		popq rbx ++ (* Type 1. *)
		(* Type check te1. *)
		cmpq (imm t_bool) t1 ++
		error jnz "Type error: Or takes a bool as a first argument." ++

		(* Test if v1 is true. *)
		testq v1 v1 ++
		jnz or_true ++

		(* Evaluate te2. *)
		compile_expr te2 ++
		popq rcx ++ (* Value 2. *)
		popq rdx ++ (* Type 2. *)
		(* Type check te2. *)
		cmpq (imm t_bool) t2 ++
		error jnz "Type error: Or takes a bool as a second argument." ++

		(* Test if v2 is false. *)
		testq v2 v2 ++
		jz or_end ++

		label or_true ++
		movq (imm 1) !%r8 ++

		label or_end ++
		pushq (imm t_bool) ++
		pushq !%r8
	| TBinop (te1, And, te2) ->
		(* Lazy evalutation. *)
		let v1 = !%rax in
		let t1 = !%rbx in
		let v2 = !%rcx in
		let t2 = !%rdx in
		let and_false = distinct_label ".and_false" in
		let and_end = distinct_label ".and_end" in
		movq (imm 1) !%r8 ++

		(* Evaluate te1. *)
		compile_expr te1 ++
		popq rax ++ (* Value 1. *)
		popq rbx ++ (* Type 1. *)
		(* Type check te1. *)
		cmpq (imm t_bool) t1 ++
		error jnz "Type error: And takes a bool as a first argument." ++

		(* Test if v1 is false. *)
		testq v1 v1 ++
		jz and_false ++

		(* Evaluate te2. *)
		compile_expr te2 ++
		popq rcx ++ (* Value 2. *)
		popq rdx ++ (* Type 2. *)
		(* Type check te2. *)
		cmpq (imm t_bool) t2 ++
		error jnz "Type error: And takes a bool as a second argument." ++

		(* Test if v2 is true. *)
		testq v2 v2 ++
		jnz and_end ++

		label and_false ++
		xorq !%r8 !%r8 ++

		label and_end ++
		pushq (imm t_bool) ++
		pushq !%r8

	| TBinop (te1, op, te2) ->
		let v1 = !%rax in
		let t1 = !%rbx in
		let v2 = !%rcx in
		let t2 = !%rdx in
		compile_expr te1 ++
		compile_expr te2 ++

		popq rcx ++ (* Value 2. *)
		popq rdx ++ (* Type 2. *)

		popq rax ++ (* Value 1. *)
		popq rbx ++ (* Type 1. *)

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

		(* Push the result. *)
		(match op with
			| Add | Sub | Mul -> pushq t1 ++ pushq v1
			| And | Or -> failwith "And and Or are compiled separately."
			| Mod | Pow -> pushq (imm t_int) ++ pushq !%rdx
			| Eq | Neq | L | Leq | G | Geq  -> pushq (imm t_bool) ++ pushq !%r8)

	| _ -> pushq (imm 0) ++ pushq (imm 0) (* TODO *)

let compile_stmt (code_func, code) = function
	| TExpr texpr -> code_func, code ++ compile_expr texpr
	| TFunc tfunc -> code_func, code (* TODO *)

let gen tast ofile =
	(* Allocation. *)
	(* let p = alloc tast in *) (* TODO *)

	(* Code generation. *)
	let code_func, code = List.fold_left compile_stmt (nop, nop) tast in

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
			(* Keep %rbp in %r14 for error handeling. *)
			movq !%rbp !%r15 ++
			(* Keep %rsp in %r15 for error handeling. *)
			movq !%rsp !%r15 ++
			movq !%rsp !%rbp ++

			comment "Code of the program." ++
			code ++

			comment "Exit 0." ++
			(* Restore the original value of %rsp and %rbp *)
			movq !%r14 !%rbp ++
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
			comment "Data for strings." ++
			data_string ++
			comment "Data for stdlib." ++
			data_stdlib ++
			comment "Data for error handeling." ++
			data_error; }
	in

	(* Write the program. *)
	X86_64.print_in_file ~file:ofile p

