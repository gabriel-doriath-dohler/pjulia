open Tast
open X86_64
open Format

(* Set of all the different sizes on wich we might call print. *)
let h_print_size:((int, unit) Hashtbl.t) = Hashtbl.create 16

(* Associate each string wich a label in data. *)
let h_string_number:((string, int) Hashtbl.t) = Hashtbl.create 16
let nb_total_string = ref 0

let string_number s =
	try Hashtbl.find h_string_number s
	with Not_found ->
		incr nb_total_string;
		Hashtbl.replace h_string_number s !nb_total_string;
		!nb_total_string

(* Code for the type of an object. *)
let t_undef = -1
let t_nothing = 0
let t_int = 1
let t_str = 2
let t_bool = 3

let popn n =
	if n = 0 then nop
	else addq (imm n) !%rsp

(* Error handeling. *)

(* Associate each error message wich a label in data. *)
let h_error_number:((string, int) Hashtbl.t) = Hashtbl.create 16
let nb_total_error = ref 0

let error_number s =
	try Hashtbl.find h_error_number s
	with Not_found ->
		incr nb_total_error;
		Hashtbl.replace h_error_number s !nb_total_error;
		!nb_total_error

let error jump error_msg =
	let n = error_number (sprintf "@.%s@." error_msg) in
	jump (sprintf ".code_error_%d" n)

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
	(* printf requires %rax to be set to zero. *)
	xorq !%rax !%rax ++
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
	(* TODO alignement *)
	(* rsi = numbers of arguments to print. *)
	label "print" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	movq !%rsi !%r12 ++
	xorq !%r13 !%r13 ++

	label ".print_loop" ++
	cmpq !%r12 !%r13 ++
	jz ".print_end" ++
	movq !%r13 !%r9 ++ (* TODO leaq *)
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

	error jmp "Error: print cannot print this type." ++

	label ".print_end" ++
	leave ++
	ret

(* Compilation. *)
let rec compile_expr te = match te.te_e with
	(* Constants. *)
	| TInt n	-> pushq (imm t_int) ++ pushq (imm64 n)
	| TStr s	->
		let n = string_number s in
		pushq (imm t_str) ++ pushq (ilab (sprintf ".string_%d" n))
	| TBool b	-> pushq (imm t_bool) ++ pushq (imm (if b then 1 else 0))

	(* Expressions with parentheses. *)
	| TCall ((_, name), args, f_list) ->
		let nb_args = List.length args in

		(* Compile the arguments and put them on the stack. *)
		List.fold_left (fun code arg -> compile_expr arg ++ code) nop args ++

		(* Compile the body of the function. *)
		(match name with
			| "print" ->
				movq (imm nb_args) !%rsi ++
				call "print" ++
				popn (16 * nb_args) ++
				pushq (imm t_nothing) ++
				pushq (imm 0)
			| _ -> popn (16 * nb_args)) (* TODO *)

	(* Operations. *)
	| TNot te	->
		compile_expr te ++
		popq rax ++ (* Value. *)
		popq rbx ++ (* Type. *)
		cmpq (imm t_bool) !%rbx ++
		error jnz "Type error: Not takes a bool." ++
		xorq (imm 1) !%rax ++
		pushq !%rbx ++
		pushq !%rax

	| _ -> pushq (imm 0) ++ pushq (imm 0) (* TODO *)

let compile_stmt (code_fun, code) = function
	| TExpr texpr -> code_fun, code ++ compile_expr texpr
	| TFunc tfunc -> code_fun, code (* TODO *)

let gen tast ofile =
	(* Allocation. *)
	(* let p = alloc tast in *) (* TODO *)

	(* Code generation. *)
	let codefun, code = List.fold_left compile_stmt (nop, nop) tast in

	(* Add the print functions. *)
	let print_functions =
		print ++ print_nothing ++ print_int ++ print_str ++ print_bool
	in
	let s_print =
		(label ".Sprint_nothing" ++ string "Nothing") ++
		(label ".Sprint_int" ++ string "%ld") ++
		(label ".Sprint_true" ++ string "true") ++
		(label ".Sprint_false" ++ string "false")
	in

	(* Add the strings to the data segment. *)
	let strings = Hashtbl.fold
		(fun s nb c -> c ++ label (sprintf ".string_%d" nb) ++ string s) h_string_number nop in

	(* Add the error messages to the data segment. *)
	let errors = Hashtbl.fold
		(fun s nb c -> c ++ label (sprintf ".error_%d" nb) ++ string s) h_error_number nop in

	(* Add the error printing functions. *)
	let code_errors = Hashtbl.fold
		(fun s nb c ->
			c ++
			label (sprintf ".code_error_%d" nb) ++

			(* Restore the original value of %rsp and %rbp *)
			movq !%r14 !%rbp ++
			movq !%r15 !%rsp ++

			(* Print the error message. *)

			movq (ilab (sprintf ".error_%d" nb)) !%rsi ++
			(* eprintf requires %rax to be set to zero. *)
			xorq !%rax !%rax ++
			(* Print on stderr. *)
			movq X86_64.stderr !%rdi ++
			call "fprintf" ++

			(* Return with code 1. *)
			movq (imm 1) !%rax ++
			ret;)
		h_error_number nop in

	let global_variables = nop in (* TODO *)

	let p =
		{ text =
			globl "main" ++ label "main" ++
			(* Keep %rbp in %r14 for error handeling. *)
			movq !%rbp !%r15 ++
			(* Keep %rsp in %r15 for error handeling. *)
			movq !%rsp !%r15 ++
			movq !%rsp !%rbp ++

			code ++
			(* Restore the original value of %rsp and %rbp *)
			movq !%r14 !%rbp ++
			movq !%r15 !%rsp ++
			(* Exit with code 0. *)
			xorq !%rax !%rax ++
			ret ++

			codefun ++
			print_functions ++
			code_errors;
		data = strings ++ errors ++ global_variables ++ s_print; }
	in

	(* Write the program. *)
	X86_64.print_in_file ~file:ofile p

