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

(* Implementation of print. *)
let printf s =
	(* %rsi = data to print. *)
	movq (ilab (".Sprint_" ^ s)) !%rdi ++
	(* printf requires %rax to be set to zero. *)
	xorq !%rax !%rax ++
	call "printf"

let print_nothing =
	(* %rsi = data to print. *)
	label "print_nothing" ++
	printf "nothing" ++
	jmp "print_loop"

let print_int =
	(* %rsi = data to print. *)
	label "print_int" ++
	(* printf requires %rax to be set to zero. *)
	xorq !%rax !%rax ++
	printf "int" ++
	jmp "print_loop"

let print_str =
	(* %rsi = data to print. *)
	label "print_str" ++
	movq !%rsi !%rdi ++
	(* printf requires %rax to be set to zero. *)
	xorq !%rax !%rax ++
	call "printf" ++
	jmp "print_loop"

let print_bool =
	(* %rsi = data to print. *)
	label "print_bool" ++
	testq !%rsi !%rsi ++
	jz "print_false" ++
	printf "true" ++
	jmp "print_loop" ++

	label "print_false" ++
	printf "false" ++
	jmp "print_loop"

let print =
	(* rsi = numbers of arguments to print. *)
	label "print" ++
	(* pushq !%rbp ++
	movq !%rsp !%rbp ++ *)
	movq !%rsi !%r12 ++
	xorq !%r13 !%r13 ++

	label "print_loop" ++
	cmpq !%r12 !%r13 ++
	jz "print_end" ++
	movq !%r13 !%r9 ++ (* TODO leaq *)
	imulq (imm 2) !%r9 ++ (* TODO leaq *)

	movq (ind ~ofs:16 ~index:r9 ~scale:8 rsp) !%rdi ++
	movq (ind ~ofs:8 ~index:r9 ~scale:8 rsp) !%rsi ++
	incq !%r13 ++

	cmpq (imm t_nothing) !%rdi ++
	jz "print_nothing" ++
	cmpq (imm t_int) !%rdi ++
	jz "print_int" ++
	cmpq (imm t_str) !%rdi ++
	jz "print_str" ++
	cmpq (imm t_bool) !%rdi ++
	jz "print_bool" ++

	(* error "Print cannot print this type." ++ *) (* TODO *)

	label "print_end" ++
	(* leave ++ *)
	ret

(* Compilation. *)
let rec compile_expr te = match te.te_e with
	(* Constants. *)
	| TInt n	-> pushq (imm t_int) ++ pushq (imm64 n)
	| TStr s	->
		let n = string_number s in
		pushq (imm t_str) ++ pushq (ilab (sprintf ".string_%d" n))
	| TBool b	-> pushq (imm t_bool) ++ pushq (imm (if b then 1 else 0))

	(* Expressions with parantheses. *)
	| TCall ((_, name), args, f_list) ->
		let nb_args = List.length args in

		(* Compile the arguments and put them on the stack. *)
		List.fold_left (fun code arg -> compile_expr arg ++ code) nop args ++

		(* Compile the body of the function. *)
		(match name with
			| "print" -> movq (imm nb_args) !%rsi ++ call "print" ++ popn (16 * nb_args)
			| _ -> popn (16 * nb_args)) (* TODO *)
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
		(* Hashtbl.fold
			(fun n _ c -> c ++ generate_code_print n)
			h_print_size
			nop ++ *)
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

	let global_variables = nop in (* TODO *)

	let p =
		{ text =
			globl "main" ++ label "main" ++
			code ++
			xorq !%rax !%rax ++ (* Exit with code 0. *)
			ret ++
			print_functions ++
			codefun;
		data = strings ++ global_variables ++ s_print; }
	in

	(* Write the program. *)
	X86_64.print_in_file ~file:ofile p

