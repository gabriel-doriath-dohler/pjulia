open Tast
open X86_64
open Format

let h_string_number:((string, int) Hashtbl.t) = Hashtbl.create 16
let nb_total_string = ref 0

let string_number s =
	try Hashtbl.find h_string_number s
	with Not_found ->
		incr nb_total_string;
		Hashtbl.add h_string_number s !nb_total_string;
		!nb_total_string

let t_undef = -1
let t_nothing = 0
let t_int = 1
let t_str = 2
let t_bool = 3

let popn n = addq (imm n) !%rsp

let repeat code n =
	let total_code = ref nop in
	for i = 1 to n do
		total_code := !total_code ++ code
	done;
	!total_code

let printf s =
	(* Takes its argument in %rsi. *)
	movq (ilab (".Sprint_" ^ s)) !%rdi ++
	(* printf requires %rax to be set to zero. *)
	xorq !%rax !%rax ++
	call "printf"

let print_int =
	(* Takes its argument in %rsi. *)
	label "print_int" ++
	printf "int" ++
	ret

let print_str =
	(* Takes its argument in %rsi. *)
	label "print_str" ++
	movq !%rsi !%rdi ++
	(* printf requires %rax to be set to zero. *)
	xorq !%rax !%rax ++
	call "printf" ++
	ret

let print_bool =
	(* Takes its argument in %rsi. *)
	label "print_bool" ++
	testq !%rsi !%rsi ++
	jz "print_false" ++
	printf "true" ++
	ret ++

	label "print_false" ++
	printf "false" ++
	ret

let print =
	label "print" ++
	movq (ind ~ofs:16 rsp) !%rdi ++
	movq (ind ~ofs:8 rsp) !%rsi ++
	cmpq (imm t_int) !%rdi ++
	jz "print_int" ++
	cmpq (imm t_str) !%rdi ++
	jz "print_str" ++
	cmpq (imm t_bool) !%rdi ++
	jz "print_bool" ++
	ret
	(* error "Print cannot print this type." ++ *) (* TODO *)

let stdlib_print =
	print ++
	print_int ++
	print_str ++
	print_bool

let stdlib =
	stdlib_print

let rec compile_expr te = match te.te_e with (* TODO *)
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
			| "print" ->
				repeat (call "print" ++ popn 16) nb_args ++ (* TODO repeat nb_args *)
				xorq !%rax !%rax
			| _ -> assert false) (* TODO *)
	| _ -> pushq (imm 0) ++ pushq (imm 0)

let compile_stmt (code_fun, code) = function
	| TExpr texpr -> code_fun, code ++ compile_expr texpr
	| TFunc tfunc -> code_fun, code (* TODO *)

let gen tast ofile =
	(* Allocation. *)
	(* let p = alloc tast in *) (* TODO *)
	(* Code generation. *)
	let codefun, code = List.fold_left compile_stmt (nop, nop) tast in
	let strings = Hashtbl.fold
		(fun s nb c -> c ++ label (sprintf ".string_%d" nb) ++ string s) h_string_number nop in
	let global_variables = nop in (* TODO *)
	let s_print =
		(label ".Sprint_int" ++ string "%d") ++
		(label ".Sprint_true" ++ string "true") ++
		(label ".Sprint_false" ++ string "false")
	in
	let p =
		{ text =
			globl "main" ++ label "main" ++
			code ++
			xorq !%rax !%rax ++ (* Exit with code 0. *)
			ret ++
			stdlib ++
			codefun;
		data = strings ++ global_variables ++ s_print; } (* TODO *)
	in

	(* Write the program. *)
	let f = open_out ofile in
	let fmt = formatter_of_out_channel f in
	X86_64.print_program fmt p;
	fprintf fmt "@?"; (* Flush the buffer. *)
	close_out f

