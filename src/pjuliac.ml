open Lexing
open Ast

(* Name of the input file. *)
let file = ref ""

(* Set the filename to s. *)
let set_file f s = f := s

(* Compilation options to stop after parsing or typing. *)
let parse_only = ref false
let type_only = ref false

(* Compilation option to print the parsed ast. *)
let print = ref false

(* Compilation option to print the tokens. *)
let debug = ref false

(* Compilation option to generate automatic tests randomly. *)
let nb_rand_test = ref 0

(* Compilation options. Can be printed with --help.*)
let usage = "Usage: pjuilac [option] file.jl"
let options = [
	("--parse-only", Arg.Set parse_only, "Do just the parsing.");
	("--type-only", Arg.Set type_only, "Do just the parsing and typing");
	("-n", Arg.Set_int nb_rand_test, "Number of random tests (default is 0).");
	("--print", Arg.Set print, "Print the parsed ast.");
	("--debug", Arg.Set debug, "Print the tokens."); ]

(* Print in stderr the localisation. *)
let print_localisation l =
	Format.eprintf
		"File \"%s\", line %d, character %d to line %d, character %d:@."
		!file l.l_start l.c_start l.l_end l.c_end

(* Open a file in read mode. *)
let open_file filename =
	try open_in filename
	with _ -> Format.eprintf "File: %s not found.@.@?" filename; exit 1

let () =
	(* Parse command line arguments. *)
	Arg.parse options (set_file file) usage;

	(* Check that a filename was given. *)
	if !file = "" && !nb_rand_test <= 0 then begin
		Format.eprintf "No file to process.@.@?";
		exit 1
	end;

	(* Check the file extention. *)
	if not (Filename.check_suffix !file ".jl") && !nb_rand_test <= 0 then begin
		Format.eprintf "The input file must have the extension .jl@.@?";
		Arg.usage options usage;
		exit 1
	end;

	(*
	(* TODO *)
	if !nb_rand_test > 0 then begin
		Format.printf "Starting %d random tests.@." !nb_rand_test;
		Random_test.test !nb_rand_test;
		Format.printf "@.Done. No errors where found.@.@?"
	end;
	*)

	(* Stop if no file is provided and random tests are generated. *)
	if !file = "" && !nb_rand_test > 0 then
		exit 0;

	(* Open the input file in read mode. *)
	let f = open_file !file in

	(* Create a lexing buffer. *)
	let lb = Lexing.from_channel f in

	try
		(* Parse. *)
		let ast = Parser.file (Lexer.next_token !debug) lb in
		close_in f;

		(*
		TODO
		(* Print. *)
		if !print then Format.printf "%a" Printer.print ast;
		*)

		(* Stop if we only want to parse. *)
		if !parse_only then exit 0;

		(*
		TODO
		(* Type. *)
		let genv, lenv, tast = Type.typing ast in
		*)

		if !type_only then exit 0;

		(* Interpret. *)

		(* Compile. *)

	with
		| Lexer.Lexing_error s ->
			print_localisation (current_loc lb);
			Format.eprintf "Lexical error: %s@.@?" s;
			exit 1
		| Parser.Error ->
			print_localisation (current_loc lb);
			Format.eprintf "Syntax error.@.@?";
			exit 1
		| Ast.Syntax_error s ->
			print_localisation (current_loc lb);
			Format.eprintf "Syntax error: %s@.@?" s;
			exit 1
		| Typ.Type_error (l, s) ->
			print_localisation l;
			Format.eprintf "Type error: %s@.@?" s;
			exit 1
