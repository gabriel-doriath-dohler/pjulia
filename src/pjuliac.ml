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

(*
Print in stderr the current position in the form of
a file name followed by row and column position.
*)
let localisation pos =
	let l = pos.pos_lnum in
	let c = pos.pos_cnum - pos.pos_bol + 1 in
	Format.eprintf "File \"%s\", line %d, characters %d-%d:@." !file l (c-1) c

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
	()

	(*
	(* TODO *)
	try
		(* Parse. *)
		let ast = Parser.file (Lexer.next_token !debug) lb in
		close_in f;

		(* Print. *)
		if !print then Format.printf "%a" Printer.print ast;

		(* Stop if we only want to parse. *)
		if !parse_only then exit 0;

		(* Type. *)
		let genv, lenv, tast = Type.typing ast in (* The variables aren't used yet. *)

		if !type_only then exit 0;

		(* Interpret. *)

		(* Compile. *)

	with
		| Lexer.Lexing_error s ->
			localisation (Lexing.lexeme_start_p lb);
			Format.eprintf "Lexical error: %s@.@?" s;
			exit 1
		| Parser.Error ->
			localisation (Lexing.lexeme_start_p lb);
			Format.eprintf "Syntax error.@.@?";
			exit 1
		| Ast.Syntax_error s ->
			localisation (Lexing.lexeme_start_p lb);
			Format.eprintf "Syntax error: %s@.@?" s;
			exit 1
		| Typ.Type_error s ->
			localisation (Lexing.lexeme_start_p lb); (* TODO localisation *)
			Format.eprintf "Type error: %s@.@?" s;
			exit 1
	*)
