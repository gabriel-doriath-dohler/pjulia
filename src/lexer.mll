{
	open Lexing
	open Parser
	open Format

	let string_buf = Buffer.create 2048

	exception Lexing_error of string
	let lexing_error s = raise (Lexing_error s)

	let cl = Ast.current_loc

	(* Decides if a semicolon should be added after the token. *)
	let add_semicolon_after_token = function
		| Tident _ | Tint _ | Tint_ident _ | Trpar_ident _ | Tstring _
		| Ttrue _ | Tfalse _ | Treturn _ | Trpar | Tend -> true
		| _ -> false

	(* Convert string to int64. *)
	let int64_of_string s =
		let pjulia_max_int = 9223372036854775808L in
		(* 2 ^ 63 will be represented as -2^63 if we consider it as a signed integer. *)
		try
			let n = Int64.of_string (sprintf "0u%s" s) in
			if Int64.unsigned_compare n pjulia_max_int <= 0 then
				n
			else
				lexing_error (sprintf "Constant to large: %s" s)
		with _ -> lexing_error (sprintf "Constant to large: %s" s)

	let print_token = function
		| Tident (_, s)				-> printf "Tident %s@.@?" s
		| Tint (_, i)				-> printf "Tint %Ld@.@?" i
		| Tstring (_, s)			-> printf "Tstring %s@.@?" s
		| Ttrue _					-> printf "True@.@?"
		| Tfalse _					-> printf "Tfalse@.@?"
		| Treturn _					-> printf "Treturn@.@?"
		| Trpar						-> printf "Trpar@.@?"
		| Tend						-> printf "Tend@.@?"
		| Tif _						-> printf "Tif@.@?"
		| Telse _					-> printf "Telse@.@?"
		| Telseif _					-> printf "Telseif@.@?"
		| Tfunction _				-> printf "Tfunction@.@?"
		| Tmutable _				-> printf "Tmutable@.@?"
		| Tstruct _					-> printf "Tstruct@.@?"
		| Tfor _					-> printf "Tfor@.@?"
		| Twhile _					-> printf "Twhile@.@?"
		| Teof						-> printf "Teof@.@?"
		| Tcomma					-> printf "Tcomma@.@?"
		| Tcolon					-> printf "Tcolon@.@?"
		| Tdoublecolon				-> printf "Tdoublecolon@.@?"
		| Tdot						-> printf "Tdot@.@?"
		| Tsemicolon				-> printf "Tsemicolon@.@?"
		| Tlpar						-> printf "Tlpar@.@?"
		| Tnot _					-> printf "Tnot@.@?"
		| Teq						-> printf "Teq@.@?"
		| Ttesteq					-> printf "Ttesteq@.@?"
		| Tneq						-> printf "Tneq@.@?"
		| Tl						-> printf "Tl@.@?"
		| Tleq						-> printf "Tleq@.@?"
		| Tg						-> printf "Tg@.@?"
		| Tgeq						-> printf "Tgeq@.@?"
		| Tadd						-> printf "Tadd@.@?"
		| Tsub _					-> printf "Tsub@.@?"
		| Tmul						-> printf "Tmul@.@?"
		| Tmod						-> printf "Tmod@.@?"
		| Tpow						-> printf "Tpow@.@?"
		| Tand						-> printf "Tand@.@?"
		| Tor						-> printf "Tor@.@?"
		| Tint_ident (_, i, (_, s))	-> printf "Tint_ident %Ld %s@.@?" i s
		| Tident_lpar (_, s)		-> printf "Tident_lpar %s@.@?" s
		| Tint_lpar (_, i)			-> printf "Tint_lpar %Ld@.@?" i
		| Trpar_ident (_, s)		-> printf "Trpar_ident %s@.@?" s

	(* true if we should insert a semicolon if we see a newline. *)
	let semicolon = ref false
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit)*
let integer = digit +
let character = [' '-'~']#['\\' '"']

let comment = "#" [^'\n']*
let space = ' ' | '\t'

rule token = parse
	| space +
	| comment					{ token lexbuf }
	| '\n'						{ new_line lexbuf;
								if !semicolon then Tsemicolon else token lexbuf }
	| ','						{ Tcomma }
	| ':'						{ Tcolon }
	| "::"						{ Tdoublecolon }
	| '.'						{ Tdot }
	| ';'						{ Tsemicolon }
	| '('						{ Tlpar }
	| ')'						{ Trpar }
	| '!'						{ Tnot (cl lexbuf)}
	| '='						{ Teq }
	| "=="						{ Ttesteq }
	| "!="						{ Tneq }
	| '<'						{ Tl }
	| "<="						{ Tleq }
	| '>'						{ Tg }
	| ">="						{ Tgeq }
	| '+'						{ Tadd }
	| '-'						{ Tsub (cl lexbuf) }
	| '*'						{ Tmul }
	| '%'						{ Tmod }
	| '^'						{ Tpow }
	| "&&"						{ Tand }
	| "||"						{ Tor }
	(* We can't simply use a hash table because we need the localisation.
	(But there is a work around.) *)
	| "if"						{ Tif (cl lexbuf) }
	| "else"					{ Telse (cl lexbuf) }
	| "elseif"					{ Telseif (cl lexbuf) }
	| "end"						{ Tend }
	| "true"					{ Ttrue (cl lexbuf) }
	| "false"					{ Tfalse (cl lexbuf) }
	| "function"				{ Tfunction (cl lexbuf) }
	| "return"					{ Treturn (cl lexbuf) }
	| "mutable"					{ Tmutable (cl lexbuf) }
	| "struct"					{ Tstruct (cl lexbuf) }
	| "for"						{ Tfor (cl lexbuf) }
	| "while"					{ Twhile (cl lexbuf) }
	| (integer as i) (ident as s)
		{ let l = cl lexbuf in
		let n = int64_of_string i in
		Tint_ident (l, n, (l, s)) }
	| ident as s '('			{ Tident_lpar (cl lexbuf, s) }
	| integer as i '('			{ Tint_lpar (cl lexbuf, int64_of_string i) }
	| ')' (ident as s)			{ Trpar_ident (cl lexbuf, s) }
	| integer as i				{ Tint (cl lexbuf, int64_of_string i) }
	| '"' [^ '"']* eof			{ lexing_error "Unterminated string." }
	| '"'						{ Tstring (cl lexbuf, lex_string lexbuf) }
	| ident as s				{ Tident (cl lexbuf, s) }
	| _ as c					{ lexing_error ("Illegal character: " ^ String.make 1 c) }
	| eof						{ Teof }

and lex_string = parse
	| '"'				{ let s = Buffer.contents string_buf in Buffer.reset string_buf; s }
	| "\\n"				{ Buffer.add_string string_buf "\n"; lex_string lexbuf }
	| "\\t"				{ Buffer.add_string string_buf "\t"; lex_string lexbuf }
	| "\\\""			{ Buffer.add_string string_buf "\""; lex_string lexbuf }
	| "\\\\"			{ Buffer.add_string string_buf "\\"; lex_string lexbuf }
	| character as c	{ Buffer.add_char string_buf c; lex_string lexbuf }

{
	(* Add semicolons automatically. *)
	let next_token debug =
		(* Closure. *)
		fun lb ->
			(* Compute the next token. *)
			let t = token lb in

			(* Set the semicolon flag. *)
			semicolon := add_semicolon_after_token t;

			if debug then
				print_token t;
			(* Return next token. *)
			t
}
