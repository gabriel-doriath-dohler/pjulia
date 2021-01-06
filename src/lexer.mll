{
	open Lexing
	open Parser
	open Format

	exception Lexing_error of string
	let lexing_error s = raise (Lexing_error s)

	let kwd_assoc = [
		"if",		Tif;
		"else",		Telse;
		"elseif",	Telseif;
		"end",		Tend;
		"true",		Ttrue;
		"false",	Tfalse;
		"function",	Tfunction;
		"return",	Treturn;
		"mutable",	Tmutable;
		"struct",	Tstruct;
		"for",		Tfor;
		"while",	Twhile; ]

	(* Lex ident or keyword. *)
	let ident_or_kwd s =
		(* Closure. *)
		let h = Hashtbl.create 16 in
		List.iter (fun (s, t) -> Hashtbl.add h s t) kwd_assoc;
		try Hashtbl.find h s
		with Not_found -> Tident s

	(* Decides if a semicolon should be added after the token. *)
	let add_semicolon_after_token = function
		| Tident _ | Tint _ | Tint_ident _ | Trpar_ident _ | Tstring _
		| Ttrue | Tfalse | Treturn | Trpar | Tend -> true
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
		| Tident s			-> printf "Tident %s@.@?" s
		| Tint i			-> printf "Tint %Ld@.@?" i
		| Tstring s			-> printf "Tstring %s@.@?" s
		| Ttrue				-> printf "True@.@?"
		| Tfalse			-> printf "Tfalse@.@?"
		| Treturn			-> printf "Treturn@.@?"
		| Trpar				-> printf "Trpar@.@?"
		| Tend				-> printf "Tend@.@?"
		| Tif				-> printf "Tif@.@?"
		| Telse				-> printf "Telse@.@?"
		| Telseif			-> printf "Telseif@.@?"
		| Tfunction			-> printf "Tfunction@.@?"
		| Tmutable			-> printf "Tmutable@.@?"
		| Tstruct			-> printf "Tstruct@.@?"
		| Tfor				-> printf "Tfor@.@?"
		| Twhile			-> printf "Twhile@.@?"
		| Teof				-> printf "Teof@.@?"
		| Tcomma			-> printf "Tcomma@.@?"
		| Tcolon			-> printf "Tcolon@.@?"
		| Tdoublecolon		-> printf "Tdoublecolon@.@?"
		| Tdot				-> printf "Tdot@.@?"
		| Tsemicolon		-> printf "Tsemicolon@.@?"
		| Tlpar				-> printf "Tlpar@.@?"
		| Tnot				-> printf "Tnot@.@?"
		| Teq				-> printf "Teq@.@?"
		| Ttesteq			-> printf "Ttesteq@.@?"
		| Tneq				-> printf "Tneq@.@?"
		| Tl				-> printf "Tl@.@?"
		| Tleq				-> printf "Tleq@.@?"
		| Tg				-> printf "Tg@.@?"
		| Tgeq				-> printf "Tgeq@.@?"
		| Tadd				-> printf "Tadd@.@?"
		| Tsub				-> printf "Tsub@.@?"
		| Tmul				-> printf "Tmul@.@?"
		| Tmod				-> printf "Tmod@.@?"
		| Tpow				-> printf "Tpow@.@?"
		| Tand				-> printf "Tand@.@?"
		| Tor				-> printf "Tor@.@?"
		| Tint_ident (i, s)	-> printf "Tint_ident %Ld %s@.@?" i s
		| Tident_lpar s		-> printf "Tident_lpar %s@.@?" s
		| Tint_lpar i		-> printf "Tint_lpar %Ld@.@?" i
		| Trpar_ident s		-> printf "Trpar_ident %s@.@?" s

	(* true if we should insert a semicolon if we see a newline. *)
	let semicolon = ref false
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit)*
let integer = digit +
let character = ([' '-'~']#['\\' '"']) | "\\\\" | "\\\"" | "\\n" |"\\t"

let comment = "#" [^'\n']*
let space = ' ' | '\t'

rule token = parse
	| eof						{ Teof }
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
	| '!'						{ Tnot }
	| '='						{ Teq }
	| "=="						{ Ttesteq }
	| "!="						{ Tneq }
	| '<'						{ Tl }
	| "<="						{ Tleq }
	| '>'						{ Tg }
	| ">="						{ Tgeq }
	| '+'						{ Tadd }
	| '-'						{ Tsub }
	| '*'						{ Tmul }
	| '%'						{ Tmod }
	| '^'						{ Tpow }
	| "&&"						{ Tand }
	| "||"						{ Tor }
	| (integer as i) (ident as s)	{ Tint_ident (int64_of_string i, s) }
	| ident as s '('			{ Tident_lpar s }
	| integer as i '('			{ Tint_lpar (int64_of_string i) }
	| ')' (ident as s)			{ Trpar_ident s }
	| integer as i				{ Tint (int64_of_string i) }
	| '"' (character* as s) '"'	{ Tstring s }
	| ident as s				{ ident_or_kwd s }
	| _ as c					{ lexing_error ("Illegal character: " ^ String.make 1 c) }


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
