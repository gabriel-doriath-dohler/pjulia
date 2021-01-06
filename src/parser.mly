%{
	open Ast
%}

/* Definition of the tokens. */
(* Control structures. *)
%token Tif Telse Telseif Tend
%token Tfor Twhile
(* Functions and structures. *)
%token Tfunction Treturn
%token Tmutable Tstruct
(* String and ident. *)
%token <string> Tstring
%token <Ast.ident> Trpar_ident
%token <Ast.ident> Tident_lpar
%token <Ast.ident> Tident
(* Int and ident. *)
%token <int64> Tint
%token <int64> Tint_lpar
%token <int64 * Ast.ident> Tint_ident
(* Tests. *)
%token Teq Ttesteq Tneq Tl Tleq Tg Tgeq
(* Symbols. *)
%token Tcomma Tcolon Tdoublecolon Tdot Tsemicolon
(* Parantheses. *)
%token Tlpar Trpar
(* Booleans. *)
%token Ttrue Tfalse
%token Tand Tor
%token Tnot
(* Arithmetic. *)
%token Tadd Tsub Tmul Tmod Tpow
(* End of file. *)
%token Teof

/* Definition of the priorities and associativities of tokens. */
%nonassoc Treturn
%nonassoc Tfor Twhile Tint Tstring Ttrue Tfalse Tlpar Tident_lpar Tident Tint_lpar Tint_ident Tif
%right Teq
(* Boolean operations. *)
%left Tor
%left Tand
(* Tests. *)
%left Ttesteq Tneq Tg Tgeq Tl Tleq
(* Arithmetic. *)
%left Tadd Tsub
%left Tmul Tmod

%nonassoc Tnot
%nonassoc uminus
%right Tpow
%left Tdot
(* Error. *)
%nonassoc else_if_error

/* Grammar entry point. */
%start file

/* Return type. */
%type <Ast.file> file

%%

file:
	decls = decl* Teof	{ decls }

decl:
	| e = expr Tsemicolon	{ Expr e }
	| f = func				{ Func f }
	| s = structure			{ Structure s }

structure:
	| Tmutable Tstruct name = Tident params = param_list Tend Tsemicolon
		{ { s_mut = true; s_name = name; s_params = params } }
	| Tstruct name = Tident params = param_list Tend Tsemicolon
		{ { s_mut = false; s_name = name; s_params = params } }

func:
	| Tfunction name = Tident_lpar params = separated_list(Tcomma, param)
		Trpar Tdoublecolon typ = Tident b = block Tend Tsemicolon
  		{ { f_name = name; f_params = params; f_type = Typ.of_string typ; f_body = b } }
	| Tfunction name = Tident_lpar params = separated_list(Tcomma, param)
		Trpar b = block Tend Tsemicolon
  		{ { f_name = name; f_params = params; f_type = Typ.Any; f_body = b } }

param:
	| name = Tident
		{ { p_name = name; p_type = Typ.Any } }
	| name = Tident Tdoublecolon typ = Tident
		{ { p_name = name; p_type = Typ.of_string typ } }

param_list:
	| 										{ [] }
	| p = param								{ [p] }
	| p = param Tsemicolon bl = param_list	{ p :: bl }
	| Tsemicolon bl = param_list			{ bl }

(* Expressions wich don't start with a unitary minus. *)
expr_without_uminus:
	(* Constants. *)
	| n = Tint															{ Int n }
	| s = Tstring														{ Str s }
	| Ttrue																{ Bool true }
	| Tfalse															{ Bool false }

	(* Expressions with parentheses. *)
	(* intident *)
	| n_id = Tint_ident
		{ Binop (Int (fst n_id), Mul, Lval (Var (snd n_id))) }
	(* int(block1) *)
	| n = Tint_lpar b = block1 Trpar									{ Binop (Int n, Mul, Par b) }
	(* (block1) *)
	| Tlpar b = block1 Trpar											{ Par b }
	(* (expr)ident *)
	| Tlpar e = expr id = Trpar_ident									{ Binop (e, Mul, Lval (Var id)) }

	(* Call. *)
	| name = Tident_lpar inputs = separated_list(Tcomma, expr) Trpar
		{ if name = "println" then
			Call ("print", inputs @ [Str "\n"])
		else
			Call (name, inputs) }
	(* Operations. *)
	| Tnot e = expr												{ Not e }
	| e1 = expr_without_uminus o = op e2 = expr					{ Binop (e1, o, e2) }
	(* Lvalues. *)
	| l = lvalue												{ Lval l }
	| l = lvalue Teq e = expr									{ Affect (l, e) }
	(* Return. *)
	| Treturn e = expr											{ Return (Some e) }
	| Treturn													{ Return None }
	(* Control structures. *)
	| Tfor idx = Tident Teq e1 = expr Tcolon e2 = expr b = block_without_uminus Tend
		{ For (idx, e1, e2, b) }
	| Twhile e = expr b = block_without_uminus Tend				{ While (e, b) }
	| Tif e = expr b1 = block_without_uminus b2 = else_block	{ If (e, b1, b2) }

(* Expression starting with a unitary minus. *)
expr_with_uminus:
	(* Match the longest expression possible after while, if, elseif, lvalue and for. *)
	| Tsub e = expr %prec uminus								{ Binop (Int 0L, Sub, e) }
	| e1 = expr_with_uminus o = op e2 = expr					{ Binop (e1, o, e2) }

%inline expr:
	| e = expr_with_uminus		{ e }
	| e = expr_without_uminus	{ e }

%inline lvalue:
	| id = Tident								{ Var id }
	| e = expr_without_uminus Tdot id = Tident	{ Field (e, id) }

else_block:
	| Tend							{ End }
	| Telse b = block Tend			{ Else b }
	| Telse Tif %prec else_if_error	{ Ast.syntax_error "The token if can't follow else." }
	| Telseif e = expr b1 = block_without_uminus b2 = else_block
		{ Else [If (e, b1, b2)] }

%inline op:
	(* Tests. *)
	| Ttesteq	{ Eq }
	| Tneq		{ Neq }
	| Tl		{ L }
	| Tleq		{ Leq }
	| Tg		{ G }
	| Tgeq		{ Geq }
	(* Arithmetic. *)
	| Tadd		{ Add }
	| Tsub		{ Sub }
	| Tmul		{ Mul }
	| Tmod		{ Mod }
	| Tpow		{ Pow }
	(* Boolean operations. *)
	| Tand		{ And }
	| Tor		{ Or }

block:
	| 								{ [] }
	| e = expr						{ [e] }
	| e = expr Tsemicolon b = block	{ e :: b }
	| Tsemicolon b = block			{ b }

(* Blocks wich don't start with a unitary minus. *)
block_without_uminus:
	| 												{ [] }
	| e = expr_without_uminus						{ [e] }
	| e = expr_without_uminus Tsemicolon b = block	{ e :: b }
	| Tsemicolon b = block							{ b }

(* Non empty block. *)
%inline block1:
	| e = expr						{ [e] }
	| e = expr Tsemicolon b = block	{ e :: b }
