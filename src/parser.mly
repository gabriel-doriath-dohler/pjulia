%{
	open Ast

	let empty_loc = { l_start = -1; l_end = -1; c_start = -1; c_end = -1; }
%}

/* Definition of the tokens. */
(* Control structures. *)
%token <Ast.loc> Tif
%token <Ast.loc> Telse
%token <Ast.loc> Telseif
%token Tend
%token <Ast.loc> Tfor
%token <Ast.loc> Twhile
(* Functions and structures. *)
%token <Ast.loc> Tfunction
%token <Ast.loc> Treturn
%token <Ast.loc> Tmutable
%token <Ast.loc> Tstruct
(* String and ident. *)
%token <Ast.loc * string> Tstring
%token <Ast.ident> Trpar_ident
%token <Ast.ident> Tident_lpar
%token <Ast.ident> Tident
(* Int and ident. *)
%token <Ast.loc * int64> Tint
%token <Ast.loc * int64> Tint_lpar
%token <Ast.loc * int64 * Ast.ident> Tint_ident
(* Tests. *)
%token Teq Ttesteq Tneq Tl Tleq Tg Tgeq
(* Symbols. *)
%token Tcomma Tsemicolon Tcolon Tdoublecolon Tdot
(* Parantheses. *)
%token Tlpar Trpar
(* Booleans. *)
%token <Ast.loc> Ttrue
%token <Ast.loc> Tfalse
%token Tand Tor
%token <Ast.loc> Tnot
(* Arithmetic. *)
%token <Ast.loc> Tsub
%token Tadd Tmul Tmod Tpow
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
	decls=decl* Teof	{ decls }

decl:
	| e=expr Tsemicolon	{ Expr e }
	| f=func			{ Func f }
	| s=structure		{ Structure s }

structure:
	| l=Tmutable z=Tstruct name=Tident params=param_list Tend Tsemicolon
		{ { s_loc=l; s_mut=true; s_name=name; s_params=params } }
	| l=Tstruct name=Tident params=param_list Tend Tsemicolon
		{ { s_loc=l; s_mut=false; s_name=name; s_params=params } }

func:
	| l=Tfunction name=Tident_lpar params=separated_list(Tcomma, param)
		Trpar Tdoublecolon typ=Tident b=block Tend Tsemicolon
  		{ { f_loc=l; f_name=name; f_params=params; f_type=(fst typ, Typ.of_string typ); f_body=b } }
	| l=Tfunction name=Tident_lpar params=separated_list(Tcomma, param)
		Trpar b=block Tend Tsemicolon
  		{ { f_loc=l; f_name=name; f_params=params; f_type=(empty_loc, Typ.Any); f_body=b } }

param:
	| name=Tident
		{ { p_name=name; p_type=(empty_loc, Typ.Any) } }
	| name=Tident Tdoublecolon typ=Tident
		{ { p_name=name; p_type=(fst typ, Typ.of_string typ) } }

param_list:
	| 									{ [] }
	| p=param							{ [p] }
	| p=param Tsemicolon bl=param_list	{ p :: bl }
	| Tsemicolon bl=param_list			{ bl }

(* Expressions wich don't start with a unitary minus. *)
expr_without_uminus:
	(* Constants. *)
	| n=Tint	{ fst n, Int (snd n) }
	| s=Tstring	{ fst s, Str (snd s) }
	| l=Ttrue	{ l, Bool true }
	| l=Tfalse	{ l, Bool false }

	(* Expressions with parentheses. *)
	(* intident *)
	| n_id=Tint_ident
		{ match n_id with
			| l, n, id -> l, Binop ((l, Int n), Mul, (l, Lval (l, Var id))) }
	(* int(block1) *)
	| n=Tint_lpar b=block1 Trpar
		{ fst n, Binop ((fst n, Int (snd n)), Mul, (fst n, Par b)) }
	(* (block1) *)
	| Tlpar b=block1 Trpar
		{ fst (List.hd b), Par b }
	(* (expr)ident *)
	| Tlpar e=expr id=Trpar_ident
		{ fst e, Binop (e, Mul, (fst id, Lval (fst id, Var id))) }

	(* Call. *)
	| name=Tident_lpar inputs=separated_list(Tcomma, expr) Trpar
		{ if snd name = "println" then
			fst name, Call ((fst name, "print"), inputs @ [empty_loc, Str "\n"])
		else
			fst name, Call (name, inputs) }
	(* Operations. *)
	| l=Tnot e=expr											{ l, Not e }
	| e1=expr_without_uminus o=op e2=expr					{ fst e1, Binop (e1, o, e2) }
	(* Lvalues. *)
	| l=lvalue												{ fst l, Lval l }
	| l=lvalue Teq e=expr									{ fst l, Affect (l, e) }
	(* Return. *)
	| l=Treturn e = expr									{ l, Return (Some e) }
	| l=Treturn												{ l, Return None }
	(* Control structures. *)
	| l=Tfor idx=Tident Teq e1=expr Tcolon e2=expr b=block_without_uminus Tend
		{ l, For (idx, e1, e2, b) }
	| l=Twhile e=expr b=block_without_uminus Tend			{ l, While (e, b) }
	| l=Tif e=expr b1=block_without_uminus b2=else_block	{ l, If (e, b1, b2) }

(* Expression starting with a unitary minus. *)
expr_with_uminus:
	(* Match the longest expression possible after while, if, elseif, lvalue and for. *)
	| l=Tsub e = expr %prec uminus							{ l, Binop ((l, Int 0L), Sub, e) }
	| e1=expr_with_uminus o=op e2=expr						{ fst e1, Binop (e1, o, e2) }

%inline expr:
	| e=expr_with_uminus	{ e }
	| e=expr_without_uminus	{ e }

%inline lvalue:
	| id=Tident								{ fst id, Var id }
	| e=expr_without_uminus Tdot id=Tident	{ fst e, Field (e, id) }

else_block:
	| Tend									{ empty_loc, End }
	| l=Telse b=block Tend					{ l, Else b }
	| z=Telse zz=Tif %prec else_if_error	{ Ast.syntax_error "The token if can't follow else." }
	| l=Telseif e=expr b1=block_without_uminus b2=else_block
		{ l, Else [l, If (e, b1, b2)] }

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
	| z=Tsub	{ Sub }
	| Tmul		{ Mul }
	| Tmod		{ Mod }
	| Tpow		{ Pow }
	(* Boolean operations. *)
	| Tand		{ And }
	| Tor		{ Or }

block:
	| 							{ [] }
	| e=expr					{ [e] }
	| e=expr Tsemicolon b=block	{ e :: b }
	| Tsemicolon b=block		{ b }

(* Blocks wich don't start with a unitary minus. *)
block_without_uminus:
	| 											{ [] }
	| e=expr_without_uminus						{ [e] }
	| e=expr_without_uminus Tsemicolon b=block	{ e :: b }
	| Tsemicolon b=block						{ b }

(* Non empty block. *)
%inline block1:
	| e=expr					{ [e] }
	| e=expr Tsemicolon b=block	{ e :: b }
