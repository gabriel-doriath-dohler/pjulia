open Format


type loc = { l_start : int; l_end : int; c_start : int; c_end : int }
[@@deriving show]

let current_loc lb =
	let pos_start = Lexing.lexeme_start_p lb in
	let pos_end = Lexing.lexeme_end_p lb in
	{ l_start = pos_start.pos_lnum;
	l_end = pos_end.pos_lnum;
	c_start = pos_start.pos_cnum - pos_start.pos_bol;
	c_end = pos_end.pos_cnum - pos_end.pos_bol; }

exception Syntax_error of string
let syntax_error s = raise (Syntax_error s)

type ident = loc * string
[@@deriving show]

(* Types. *)
module Typ = struct
	type t = Any | Nothing | Int64 | Bool | String | Struct of string
	[@@deriving show]

	let compare = compare

	(* Printing. *)
	let print fmt t =
		fprintf fmt "%s" (match t with
			| Any		-> "Any"
			| Nothing	-> "Nothing"
			| Int64		-> "Int64"
			| Bool		-> "Bool"
			| String	-> "String"
			| Struct s	-> sprintf "Struct %s" s)

	let rec print_list fmt = function
		| []		-> ()
		| [t]		-> print fmt t
		| t :: l	-> fprintf fmt "%a, " print t; print_list fmt l

	let of_string (_, typ) = match typ with
			| "Any"		-> Any
			| "Nothing"	-> Nothing
			| "Int64"	-> Int64
			| "Bool"	-> Bool
			| "String"	-> String
			| s			-> Struct s

	(* Error handeling. *)
	exception Type_error of loc * string
	let type_error l s = raise (Type_error (l, s))

	let compat_error l t1 t2 = type_error l (asprintf
		"This expression has type %a but an expression was expected of type %a."
		print t1 print t2)

	(* Check the compatibility of t1 and t2. *)
	let are_compatible t1 t2 =
		t1 = t2 || t1 = Any || t2 = Any

	(* Check the compatibility of t1 and t2. *)
	let assert_compatible l t1 t2 =
		if not (are_compatible t1 t2) then
			compat_error l t1 t2

	(* Returns the type t1 and t2 must be to be compatible. *)
	let common_type l t1 t2 = match t1, t2 with
		| Any, Any			-> Any
		| Any, _			-> t2
		| _, Any			-> t1
		| _ when t1 = t2	-> t1
		| _					-> compat_error l t1 t2
end

type binop = Eq | Neq | L | Leq | G | Geq | Add | Sub | Mul | Mod | Pow | And | Or
[@@deriving show]

type expr = loc * non_loc_expr

and non_loc_expr =
	(* Constants. *)
	| Int of int64
	| Str of string
	| Bool of bool

	(* Expressions with parantheses. *)
	| Par of block
	| Call of ident * block

	(* Operations. *)
	| Not of expr
	| Binop of expr * binop * expr

	| Lval of lvalue
	| Affect of lvalue * expr
	| Return of expr option

	(* Control structures. *)
	| For of ident * expr * expr * block
	| While of expr * block
	| If of expr * block * else_block
[@@deriving show]

and else_block = loc * non_loc_else_block
[@@deriving show]

and non_loc_else_block =
	| End
	| Else of block
	| Elseif of expr * block * else_block
[@@deriving show]

and block = expr list
[@@deriving show]

and lvalue = loc * non_loc_lvalue
[@@deriving show]

and non_loc_lvalue =
	| Var of ident
	| Field of expr * ident
[@@deriving show]

type param =
	{ p_name : ident;
	p_type : Typ.t; }
[@@deriving show]

type func =
	{ f_name : ident;
	f_loc : loc;
	f_params : param list;
	f_type : Typ.t;
	f_body : block;
	f_is_constructor : bool;
	f_mutable : bool; }
[@@deriving show]

type decl = Func of func | Expr of expr
[@@deriving show]

type file = decl list
[@@deriving show]

