open Format


exception Syntax_error of string
let syntax_error s = raise (Syntax_error s)

type ident = string
[@@deriving show]

(* Types. *)
module Typ = struct
	type t = Any | Nothing | Int64 | Bool | String | Struct of ident
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

	let of_string = function
		| "Any"		-> Any
		| "Nothing"	-> Nothing
		| "Int64"	-> Int64
		| "Bool"	-> Bool
		| "String"	-> String
		| s			-> Struct s

	(* Error handeling. *)
	exception Type_error of string
	let type_error s = raise (Type_error s)

	let compat_error t1 t2 = type_error (asprintf
		"This expression has type %a but an expression was expected of type %a."
		print t1 print t2)

	(* Check the compatibility of t1 and t2. *)
	let are_compatible t1 t2 =
		t1 = t2 || t1 = Any || t2 = Any

	(* Check the compatibility of t1 and t2. *)
	let assert_compatible t1 t2 =
		if not (are_compatible t1 t2) then
			compat_error t1 t2

	(* Returns the type t1 and t2 must be to be compatible. *)
	let common_type t1 t2 = match t1, t2 with
		| Any, Any			-> Any
		| Any, _			-> t2
		| _, Any			-> t1
		| _ when t1 = t2	-> t1
		| _					-> compat_error t1 t2
end

type binop = Eq | Neq | L | Leq | G | Geq | Add | Sub | Mul | Mod | Pow | And | Or
[@@deriving show]

type expr =
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

and else_block =
	| End
	| Else of block
	| Elseif of expr * block * else_block
[@@deriving show]

and block = expr list
[@@deriving show]

and lvalue =
	| Var of ident
	| Field of expr * ident
[@@deriving show]

type param =
	{ p_name : ident;
	p_type : Typ.t; }
[@@deriving show]

type func =
	{ f_name : ident;
	f_params : param list;
	f_type : Typ.t;
	f_body : block; }
[@@deriving show]

type structure =
	{ s_mut : bool;
	s_name : ident;
	s_params : param list; }
[@@deriving show]

type decl = Structure of structure | Func of func | Expr of expr
[@@deriving show]

type file = decl list
[@@deriving show]

