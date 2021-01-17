open Format
open Ast

module Imap = Map.Make(String)
type env = { g : Typ.t Imap.t; l : Typ.t Imap.t; ofs : int Imap.t; next_ofs_arg : int; next_ofs_var : int; }

type texpr =
	{ te_loc : loc;
	te_e : non_loc_texpr;
	te_type : Typ.t; }

and non_loc_texpr =
	(* Constants. *)
	| TInt of int64
	| TStr of string
	| TBool of bool

	(* Expressions with parentheses. *)
	| TPar of tblock
	| TCall of ident * (texpr list) * ((Typ.t list * Typ.t) list)

	(* Operations. *)
	| TNot of texpr
	| TBinop of texpr * binop * texpr

	| TLval of tlvalue
	| TAffect of tlvalue * texpr
	| TReturn of texpr option

	(* Control structures. *)
	| TFor of texpr_for
	| TWhile of texpr_while
	| TIf of texpr * tblock * tblock

and texpr_for =
	{ for_loc : loc;
	for_expr : ident * texpr * texpr * tblock;
	for_env : env; }

and texpr_while =
	{ while_loc : loc;
	while_expr : texpr * tblock;
	while_env : env; }

and tblock =
	{ block_type : Typ.t;
	block_b : texpr list; }

and tlvalue =
	{ lvalue_loc : loc;
	lvalue_type : Typ.t;
	lvalue_lvalue : non_loc_tlvalue; }

and non_loc_tlvalue =
	| TVar of ident
	| TField of texpr * ident

and tfunc =
	{ tf_name : ident;
	tf_loc : loc;
	tf_params : param list;
	tf_type : Typ.t;
	tf_body : tblock;
	tf_is_constructor : bool;
	tf_mutable : bool;
	tf_env : env;
	mutable tf_fpmax : int; }

type tdecl = TFunc of tfunc | TExpr of texpr

type tfile = tdecl list
