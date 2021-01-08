open Format
open Ast

module Imap = Map.Make(String)

type texpr =
	{ te_loc : loc;
	te_e : non_loc_texpr;
	te_type : Typ.t; }

and non_loc_texpr =
	(* Constants. *)
	| TInt of int64
	| TStr of string
	| TBool of bool

	(* Expressions with parantheses. *)
	| TPar of tblock
	| TCall of ident * tblock

	(* Operations. *)
	| TNot of texpr
	| TBinop of texpr * binop * texpr

	| TLval of tlvalue
	| TAffect of tlvalue * texpr
	| TReturn of texpr option

	(* Control structures. *)
	| TFor of texpr_for
	| TWhile of texpr_while
	| TIf of texpr * tblock * telse_block

and texpr_for =
	{ for_loc : loc;
	for_expr : ident * texpr * texpr * tblock;
	for_env : Typ.t Imap.t; }

and texpr_while =
	{ while_loc : loc;
	while_expr : texpr * tblock;
	while_env : Typ.t Imap.t; }

and telse_block =
	{ else_loc : loc;
	else_b : non_loc_telse_block; }

and non_loc_telse_block =
	| TEnd
	| TElse of tblock
	| TElseif of texpr * tblock * telse_block

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

type tfunc =
	{ tf_name : ident;
	tf_loc : loc;
	tf_params : param list;
	tf_type : Typ.t;
	tf_body : tblock;
	tf_is_constructor : bool;
	tf_mutable : bool;
	tf_env : Typ.t Imap.t; }

type tdecl = TFunc of tfunc | TExpr of texpr

type tfile = tdecl list
