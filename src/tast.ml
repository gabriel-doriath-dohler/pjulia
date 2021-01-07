open Format
open Ast

module Imap = Map.Make(String)

type texpr = loc * non_loc_texpr

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

and texpr_for = loc * non_loc_texpr_for

and non_loc_texpr_for =
	{ for_expr : ident * texpr * texpr * tblock;
	for_env : Typ.t Imap.t; }

and texpr_while = loc * non_loc_texpr_while

and non_loc_texpr_while =
	{ while_expr : texpr * tblock;
	while_env : Typ.t Imap.t; }

and telse_block = loc * non_loc_telse_block

and non_loc_telse_block =
	| TEnd
	| TElse of tblock
	| TElseif of texpr * tblock * telse_block

and tblock = texpr list

and tlvalue = loc * non_loc_tlvalue

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
