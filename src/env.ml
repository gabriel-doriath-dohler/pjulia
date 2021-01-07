open Ast
open Tast

(* Map an ident name to its type. *)
module Imap = Map.Make(String)
let bindings = Imap.add "nothing" Typ.Nothing Imap.empty

(* Set of declared types. *)
let declared_types:((Typ.t, unit) Hashtbl.t) = Hashtbl.create 16
let () = List.iter
	(fun t -> Hashtbl.add declared_types t ())
	[ Typ.Any; Typ.Nothing; Typ.Int64; Typ.Bool; Typ.String; ]

(* Map a function name to its definition. *)
let functions:((string, tfunc) Hashtbl.t) = Hashtbl.create 16
let declared_functions:((string, unit) Hashtbl.t) = Hashtbl.create 16

(* Keep the informations about the fields. *)
let h_struct_name_of_field:((string, string) Hashtbl.t) = Hashtbl.create 16
let h_is_field_mutable:((string, bool) Hashtbl.t) = Hashtbl.create 16
let h_type_of_field:((string, Typ.t) Hashtbl.t) = Hashtbl.create 16

(* Helper functions. *)

(* For variables. *)
let add_var name typ env =
	Imap.add name typ env

let type_of name env =
	Imap.find name env

let is_var_defined name env =
	Imap.mem name env

(* For types. *)
let declare_type typ_name =
	Hashtbl.replace declared_types typ_name ()

let is_type_defined =
	Hashtbl.mem declared_types

(* For functions. *)
let declare_function name =
	Hashtbl.replace declared_functions name ()

let add_function name tfunction =
	Hashtbl.add functions name tfunction

(* For fields. *)
let is_field_defined field_name =
	Hashtbl.mem h_is_field_mutable field_name

let is_field_mutable field_name =
	Hashtbl.find h_is_field_mutable field_name

let type_of_field field_name =
	Hashtbl.find h_type_of_field field_name

let struct_name_of_field field_name =
	Hashtbl.find h_struct_name_of_field field_name

let add_field field_name struct_name mut typ =
	Hashtbl.add h_struct_name_of_field field_name struct_name;
	Hashtbl.add h_is_field_mutable field_name mut;
	Hashtbl.add h_type_of_field field_name typ

(* For structures. *)
let add_struct name tfunction =
	add_function name tfunction;
	let mut = tfunction.tf_mutable in
	List.iter
		(fun p -> add_field (snd p.p_name) name mut p.p_type)
		tfunction.tf_params
