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
let add_variable name typ env =
	Imap.add name typ env

let type_of name env =
	Imap.find name env

let is_variable_defined name env =
	Imap.mem name env

(* For types. *)
let declare_type typ =
	Hashtbl.replace declared_types typ ()

let is_type_defined =
	Hashtbl.mem declared_types

(* For functions. *)
let declare_function name =
	Hashtbl.replace declared_functions name ()

let add_function name tfunction =
	Hashtbl.add functions name tfunction

let is_function_defined name =
	Hashtbl.mem functions name

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
	Hashtbl.replace h_struct_name_of_field field_name struct_name;
	Hashtbl.replace h_is_field_mutable field_name mut;
	Hashtbl.replace h_type_of_field field_name typ

(* For structures. *)
let add_structure f =
	let mut = f.f_mutable in
	let name = snd f.f_name in
	let tfunction =
		{ tf_name = f.f_name;
		tf_loc = f.f_loc;
		tf_params = f.f_params;
		tf_type = f.f_type;
		tf_body = [];
		tf_is_constructor = f.f_is_constructor;
		tf_mutable = mut;
		tf_env = Imap.empty; } in
	add_function name tfunction;
	List.iter
		(fun p -> add_field (snd p.p_name) name mut p.p_type)
		tfunction.tf_params

let is_structure_defined name =
	Hashtbl.mem declared_types (Typ.Struct name)
