open Format
open Ast
open Tast

(* Map an ident name to its type. *)
module Imap = Map.Make(String)
let bindings = Imap.add "nothing" Typ.Nothing Imap.empty

(* Set of declared types. *)
let declared_types:((Typ.t, unit) Hashtbl.t) = Hashtbl.create 16
let () = List.iter
	(fun t -> Hashtbl.add declared_types t ())
	[ Typ.Any; Typ.Nothing; Typ.Int64; Typ.Bool; Typ.Str; ]

(* Map a function name to its definition. *)
let functions:((string, tfunc) Hashtbl.t) = Hashtbl.create 16
let declared_functions:((string, unit) Hashtbl.t) = Hashtbl.create 16

(* Keep the informations about the fields. *)
let h_struct_name_of_field:((string, string) Hashtbl.t) = Hashtbl.create 16
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
	Hashtbl.mem h_type_of_field field_name

let type_of_field field_name =
	Hashtbl.find h_type_of_field field_name

let struct_name_of_field field_name =
	Hashtbl.find h_struct_name_of_field field_name

let add_field field_name struct_name typ =
	Hashtbl.replace h_struct_name_of_field field_name struct_name;
	Hashtbl.replace h_type_of_field field_name typ

let assert_field_undefined l field_name =
	if is_field_defined field_name then
		Typ.type_error l (sprintf "The field %s is already used." field_name)

let assert_field_defined l field_name =
	if not (is_field_defined field_name) then
		Typ.type_error l (sprintf "The field %s is undefined." field_name)

(* For structures. *)
let add_structure f =
	let name = snd f.f_name in
	let tfunction =
		{ tf_name = f.f_name;
		tf_loc = f.f_loc;
		tf_params = f.f_params;
		tf_type = f.f_type;
		tf_body = { block_type = Typ.Nothing; block_b = []; };
		tf_is_constructor = f.f_is_constructor;
		tf_mutable = f.f_mutable;
		tf_env = Imap.empty; } in
	add_function name tfunction;
	List.iter
		(fun p -> add_field (snd p.p_name) name p.p_type)
		tfunction.tf_params

let is_structure_defined name =
	Hashtbl.mem declared_types (Typ.Struct name)

let structure_from_name name =
	Hashtbl.find functions name

let is_mutable name =
	let s = structure_from_name name in
	s.tf_mutable

let assert_mutable l field_name struct_name =
	if not (is_mutable struct_name) then
		Typ.type_error l (sprintf
			"The field %s belongs to the immutable structure %s."
			field_name struct_name)

