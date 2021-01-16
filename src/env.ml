open Format
open Ast
open Tast

(* Map an ident name to its type. *)
let genv = Imap.add "nothing" Typ.Nothing Imap.empty
let lenv = Imap.empty

let empty_env = { g = genv; l = lenv }

(* Set of declared types. *)
let declared_types:((Typ.t, unit) Hashtbl.t) = Hashtbl.create 16
let () = List.iter
	(fun t -> Hashtbl.add declared_types t ())
	[ Typ.Any; Typ.Nothing; Typ.Int64; Typ.Bool; Typ.Str; ]

(* Map a function name to its definition. *)
let tfunctions:((string, tfunc) Hashtbl.t) = Hashtbl.create 16
let func:((string, (Typ.t list * Typ.t * bool)) Hashtbl.t) = Hashtbl.create 16

(* Keep the informations about the fields. *)
let h_struct_name_of_field:((string, string) Hashtbl.t) = Hashtbl.create 16
let h_type_of_field:((string, Typ.t) Hashtbl.t) = Hashtbl.create 16

(* Helper functions. *)

(* For variables. *)
let add_local_variable name typ env =
	{ env with l = Imap.add name typ env.l }

let add_global_variable name typ env =
	{ env with g = Imap.add name typ env.g }

let is_local name env =
	Imap.mem name env.l

let is_global name env =
	not (is_local name env) && Imap.mem name env.g

let type_of name env =
	try Imap.find name env.l
	with Not_found -> Imap.find name env.g

let local_type_of name env =
	Imap.find name env.l

let is_variable_defined name env =
	Imap.mem name env.l || Imap.mem name env.g

let assert_variable_defined l name env =
	if not (is_variable_defined name env) then
		Typ.type_error l (sprintf "The variable %s isn't defined." name)

(* For types. *)
let declare_type typ =
	Hashtbl.replace declared_types typ ()

let is_type_defined =
	Hashtbl.mem declared_types

(* For functions. *)
let add_function name arguments return_type mut =
	let arguments_type_list = List.map (fun p -> p.p_type) arguments in
	Hashtbl.add func name (arguments_type_list, return_type, mut)

let add_tfunction tfunction =
	Hashtbl.add tfunctions (snd tfunction.tf_name) tfunction

let is_function_defined name =
	Hashtbl.mem func name

let is_function_compatible t_list (arguments_type, return_type) =
	List.for_all2 Typ.are_compatible arguments_type t_list

let compatible_functions name t_list =
	let f_list = List.map
		(fun (arguments_type, return_type, _) -> (arguments_type, return_type))
		(Hashtbl.find_all func name) in
	List.filter (is_function_compatible t_list) f_list

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
let add_structure name arguments mut =
	declare_type (Typ.Struct name);
	add_function name arguments (Typ.Struct name) mut;
	List.iter
		(fun p -> add_field (snd p.p_name) name p.p_type)
		arguments

let is_structure_defined name =
	Hashtbl.mem declared_types (Typ.Struct name)

let structure_from_name name =
	Hashtbl.find func name

let is_mutable name = match structure_from_name name with
	| _, _, mut -> mut

let assert_mutable l field_name struct_name =
	if not (is_mutable struct_name) then
		Typ.type_error l (sprintf
			"The field %s belongs to the immutable structure %s."
			field_name struct_name)

