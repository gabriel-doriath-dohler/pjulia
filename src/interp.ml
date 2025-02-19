open Ast
open Format
open Int64

module Imap = Map.Make(String)

exception Interp_error of string
let error s = raise (Interp_error s)

type value =
  | Vnothing
  | Vbool of int64 (* booleans are considered as integers (true=1 and false=0)*)
  | Vint of int64
  | Vstring of string
  | Vstruc of (string * value) list

exception Ereturn of value
let break v = raise (Ereturn v)

let boolint b = if b then Vbool one else Vbool zero

let rec print_value = function
  | Vnothing -> ()
  | Vbool n when n = one -> printf "true"
  | Vbool n when n = zero -> printf "false"
  | Vbool _ -> error "Unexpected boolean error"
  | Vint n -> printf "%Ld" n
  | Vstring s -> printf "%s" s
  | Vstruc l ->
      let rec print_struc = function
        | [] -> ()
        | [(w,v)] -> printf "%s : " w; print_value v
        | (w,v)::q -> printf "%s : " w; print_value v; printf "; "
      in printf "{"; print_struc l; printf "}"

let rec power x = function
  | 0L -> one
  | a when a < zero -> error "Negative power"
  | a -> mul x (power x (sub a one))

let rec assoc_replace x v = function
  | [] -> raise Not_found
  | (t,_)::l when x=t -> (x,v)::l
  | (t,w)::l -> (t,w)::(assoc_replace x v l)

let interp_binop op v1 v2 = match op, v1, v2 with
  | Add, (Vint n1|Vbool n1), (Vint n2|Vbool n2) -> Vint (add n1 n2)
  | Sub, (Vint n1|Vbool n1), (Vint n2|Vbool n2) -> Vint (sub n1 n2)
  | Mul, (Vint n1|Vbool n1), (Vint n2|Vbool n2) -> Vint (mul n1 n2)
  | Mod, (Vint _|Vbool _), (Vint 0L|Vbool 0L) -> error "Division by zero"
  | Mod, (Vint n1|Vbool n1), (Vint n2|Vbool n2) -> Vint (sub n1 (mul n2 (div n1 n2)))
  | Pow, (Vint n1|Vbool n1), (Vint n2|Vbool n2) -> Vint (power n1 n2)
  | Eq, Vint n, Vbool b | Eq, Vbool b, Vint n -> boolint (n=b)
  | Eq, _, _ -> boolint (v1 = v2)
  | Neq, Vint n, Vbool b | Neq, Vbool b, Vint n -> boolint (n<>b)
  | Neq, _, _ -> boolint (v1 <> v2)
  | L, (Vint n1|Vbool n1), (Vint n2|Vbool n2) -> boolint (n1 < n2)
  | Leq, (Vint n1|Vbool n1), (Vint n2|Vbool n2) -> boolint (n1 <= n2)
  | G, (Vint n1|Vbool n1), (Vint n2|Vbool n2) -> boolint (n1 > n2)
  | Geq, (Vint n1|Vbool n1), (Vint n2|Vbool n2) -> boolint (n1 >= n2)
  | _ -> error "Unsupported operand types"

let is_true = function
  | (Vint 1L|Vbool 1L) -> true
  | (Vint 0L|Vbool 0L) -> false
  | _ -> error "Not a boolean"



let file l =
  (*function maps a function name to the function*)
  (*structures maps a structure name to the structure*)
  (*fields maps a field name to the corresponding structure*)
  let functions0 = ref Imap.empty and structures0 = ref Imap.empty and fields0 = ref Imap.empty in

  (*vars maps a variable name to its value*)
  let vars = ref Imap.empty in
  vars := Imap.add "nothing" Vnothing !vars;

  let read = function
    | Func s when s.f_is_constructor -> structures0 := Imap.add (snd s.f_name) s !structures0;
        fields0 := List.fold_left (fun flds param -> Imap.add (snd param.p_name) s flds) !fields0 s.f_params
    | Func f -> functions0 := Imap.add (snd f.f_name) f !functions0
    | Expr _ -> ()
  in List.iter read l;
  let functions = !functions0 and structures = !structures0 and fields = !fields0 in



let rec interp_expr expr = let l , e = expr in match e with
  | Int n -> Vint n
  | Str s -> Vstring s
  | Bool b -> boolint b

  | Par b -> interp_block b

  | Call((_,"div"),[e1;e2]) -> (match interp_expr e1, interp_expr e2 with
      | (Vint n1|Vbool n1), (Vint n2|Vbool n2) -> (try Vint (div n1 n2) with Division_by_zero -> error "Division by zero")
      | _ -> error "Integers were expected")
  | Call((_,"div"),_) -> error "Unexpected number of arguments"

  | Call((_,"print"),b) -> List.iter (fun e -> print_value (interp_expr e)) b; Vnothing

  | Call((_,id),b) -> if Imap.mem id functions
      then let f = Imap.find id functions in interp_fun f b
      else let s = (try Imap.find id structures with Not_found -> error "Unbound function/structure") in interp_struc s b

  | Not e -> boolint (not (is_true (interp_expr e)))
  | Binop(e1,And,e2) -> let v1 = interp_expr e1 in
      if is_true v1 then interp_expr e2 else Vbool zero
  | Binop(e1,Or,e2) -> let v1 = interp_expr e1 in
      if not (is_true v1) then interp_expr e2 else Vbool one
  | Binop(e1,op,e2) -> interp_binop op (interp_expr e1) (interp_expr e2)

  | Lval (_,Var (_,x)) -> (try Imap.find x !vars with Not_found -> error "Unbound variable")
  | Lval (_,Field(e,(_,x))) ->
      (match interp_expr e with
      | Vstruc l -> (try List.assoc x l with Not_found -> error "Field incompatible with structure")
      | _ -> error "This expression is not a structure")

  | Affect((_,Var (_,x)),e) -> let v = interp_expr e in vars := Imap.add x v !vars; v
  | Affect((_,Field(e1,(_,fld))),e2) -> if not (Imap.mem fld fields) then error "Unbound field";
      if not ((Imap.find fld fields).f_mutable) then error "Unmutable structure";
      let w = interp_expr e2 in
      (try
        let x, v0 = var_of_expr e1 in if v0 != (Imap.find x !vars) then error "Unexpected behaviour on affecting field" else let v = (match v0 with
          | Vstruc l0 -> let l = try assoc_replace fld w l0 with Not_found -> error "Field incompatible with structure" in Vstruc l
          | _ -> error "This expression is not a structure")
        in vars := Imap.add x v !vars;
        w
      with Not_found -> w)

  | Return None -> break Vnothing
  | Return (Some e) -> break (interp_expr e)

  | For((_,x),e1,e2,b) ->
      let v1 = interp_expr e1 and v2 = interp_expr e2 in
      let n1, n2 = (match v1, v2 with
        | (Vint n|Vbool n), (Vint m|Vbool m) -> n, m
        | _ -> error "'For' bounds must be integers";) in

        let v = ref Vnothing in
        let replacing, vx = try true, Imap.find x !vars with Not_found -> false, Vnothing in

        let i = ref n1 in
        while !v=Vnothing && !i<=n2 do
          vars := Imap.add x (Vint !i) !vars;
          v := interp_block b;
          i := add !i one;
        done;
        if replacing then vars := Imap.add x vx !vars;
        !v

  | While(e,b) ->
      let v = ref Vnothing in
      while is_true (interp_expr e) do
        let v' = interp_block b in
        if !v=Vnothing then v := v'
      done;
      !v

  | If(e,b1,b2) ->
      let v = interp_expr e in
      if is_true v then interp_block b1 else interp_block b2

(*return the last value in the block*)
and interp_block = function
  | [] -> Vnothing
  | [e] -> interp_expr e
  | e::b -> let _ = interp_expr e in interp_block b


and interp_fun f b =
  let vars_save = !vars and vars_f = ref !vars in

  let add_param p e =
    vars_f := Imap.add (snd p.p_name) (interp_expr e) !vars_f
  in

  let rec aux params b = match params, b with
    | [], [] -> ()
    | p::params, e::b -> add_param p e; aux params b
    | _ -> error "Unexpected number of parameters"
  in aux f.f_params b;

  vars := !vars_f;
  let v = try interp_block f.f_body with Ereturn v -> v in
  vars := vars_save;
  v


and interp_struc s b =
  let rec aux flds b = match flds, b with
    | [], [] -> []
    | x::flds, e::b -> (snd x.p_name, interp_expr e)::(aux flds b)
    | _ -> error "Unexpected number of fields"
  in Vstruc (aux s.f_params b)


(*Used only when changing a field : if the expression the field is attached to
is a variable or a block of blocks/variables, return the first variable with its value,
else raise Not_found. Also interpret the expr for eventual side effects*)
and var_of_expr e =
  (*returns variable as an option and its value*)
  let rec aux expr = let l , e = expr in match e with
    | Lval(_,Var(_,x)) -> Some x, interp_expr expr
    | Lval(_,Field(e,(_,x))) -> (* TO DO *) error "Not implemented yet"

    | Par [] -> None, Vnothing 
    | Par (e::b) -> let x0, v = aux e in
        (try Some (Option.get x0), v with Invalid_argument _ ->
          if v = Vnothing then aux (l,(Par b))
          else None, v)
    | _ -> None, interp_expr expr
  
  in try let x0, v = aux e in Option.get x0, v with Invalid_argument _ -> raise Not_found


in let interp = function
  | Func _ -> ()
  | Expr e -> let v = try interp_expr e with Ereturn v -> v in
      (match snd e with
      | Affect _ -> ()
      | _ -> print_value v)
in

List.iter interp l;