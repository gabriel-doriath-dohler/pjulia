open Ast
open Format

module Imap = Map.Make(String)

exception Interp_error of string
let error s = raise (Interp_error s)

type value =
  | Vnothing
  | Vbool of int (* booleans are considered as integers (true=1 and false=0)*)
  | Vint of int
  | Vstring of string
  | Vstruc of (string * value) list

exception Ereturn of value
let break v = raise (Ereturn v)

let boolint b = if b then Vbool 1 else Vbool 0

let rec print_value = function
  | Vnothing -> ()
  | Vbool 1 -> printf "True@?"
  | Vbool 0 -> printf "False@?"
  | Vbool _ -> error "Unexpected boolean error"
  | Vint n -> printf "%d@?" n
  | Vstring s -> printf "%s@?" s
  | Vstruc l ->
      let rec print_struc = function
        | [] -> ()
        | [(w,v)] -> printf "%s : @?" w; print_value v
        | (w,v)::q -> printf "%s : @?" w; print_value v; printf "; @?"
      in printf "{@?"; print_struc l; printf "}@?"

let rec power x = function
  | 0 -> 1
  | a when a<0 -> error "Negative power"
  | a -> x * (power x (a-1))

let rec assoc_replace x v = function
  | [] -> raise Not_found
  | (t,_)::l when x=t -> (x,v)::l
  | (t,w)::l -> (t,w)::(assoc_replace x v l)

let interp_binop op v1 v2 = match op, v1, v2 with
  | Add, (Vint n1|Vbool n1), (Vint n2|Vbool n2) -> Vint (n1+n2)
  | Sub, (Vint n1|Vbool n1), (Vint n2|Vbool n2) -> Vint (n1-n2)
  | Mul, (Vint n1|Vbool n1), (Vint n2|Vbool n2) -> Vint (n1*n2)
  | Mod, (Vint _|Vbool _), (Vint 0|Vbool 0) -> error "Division by zero"
  | Mod, (Vint n1|Vbool n1), (Vint n2|Vbool n2) -> Vint (n1 mod n2)
  | Pow, (Vint n1|Vbool n1), (Vint n2|Vbool n2) -> Vint (power n1 n2)
  | Eq, Vint n, Vbool b | Eq, Vbool b, Vint n -> boolint (n=b)
  | Eq, _, _ -> boolint (compare v1 v2 = 0)
  | Neq, _, _ -> boolint (compare v1 v2 <> 0)
  | L, Vint n1, Vint n2 -> boolint (n1 < n2)
  | Leq, Vint n1, Vint n2 -> boolint (n1 <= n2)
  | G, Vint n1, Vint n2 -> boolint (n1 > n2)
  | Geq, Vint n1, Vint n2 -> boolint (n1 >= n2)
  | _ -> error "Unsupported operand types"


let is_true = function
  | (Vint 1|Vbool 1) -> true
  | (Vint 0|Vbool 0) -> false
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
  | Int n -> Vint (Int64.to_int n)
  | Str s -> Vstring s
  | Bool b -> boolint b

  | Par b -> interp_block b

  | Call((_,"div"),[e1;e2]) -> (match interp_expr e1, interp_expr e2 with
      | (Vint n1|Vbool n1), (Vint n2|Vbool n2) -> Vint (n1/n2)
      | _ -> error "Integers were expected")
  | Call((_,"div"),_) -> error "Unexpected number of arguments"

  | Call((_,"print"),b) -> List.iter (fun e -> print_value (interp_expr e)) b; Vnothing
  | Call((_,"println"),b) -> List.iter (fun e -> print_value (interp_expr e); printf "\n@.") b; Vnothing

  | Call((_,id),b) -> if Imap.mem id functions
      then let f = Imap.find id functions in interp_fun f b
      else let s = (try Imap.find id structures with Not_found -> error "Unbound function/structure") in interp_struc s b

  | Not e -> boolint (not (is_true (interp_expr e)))
  | Binop(e1,And,e2) -> let v1 = interp_expr e1 in
      if is_true v1 then interp_expr e2 else Vbool 0
  | Binop(e1,Or,e2) -> let v1 = interp_expr e1 in
      if not (is_true v1) then interp_expr e2 else Vbool 1
  | Binop(e1,op,e2) -> interp_binop op (interp_expr e1) (interp_expr e2)

  | Lval (_,Var (_,x)) -> (try Imap.find x !vars with Not_found -> error "Unbound variable")
  | Lval (_,Field(e,(_,x))) ->
      (match interp_expr e with
      | Vstruc l -> (try List.assoc x l with Not_found -> error "Field incompatible with structure")
      | _ -> error "This expression is not a structure")

  | Affect((_,Var (_,x)),e) -> vars := Imap.add x (interp_expr e) !vars; Vnothing
  | Affect((_,Field(e1,(_,x))),e2) -> if not (Imap.mem x fields) then error "Unbound field";
      let v = interp_expr e2 in
      (match interp_expr e2 with
        | Vstruc l0 -> let l = try assoc_replace x v l0 with Not_found -> error "Field incompatible with structure" in Vstruc l
        | _ -> error "This expression is not a structure")

  | Return None -> break Vnothing
  | Return (Some e) -> break (interp_expr e)

  | For((_,x),e1,e2,b) ->
      let v1 = interp_expr e1 and v2 = interp_expr e2 in
      let n1, n2 = (match v1,v2 with
        | (Vint n|Vbool n), (Vint m|Vbool m) -> n, m
        | _ -> error "'For' bounds must be integers";) in

        let v = ref Vnothing in
        let replacing, vx = try true, Imap.find x !vars with Not_found -> false, Vnothing in

        let i = ref n1 in
        while !v=Vnothing && !i<=n2 do
          vars := Imap.add x (Vint !i) !vars;
          v := interp_block b;
          i := !i+1;
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


and interp_block = function
  | [] -> Vnothing
  | e::b -> (match interp_expr e with
      | Vnothing -> interp_block b
      | v -> v)


and interp_fun f b =
  let vars_save = !vars in

  let add_param p e =
    vars := Imap.add (snd p.p_name) (interp_expr e) !vars
  in

  let rec aux params b = match params, b with
    | [], [] -> ()
    | p::params, e::b -> add_param p e; aux params b
    | _ -> error "Unexpected number of parameters"
  in aux f.f_params b;

  let v = try interp_block f.f_body with Ereturn v -> v in
  vars := vars_save;
  v


and interp_struc s b =
  let rec aux flds b = match flds, b with
    | [], [] -> []
    | x::flds, e::b -> (snd x.p_name, interp_expr e)::(aux flds b)
    | _ -> error "Unexpected number of fields"
  in Vstruc (aux s.f_params b) in



  let interp = function
    | Func _ -> ()
    | Expr e -> let v = try interp_expr e with Ereturn v -> v in print_value v
  in

  List.iter interp l;
