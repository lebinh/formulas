type id = string

type spec_var =
  | IntVar of id

type formula =
  | BForm of b_formula
  | And of (formula * formula)
  | Or of (formula * formula)
  | Not of formula
  | Forall of (spec_var * formula)
  | Exists of (spec_var * formula)

and b_formula =
  | BConst of bool
  (* all comparisons are comparing to 0 *)
  | Lt of (exp * exp)
  | Lte of (exp * exp)
  | Gt of (exp * exp)
  | Gte of (exp * exp)
  | Eq of (exp * exp)
  | Neq of (exp * exp)
   
and exp =
  | Var of spec_var
  | IConst of int
  | Add of (exp * exp)
  | Subtract of (exp * exp)
  | Mult of (exp * exp)

let mkSpecVar id =
  IntVar id  

let mkVar id = Var (mkSpecVar id)
and mkAnd f1 f2 = And (f1, f2)
and mkOr f1 f2 = Or (f1, f2)
and mkNot f = Not f
and mkForall v f = Forall (mkSpecVar v, f)
and mkExists v f = Exists (mkSpecVar v, f)

and mkTrue = BConst true
and mkFalse = BConst false
and mkLt e1 e2 = Lt (e1, e2)
and mkLte e1 e2 = Lte (e1, e2)
and mkGt e1 e2 = Gt (e1, e2)
and mkGte e1 e2 = Gte (e1, e2)
and mkEq e1 e2 = Eq (e1, e2)
and mkNeq e1 e2 = Neq (e1, e2)

and mkAdd e1 e2 = Add (e1, e2)
and mkSubtract e1 e2 = Subtract (e1, e2)
and mkMult e1 e2 = Mult (e1, e2)

let var_name sv = match sv with
  | IntVar id -> id
    
let string_of_spec_var sv = var_name sv

let rec string_of_formula f = match f with
  | BForm b -> string_of_b_formula b
  | And (f1, f2) -> "(" ^ (string_of_formula f1) ^ ") and (" ^ (string_of_formula f2) ^ ")"
  | Or (f1, f2) -> "(" ^ (string_of_formula f1) ^ ") or (" ^ (string_of_formula f2) ^ ")"
  | Not f1 -> "not(" ^ (string_of_formula f1) ^ ")"
  | Forall (sv, f1) -> "all (" ^ (string_of_spec_var sv) ^ ", " ^ (string_of_formula f1) ^ ")"
  | Exists (sv, f1) -> "ex (" ^ (string_of_spec_var sv) ^ ", " ^ (string_of_formula f1) ^ ")"
  
and string_of_b_formula bf = 
  let helper e1 e2 op =
    (string_of_exp e1) ^ op ^ (string_of_exp e2)
  in match bf with
    | BConst b -> (string_of_bool b)
    | Lt (e1, e2) -> helper e1 e2 " < "
    | Lte (e1, e2) -> helper e1 e2 " <= "
    | Gt (e1, e2) -> helper e1 e2 " > "
    | Gte (e1, e2) -> helper e1 e2 " >= "
    | Eq (e1, e2) -> helper e1 e2 " = "
    | Neq (e1, e2) -> helper e1 e2 " != "

and string_of_exp e0 = 
  let need_parentheses e = match e with
    | Var _ | IConst _ | Mult _ -> false
    | Add _ | Subtract _ -> true
  in let wrap e =
    if need_parentheses e then "(" ^ (string_of_exp e) ^ ")"
    else (string_of_exp e)
  in match e0 with
    | Var sv -> string_of_spec_var sv
    | IConst i -> string_of_int i
    | Add (e1, e2) ->
        let e1 = string_of_exp e1 in
        let e2 = string_of_exp e2 in
        e1 ^ " + " ^ e2
    | Subtract (e1, e2) ->
        let e1 = string_of_exp e1 in
        let e2 = string_of_exp e2 in
        e1 ^ " - " ^ e2
    | Mult (e1, e2) ->
        let e1 = wrap e1 in
        let e2 = wrap e2 in
        e1 ^ "*" ^ e2

let is_same_var (v1: spec_var) (v2: spec_var) = match v1, v2 with
  | IntVar id1, IntVar id2 -> if id1 = id2 then true else false
  