(**************************************
  formula and expression simplfication  
***************************************)

open Ast

let cannot_reach = Util.cannot_reach "Simplification"
let invalid_arg = Util.invalid_arg "Simplification"

let rec distribute_mult (e0: exp) : exp = match e0 with
  | Var _ | IConst _ -> e0
  | Add (e1, e2) ->
    let e1 = distribute_mult e1 in
    let e2 = distribute_mult e2 in
    Add (e1, e2)
  | Subtract (e1, e2) ->
    let e1 = distribute_mult e1 in
    let e2 = distribute_mult e2 in
    Subtract (e1, e2)
  | Mult (e1, e2) ->
    let e1 = distribute_mult e1 in
    let e2 = distribute_mult e2 in
    let res = match e1, e2 with
      | Add (e11, e12), e ->
          let e11 = distribute_mult (Mult (e11, e)) in
          let e12 = distribute_mult (Mult (e12, e)) in
          Add (e11, e12)
      | e, Add (e21, e22) ->
          let e21 = distribute_mult (Mult (e, e21)) in
          let e22 = distribute_mult (Mult (e, e22)) in
          Add (e21, e22)
      | Subtract (e11, e12), e ->
          let e11 = distribute_mult (Mult (e11, e)) in
          let e12 = distribute_mult (Mult (e12, e)) in
          Subtract (e11, e12)
      | e, Subtract (e21, e22) ->
          let e21 = distribute_mult (Mult (e, e21)) in
          let e22 = distribute_mult (Mult (e, e22)) in
          Subtract (e21, e22)
      | _ -> Mult (e1, e2)
    in res
    
let rec is_term (e0: exp) : bool = match e0 with
  | Var _ | IConst _ -> true
  | Add _ | Subtract _ -> false
  | Mult (e1, e2) -> (is_term e1) && (is_term e2)
  
let rec nomarlize_term (e0: exp) : exp =
  if not (is_term e0) then invalid_arg "Not a term!"
  else
    let res = match e0 with
      | IConst _ | Var _ -> e0
      (* TODO: Actually implement this, possibly way:
          1. Get a list of all var names
          2. Sort that list
          3. Reconstruct a term from the sorted list
          ??? howto deal with const *)
      | Mult (e1, e2) -> e0 (* FIXME: just place holder *)
      | _ -> cannot_reach "nomarlize_term"
    in res

let rec compare_term (e1: exp) (e2: exp) : int =
  if not (is_term e1 && is_term e2) then invalid_arg "Not a term!"
  else
    (* Assume that terms are already nomarlized
    let e1 = nomarlize_term e1 in
    let e2 = nomarlize_term e2 in
    *)
    let res = match e1, e2 with
      | IConst i1, IConst i2 -> compare i1 i2
      | IConst i1, _ -> -1
      | _, IConst i2 -> 1
      | Var sv1, Var sv2 ->
          let v1 = var_name sv1 in
          let v2 = var_name sv2 in
          compare v1 v2
      | Var sv1, _ -> -1
      | _, Var sv2 -> 1
      | Mult (e11, e12), Mult (e21, e22) ->
          let first_comparison = compare_term e11 e21 in
          if first_comparison = 0 then compare_term e12 e22
          else first_comparison
      | _, _ -> cannot_reach "compare_term"
    in res
    
let nomarlize_exp (e0: exp) : exp =
  let rec get_term_list (e0: exp) : exp list = match e0 with
    | Var _ | IConst _ | Mult _ -> [e0]
    | Add (e1, e2) | Subtract (e1, e2) ->
        let le1 = get_term_list e1 in
        let le2 = get_term_list e2 in
        List.append le1 le2
  in
  let e = distribute_mult e0 in
  match e with
    | Var _ | IConst _ -> e
    | Mult _ -> e (* Assume that term is already nomarlized during distribute_mult *)
    | _ -> e (* TODO: implement it *)

let negate_bformula (bf0: b_formula) : b_formula option = match bf0 with
  | BConst b -> Some (BConst (not b))
  | Lt (e1, e2) -> Some (Gte (e1, e2))
  | Lte (e1, e2) -> Some (Gt (e1, e2))
  | Gt (e1, e2) -> Some (Lte (e1, e2))
  | Gte (e1, e2) -> Some (Lt (e1, e2))
  | Eq (e1, e2) -> Some (Neq (e1, e2))
  | Neq (e1, e2) -> Some (Eq (e1, e2))

let rec negate_formula (f0: formula) : formula = match f0 with
  | BForm bf ->
      let neg_bf = negate_bformula bf in
      let res = match neg_bf with
        | Some new_bf -> BForm new_bf
        | None -> Not (BForm bf)
      in res
  | And (f1, f2) -> Or (negate_formula f1, negate_formula f2)
  | Or (f1, f2) -> And (negate_formula f1, negate_formula f2)
  | Not f -> f
  | Forall (sv, f) -> Exists (sv, negate_formula f)
  | Exists (sv, f) -> Forall (sv, negate_formula f)

let rec simplify_exp (e0: exp) : exp =
  match e0 with
  | Var _ | IConst _ -> e0
  | Add (e1, e2) ->
      let e1 = simplify_exp e1 in
      let e2 = simplify_exp e2 in
      let res = match e1, e2 with
        | IConst i1, IConst i2 -> IConst (i1+i2)
        | IConst 0, e -> e
        | e, IConst 0 -> e
        | _, _ -> Add (e1, e2)
      in res
  | Subtract (e1, e2) ->
      let e1 = simplify_exp e1 in
      let e2 = simplify_exp e2 in
      let res = match e1, e2 with
        | IConst i1, IConst i2 -> IConst (i1-i2)
        | IConst 0, e -> e
        | e, IConst 0 -> e
        | _, _ -> Subtract (e1, e2)
      in res
  | Mult (e1, e2) ->
      let e1 = simplify_exp e1 in
      let e2 = simplify_exp e2 in
      let res = match e1, e2 with
        | IConst i1, IConst i2 -> IConst (i1*i2)
        | IConst i1, e ->
            if i1 == 0 then IConst 0
            else if i1 == 1 then e
            else if i1 == -1 then Subtract (IConst 0, e)
            else Mult (e1, e2)
        | e, IConst i2 ->
            if i2 == 0 then IConst 0
            else if i2 == 1 then e
            else if i2 == -1 then Subtract (IConst 0, e)
            else Mult (e1, e2)
        | _ -> Mult (e1, e2)
      in res

let simplify_bformula (bf0: b_formula) : b_formula = 
  let helper e1 e2 op default =
    let e1 = simplify_exp e1 in
    let e2 = simplify_exp e2 in
    let res = match e1, e2 with
      | IConst i1, IConst i2 -> BConst (op (e1, e2))
      | _, _ -> default (e1, e2)
    in res
  in match bf0 with
    | BConst _ -> bf0
    | Lt (e1, e2) ->
        helper e1 e2 (function (v1, v2) -> v1 < v2) (function e -> Lt e)
    | Lte (e1, e2) ->
        helper e1 e2 (function (v1, v2) -> v1 <= v2) (function e -> Lte e)
    | Gt (e1, e2) ->
        helper e1 e2 (function (v1, v2) -> v1 > v2) (function e -> Gt e)
    | Gte (e1, e2) ->
        helper e1 e2 (function (v1, v2) -> v1 >= v2) (function e -> Gte e)
    | Eq (e1, e2) ->
        helper e1 e2 (function (v1, v2) -> v1 = v2) (function e -> Eq e)
    | Neq (e1, e2) ->
        helper e1 e2 (function (v1, v2) -> v1 != v2) (function e -> Neq e)

let rec simplify (f: formula) : formula = match f with
  | BForm bf -> BForm (simplify_bformula bf)
  | And (f1, f2) ->
      let f1 = simplify f1 in
      let f2 = simplify f2 in
      let res = match f1, f2 with
        | BForm (BConst false), _ -> BForm (BConst false)
        | BForm (BConst true), _ -> f2
        | _, BForm (BConst false) -> BForm (BConst false)
        | _, BForm (BConst true) -> f1
        | _, _ -> And (f1, f2)
      in res
  | Or (f1, f2) ->
      let f1 = simplify f1 in
      let f2 = simplify f2 in
      let res = match f1, f2 with
        | BForm (BConst true), _ -> BForm (BConst true)
        | BForm (BConst false), _ -> f2
        | _, BForm (BConst true) -> BForm (BConst true)
        | _, BForm (BConst false) -> f1
        | _, _ -> Or (f1, f2)
      in res
  | Not f1 ->
      let f1 = negate_formula f1 in
      simplify f1
  | Forall (sv, f1) ->
      let f1 = simplify f1 in
      Forall (sv, f1)
  | Exists (sv, f1) ->
      let f1 = simplify f1 in
      Exists (sv, f1)

let rec is_quantifier_free (f0: formula) : bool = match f0 with
  | BForm _ -> true
  | And (f1, f2) | Or (f1, f2) -> (is_quantifier_free f1) && (is_quantifier_free f2)
  | Not f1 -> is_quantifier_free f1
  | Forall _ | Exists _ -> false
