(**************************************
  formula or expression simplfication  
***************************************)

open Ast

let rec simplify_exp (e0: exp) : exp = match e0 with
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

let rec simplify f = match f with
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
      let f1 = simplify f1 in
      let res= match f1 with
        | BForm (BConst b) -> BForm (BConst (not b))
        | _ -> Not f1
      in res
  | Forall (sv, f1) ->
      let f1 = simplify f1 in
      Forall (sv, f1)
  | Exists (sv, f1) ->
      let f1 = simplify f1 in
      Exists (sv, f1)

let rec is_quantifier_free f0 = match f0 with
  | BForm _ -> true
  | And (f1, f2) | Or (f1, f2) -> (is_quantifier_free f1) && (is_quantifier_free f2)
  | Not f1 -> is_quantifier_free f1
  | Forall _ | Exists _ -> false

