(*
  mixed integer real linear quantifier elimination
 *)

open Ast

let rec is_integer_valued e0 = match e0 with
  | Var IntVar _ | IConst _ | Floor _ -> true
  | Var FloatVar _ | FConst _ -> false
  | Add (e1, e2) | Subtract (e1, e2) -> (is_integer_valued e1) && (is_integer_valued e2)
  | Mult (i, e) -> is_integer_valued e

let rec simplify_exp (e0: exp) : exp = match e0 with
  | Var _ | IConst _ | FConst _ -> e0
  | Add (e1, e2) ->
      let e1 = simplify_exp e1 in
      let e2 = simplify_exp e2 in
      let res = match e1, e2 with
        | IConst i1, IConst i2 -> IConst (i1+i2)
        | FConst f1, FConst f2 -> FConst (f1+.f2)
        | IConst i1, FConst f2 -> FConst ((float_of_int i1)+.f2)
        | FConst f1, IConst i2 -> FConst (f1+.(float_of_int i2))
        | IConst 0, e -> e
        | FConst 0.0, e -> e
        | e, IConst 0 -> e
        | e, FConst 0.0 -> e
        | _, _ -> Add (e1, e2)
      in res
  | Subtract (e1, e2) ->
      let e1 = simplify_exp e1 in
      let e2 = simplify_exp e2 in
      let res = match e1, e2 with
        | IConst i1, IConst i2 -> IConst (i1-i2)
        | FConst f1, FConst f2 -> FConst (f1-.f2)
        | IConst i1, FConst f2 -> FConst ((float_of_int i1)-.f2)
        | FConst f1, IConst i2 -> FConst (f1-.(float_of_int i2))
        | IConst 0, e -> e
        | FConst 0.0, e -> e
        | e, IConst 0 -> e
        | e, FConst 0.0 -> e
        | _, _ -> Subtract (e1, e2)
      in res
  | Mult (i, e1) ->
      let e1 = simplify_exp e1 in
      let res = match e1 with
        | IConst i1 -> IConst (i*i1)
        | FConst f1 -> FConst ((float_of_int i)*.f1)
        | _ -> 
            if i = 0 then IConst 0
            else if i = 1 then e1
            else Mult (i, e1)
      in res
  | Floor e1 ->
      let e1 = simplify_exp e1 in
      let res = match e1 with
        | FConst f1 -> IConst (int_of_float f1)
        | _ -> if is_integer_valued e1 then e1 else Floor e1
      in res

let simplify_bformula (bf0: b_formula) : b_formula = 
  let float_val e = match e with
    | IConst i -> Some (float_of_int i)
    | FConst f -> Some f
    | _ -> None
  in
  let helper e1 e2 op default =
    let e1 = simplify_exp e1 in
    let e2 = simplify_exp e2 in
    let val1 = float_val e1 in
    let val2 = float_val e2 in
    let res = match val1, val2 with
      | Some f1, Some f2 -> BConst (op (e1, e2))
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

let rec split_z (e0: exp) (v0: spec_var): (int * exp) = match e0 with
  | Var v -> 
      if is_same_var v0 v then
        (1, IConst 0)
      else
        (0, e0)
  | IConst _ | FConst _ -> (0, e0)
  | Add (e1, e2) ->
      let c1, s1 = split_z e1 v0 in
      let c2, s2 = split_z e2 v0 in
      (c1+c2, Add (s1, s2))
  | Subtract (e1, e2) ->
      let c1, s1 = split_z e1 v0 in
      let c2, s2 = split_z e2 v0 in
      (c1-c2, Subtract (s1, s2))
  | Mult (i, e1) ->
      let c1, s1 = split_z e1 v0 in
      (i*c1, Mult (i, s1))
  | Floor e1 ->
      let c1, s1 = split_z e1 v0 in
      (c1, Floor s1)
    
let lin_z_bformula (bf0: b_formula) (v0: spec_var) : formula = 
  let rec helper bf0 v0 = match bf0 with
    | BConst _ -> BForm bf0
    | Lt (e1, e2) ->
        let c1, s1 = split_z e1 v0 in
        let c2, s2 = split_z e2 v0 in
        let c = c1 - c2 in
        let cv = Mult (c, Var v0) in
        let s = Subtract (s2, s1) in
        let part1 = BForm (Lt (cv, Floor s)) in
        let part2 = BForm (Eq (cv, Floor s)) in
        let part3 = BForm (Lt (Floor s, s)) in
        Or (part1, And (part2, part3))
    | Lte (e1, e2) ->
        Or ((helper (Lt (e1, e2)) v0), (helper (Eq (e1, e2)) v0))
    | Gt (e1, e2) ->
        helper (Lt (e2, e1)) v0
    | Gte (e1, e2) ->
        helper (Lte (e2, e1)) v0
    | Eq (e1, e2) ->
        let c1, s1 = split_z e1 v0 in
        let c2, s2 = split_z e2 v0 in
        let c = c1 - c2 in
        let s = Subtract (s2, s1) in
        let part1 = BForm (Eq (Mult (c, Var v0), Floor s)) in
        let part2 = BForm (Eq (s, Floor s)) in
        And (part1, part2)
    | Neq (e1, e2) ->
        Not (helper (Eq (e1, e2)) v0)
  in simplify (helper bf0 v0)

let rec lin_z (f0: formula) (v0: spec_var): formula = match f0 with
  | BForm bf -> lin_z_bformula bf v0
  | And (f1, f2) -> And (lin_z f1 v0, lin_z f2 v0)
  | Or (f1, f2) -> Or (lin_z f1 v0, lin_z f2 v0)
  | Not f1 -> Not (lin_z f1 v0)
  | Forall _ | Exists _ -> failwith "lin_z was called with quantified formula"

let rec is_quantifier_free f0 = match f0 with
  | BForm _ -> true
  | And (f1, f2) | Or (f1, f2) -> (is_quantifier_free f1) && (is_quantifier_free f2)
  | Not f1 -> is_quantifier_free f1
  | Forall _ | Exists _ -> false

let rec qe_z (f0: formula): formula =
  let helper f0 = match f0 with
    | BForm _ -> f0
    | And (f1, f2) -> And (qe_z f1, qe_z f2)
    | Or (f1, f2) -> Or (qe_z f1, qe_z f2)
    | Not f1 -> Not (qe_z f1)
    | Exists (sv, f) ->
        if is_quantifier_free f then
          let res = match f with
            | BForm bf -> Exists (sv, lin_z_bformula bf sv)
            | Or (f1, f2) ->
                let f1 = Exists (sv, f1) in
                let f2 = Exists (sv, f2) in
                Or (qe_z f1, qe_z f2)
            | And (f1, f2) ->
                let lin_f1 = lin_z f1 sv in
                let lin_f2 = lin_z f2 sv in
                Exists (sv, And (lin_f1, lin_f2))
            | Not f1 ->
                Exists (sv, Not (lin_z f1 sv))
            | _ -> failwith "Impossible error! The program cannot reach this point."
          in res
        else
          f0
    | Forall (sv, f) ->
        Not (qe_z (Exists (sv, Not f)))
  in simplify (helper f0)
(*
let rec split_r (e0: exp) (v0: spec_var): ((formula * int * exp) list)
*)
