open Position
open AST

let arrow_type_constructor = TCId "->"

let monoscheme ty = TypeScheme ([], ty)

let arrow pos ity oty =
  TyApp (with_pos pos arrow_type_constructor, [ ity; oty ])

let product_type_constructor = TCId "*"

let product pos ty1 ty2 =
  TyApp (with_pos pos product_type_constructor, [ ty1; ty2 ])

exception NoProduct

let unproduct = function
  | TyApp ({ value = TCId "*" }, tys) -> tys
  | _ -> raise NoProduct

let rec productn pos = function
  | [] -> assert false
  | [x] -> x
  | x :: xs -> with_pos pos (product pos x (productn pos xs))

module TVSet = Set.Make (struct
                   type t = type_variable_identifier
                   let compare = compare
                 end)

let rec ftv = TVSet.(function
  | TyVar x ->
     singleton x
  | TyApp (_, ts) ->
     List.fold_left (fun s ty -> union s (ftv ty.value)) empty ts)

module VSet = Set.Make (struct
                  type t = identifier
                  let compare = compare
                end)

let rec pattern_variables = function
  | PVar (x, _) ->
     [x]
  | PTuple ps ->
     List.fold_left (fun s p -> s @ (pattern_variables p.value)) [] ps

let pattern_variables_set p =
  VSet.(List.fold_left (fun s x -> add x s) empty (pattern_variables p))

exception UntypedVariableInPattern

let rec pattern_bindings = function
  | PVar (x, Some s) ->
     [(x, s)]
  | PVar (x, None) ->
     raise UntypedVariableInPattern
  | PTuple ps ->
     List.fold_left (fun s p -> s @ pattern_bindings p.value) [] ps

let rec fv = VSet.(function
  | Var x ->
     singleton x
  | Lit _ ->
     empty
  | App (t, u) ->
     union (fv t.value) (fv u.value)
  | Lam (p, t) ->
     diff (fv t.value) (pattern_variables_set p.value)
  | Let (vd, t) ->
     let (bvs, fvs) = value_definition_fv vd in
     union fvs (diff (fv t.value) bvs)
  | Record fs ->
     List.fold_left (fun s (_, t) -> union s (fv t.value)) empty fs
  | Tuple ts ->
     List.fold_left (fun s t -> union s (fv t.value)) empty ts
  | Proj (t, _) ->
     fv t.value
)
and value_definition_fv = VSet.(function
  | SimpleValue (p, t, _) ->
     (pattern_variables_set p.value, fv t.value)
)

let rec mk_let ds t =
  match ds with
  | [] -> t
  | d :: ds -> with_pos t.position (Let (d, mk_let ds t))

let map fterm p =
  let rec program p =
    List.map toplevel_definition p
  and toplevel_definition = function
    | ToplevelValue vd ->
       ToplevelValue (value_definition vd)
    | td ->
       td
  and value_definition = function
    | SimpleValue (p, t, attributes) ->
       SimpleValue (p, term' t, attributes)
  and term' t =
    with_pos t.position (fterm term t.value)
  and term = function
    | Let (d, t) ->
       Let (value_definition d, term' t)
    | Lam (p, t) ->
       Lam (p, term' t)
    | Tuple ts ->
       Tuple (List.map term' ts)
    | Record fs ->
       Record (List.map (fun (l, t) -> (l, term' t)) fs)
    | App (t, u) ->
       App (term' t, term' u)
    | Proj (t, l) ->
       Proj (term' t, l)
    | (Var _ | Lit _) as u ->
       u
  in
  program p

let contextual_map fterm empty bind p =
  let bind_pattern env p =
    List.fold_left (fun env x -> bind x env) env (located pattern_variables p)
  in
  let rec program p =
    ExtStd.List.fold_map toplevel_definition empty p
  and toplevel_definition env = function
    | ToplevelValue vd ->
       bind_value_definition env vd, ToplevelValue (value_definition env vd)
    | td ->
       env, td
  and value_definition env = function
    | SimpleValue (p, t, attributes) ->
       SimpleValue (p, term' env t, attributes)
  and term' env t =
    with_pos t.position (fterm term env t.value)
  and term env = function
    | Let (d, t) ->
       Let (value_definition env d, term' env t)
    | Lam (p, t) ->
       let env = bind_pattern env p in
       Lam (p, term' env t)
    | Tuple ts ->
       Tuple (List.map (term' env) ts)
    | Record fs ->
       Record (List.map (fun (l, t) -> (l, term' env t)) fs)
    | App (t, u) ->
       App (term' env t, term' env u)
    | Proj (t, l) ->
       Proj (term' env t, l)
    | (Var _ | Lit _) as u ->
       u
  and bind_value_definition env = function
    | SimpleValue (p, t, attributes) ->
       bind_pattern env p
  in
  program p

exception UnboundValue of identifier

let rec lookup_definition env f =
  match env with
  | [] ->
     raise (UnboundValue f)
  | (SimpleValue (p, _, _) as d) :: env ->
     if VSet.mem f (pattern_variables_set p.value) then
       d
     else
       lookup_definition env f
