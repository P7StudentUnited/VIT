open Position
open AST
open ProgramManipulation

let type_error = Error.error "typing"

type type_binding =
  type_constructor_identifier * type_variable_identifier list * type_definition

type environment =
  Environment of type_binding list

let bind_type_definition t ts td (Environment env) =
  Environment ((t, ts, td) :: env)

module S = struct

  type 'a structure =
    | TyApp of type_constructor_identifier located * 'a list

  let rec map f (TyApp (t, ts)) = TyApp (t, List.map f ts)

  let fold f (TyApp (_, ts)) accu = List.fold_right f ts accu

  let iter f (TyApp (_, ts)) = List.iter f ts

  exception Iter2

  let rec iter2 f (TyApp (t, ts)) (TyApp (u, us)) =
    let u = Position.value u and t = Position.value t in
    if u = t then List.iter2 f ts us else raise Iter2

end

module O = struct

  type tyvar =
    int

  type 'a structure =
    'a S.structure

  type ty =
    AST.ty

  let type_variable_identifier x =
    let b = Buffer.create 13 in
    let rec mangle x =
      Buffer.add_char b (Char.chr (97 + x mod 26));
      if x > 26 then mangle (x / 26)
    in
    mangle (Utilities.in_order x);
    TVId (Buffer.contents b)

  let variable x =
    AST.TyVar (type_variable_identifier x)

  let structure (S.TyApp (t, ts)) =
    let pos = Position.position t in
    let ts = List.map (Position.with_pos pos) ts in
    AST.TyApp (t, ts)

  let mu x t =
    failwith "No recursive types"

  type scheme =
    tyvar list * ty

end

module Solver =
  Inferno.SolverHi.Make(struct include String type tevar = t end)(S)(O)

let located f x =
  f (Position.position x) (Position.value x)

let mk_tvar pos x =
  with_pos pos (O.type_variable_identifier x)

let mk_type_scheme pos ts ty =
  TypeScheme (List.map (mk_tvar pos) ts, with_pos pos ty)

let int_type =
  S.TyApp (Position.unknown_pos (TCId "int"), [])

let unit_type =
  S.TyApp (Position.unknown_pos (TCId "unit"), [])

let arrow t1 t2 =
  S.TyApp (Position.unknown_pos (TCId "->"), [t1; t2])

let pair t1 t2 =
  S.TyApp (Position.unknown_pos (TCId "*"), [t1; t2])

let fresh_variable_identifier =
  let i = ref 0 in
  fun () -> incr i; Printf.sprintf "_%d" !i

open Solver

let defn xs f =
  let rec aux vs xs =
    match xs with
    | [] ->
       f (List.rev vs) <$$> fun a -> ([], a)
    | x :: xs ->
       exist (fun v ->
           def x v (aux (v :: vs) xs)
         )
       <$$> fun (ty, (tys, a)) -> (ty :: tys, a)
  in
  aux [] xs

let rec conjs = function
  | [] -> pure []
  | [c] -> c <$$> fun x -> [x]
  | c :: cs -> c ^& conjs cs <$$> fun (x, xs) -> x :: xs

let existn xs f =
  let rec aux vs = function
    | [] ->
       f (List.rev vs)
    | x :: xs ->
       exist (fun v -> aux ((x, v) :: vs) xs)
       <$$> fun (ty, a) -> a
  in
  aux [] xs

let rec is_productn v : _ list -> unit co = function
  | [] ->
     pure ()
  | [x] ->
     v -- x
  | x :: xs ->
     exist (fun v' -> v --- pair x v' ^& is_productn v' xs)
     <$$> fun (_, ((), ())) -> ()

let ctrue = pure ()

(** Assume that the free variables of [ty] are existentially
    quantified. *)
let internalize_ty v ty =
  let env = ref [] in
  let rec aux v = function
    | AST.TyVar x ->
       begin try
           v -- List.assoc x !env
         with Not_found ->
           env := (x, v) :: !env;
           ctrue
       end
    | AST.TyApp (t, tys) ->
       existn tys (fun xvs ->
           let vs = snd (List.split xvs) in
           List.fold_left (fun c (ty, v) ->
               c ^^ aux v (Position.value ty)
             ) (v --- S.TyApp (t, vs)) xvs
         )
  in
  aux v ty

let match_result (TypeScheme (_, sty)) ty =
  let result_of_arrow =
    exist (fun v1 ->
        exist (fun v2 ->
            exist (fun v3 ->
                internalize_ty v3 ty
                ^^ internalize_ty v2 sty.value
                ^^ v2 --- arrow v1 v3)))
    <$$> fun (ty, _) -> ProgramManipulation.unproduct ty
  in
  let exact_result =
    exist (fun v ->
        internalize_ty v ty
        ^^ internalize_ty v sty.value)
    <$$> fun _ -> []
  in
  try Some (snd (Solver.solve false (let0 result_of_arrow)))
  with _ -> try Some (snd (Solver.solve false (let0 exact_result)))
            with _ -> None

exception UnboundLabel of label Position.located

let lookup_projection (Environment env : environment) l =
  try
    let (t, ts, td) =
      List.(find (fun (t, ts, td) ->
                match td with
                | AST.RecordType fds ->
                   exists (fun (l', _) -> l'.value = l.value) fds
                | _ -> false)
              env)
    in
    let _, lty =
      match td with
      | AST.RecordType fds -> List.find (fun (l', _) -> l'.value = l.value) fds
      | _ -> assert false (* By previous List.find. *)
    in
    let tys = List.map (fun x -> Position.with_pos l.position (TyVar x)) ts in
    let rty = TyApp (Position.with_pos l.position t, tys) in
    let ts = List.map (Position.with_pos l.position) ts in
    (ts, rty, lty.value, td)
  with Not_found ->
    raise (UnboundLabel l)

let type_constructor_arity (Environment env) t =
  try
    let (_, ts, td) = List.find (fun (t', _, _) -> t.value = t') env in
    List.length ts
  with Not_found ->
    let TCId x = t.value in
    type_error t.position (Printf.sprintf "Unbound type constructor `%s'." x)

module ConstraintGeneration = struct

let rec pattern_variables pos = function
  | PVar (Id x, _) -> [x]
  | PTuple ps -> List.(concat (map (located pattern_variables) ps))

let rec program env (t : AST.program) : AST.program Solver.co =
  match t with
  | [] ->
     pure []

  | td :: tds ->
     let env, constraint_ctx = toplevel_definition env td in
     constraint_ctx (program env tds)

and toplevel_definition (type a b) env = function
  | ToplevelValue vd ->
     let constraint_ctx = value_definition env vd in
     let constraint_ctx = fun c ->
       constraint_ctx c
       <$$> fun (vd', u') -> (ToplevelValue vd') :: u'
     in
     (env, constraint_ctx)

  | TypeDefinition td ->
     let (env, ctx) = type_introduction env td in
     let ctx = fun c ->
       ctx c
       <$$> fun (td, u) -> (TypeDefinition td) :: u in
     (env, ctx)

and signature env = function
  | [] ->
     (env, fun c -> c <$$> fun a -> ([], a))
  | d :: ds ->
     let env, ctx = declaration env d in
     let env, ctx' = signature env ds in
     let ctx = fun c ->
       ctx (ctx' c)
       <$$> fun (d, (ds, a)) -> (d :: ds, a)
     in
     (env, ctx)

and declaration
  : type a. _ -> _ -> _ * (a co -> (declaration * a) co)
  = fun env -> function
  | (DeclareValue (Id x, TypeScheme (ts, ty), _)) as d ->
     let ctx = fun c ->
       Solver.let1 x (fun v ->
           internalize_ty v ty.value
         ) c
       <$$> fun (_, _, _, a) -> (d, a)
     in
     (env, ctx)

  | DeclareType td ->
     let (env, ctx) = type_introduction env td in
     let ctx = fun c ->
       ctx c <$$> fun (td, a) -> (DeclareType td, a)
     in
     (env, ctx)

and type_introduction
: type a. _ -> _ -> _ * (a co -> (type_introduction * a) co)
= fun env (ts, t, td) ->
  let env = type_definition env ts t td in
  let constraint_ctx = fun c ->
    c <$$> fun a -> ((ts, t, td), a)
  in
  (env, constraint_ctx)

and type_definition env ts t td =
  match td with
  | Abstract ->
     let ts' = List.map Position.value ts in
     bind_type_definition t.value ts' td env
  | RecordType fds ->
     let ftvs =
       TVSet.(List.fold_left (fun ftvs ty ->
                  union ftvs (ftv ty.value))
                empty (snd (List.split fds)))
     in
     let ts' = List.map Position.value ts in
     if not TVSet.(subset ftvs (of_list ts')) then
       type_error t.position "Undeclared type variable.";
     List.iter (fun (l, _) -> try
           ignore (lookup_projection env l);
           type_error l.position "Label already used by another type."
         with _ -> ()) fds;
     let env = bind_type_definition t.value ts' (RecordType fds) env in
     List.iter (located (well_formed_type env)) (snd (List.split fds));
     env

and well_formed_type env pos = function
  | TyVar _ ->
     ()
  | TyApp (t, tys) ->
     if type_constructor_arity env t <> List.length tys then
       type_error t.position "Invalid application of type constructor.";
     List.iter (located (well_formed_type env)) tys

and value_definition
: type a. _ -> value_definition -> a co -> (value_definition * a) co
= fun env -> function
  | SimpleValue (p, t, ats) -> fun c ->

     let xs = located pattern_variables p in
     Solver.letn xs (fun vs ->
         let ctx c =
           if List.mem Recursive ats then
             defn xs (fun vs' ->
                 conjs (List.map2 ( -- ) vs vs') ^^ c
               ) <$$> fun (_, c) -> c
           else
             c
         in
         let xvs = List.combine xs vs in
         ctx (
             exist (fun v ->
                 located (term env) t v
                 ^& located (pattern env v xvs Polymorphic) p)
           )
     ) c

     <$$> (fun (ss, ts, (_, (t', p')), a) ->
         let pos = Position.position p in
         let ss = List.map (fun (ts, ty) -> mk_type_scheme pos ts ty) ss in
         let inferred = List.combine xs ss in
         (SimpleValue (p' inferred, t', ats), a)
       )

and term env pos t ty =
  match t with
  | Lit (LInt x) ->

     ty --- int_type

     <$$> fun _ ->
     with_pos pos t

  | Var (Id x) ->

     instance x ty

     <$$> fun _ ->
     with_pos pos t

  | Lam (p, t) ->

     let xs = located pattern_variables p in
     exist (fun v1 ->
         exist (fun v2 ->
             defn xs (fun vs ->
                 let xvs = List.combine xs vs in
                 ty --- arrow v1 v2
                 ^& located (pattern env v1 xvs Monomorphic) p
                 ^& located (term env) t v2)))

     <$$> fun (ty1, (ty2, (tys, ((), (p', t'))))) ->
     let tys = List.(combine xs tys) in
     with_pos pos (Lam (p' tys, t'))

  | App (t, u) ->

     exist (fun v1 ->
         exist (fun v2 ->
             v1 --- arrow v2 ty
             ^& located (term env) t v1
             ^& located (term env) u v2))

    <$$> fun (ty1, (ty2, ((), (t', u')))) ->
    with_pos pos (App (t', u'))

  | Let (vd, t) ->
     let constraint_context = value_definition env vd in

     constraint_context (located (term env) t ty)

     <$$> fun (vd, t) ->
     with_pos pos (Let (vd, t))

  | Tuple [t] ->
     located (term env) t ty

  | Tuple (t :: ts) ->

     let t' = match ts with
       | [] -> assert false
       | [t] -> t
       | ts -> with_pos pos (Tuple ts)
     in

     exist (fun v1 ->
         exist (fun v2 ->
             ty --- pair v1 v2
             ^& located (term env) t v1
             ^& located (term env) t' v2))

     <$$> fun (_, (_, ((), (t, t')))) ->
     with_pos pos begin match Position.value t' with
     | Tuple ts -> Tuple (t :: ts)
     | _ -> Tuple [t; t']
     end

  | Proj (t, l) ->
     let (_, rty, lty, _) = lookup_projection env l in
     let rty = Position.with_pos t.position rty in
     let lty = Position.with_pos l.position lty in
     exist_ (fun vr ->
         exist_ (fun vv ->
             vv --- arrow vr ty
             ^^ internalize_ty vv (ProgramManipulation.arrow pos rty lty)
             ^^ located (term env) t vr))
     <$$> (fun t' -> with_pos pos (Proj (t', l)))

  | Record (((l, _) :: _) as fs) ->
     let (_, rty, _, td) = lookup_projection env l in
     begin match td with
     | RecordType fds ->
        let compare_label (l1, _) (l2, _) = compare l1.value l2.value in
        let fds = List.sort compare_label fds
        and fs = List.sort compare_label fs in
        exist_ (fun va ->
            exist_ (fun vp ->
                existn fds (fun fdvs ->
                    let vs = snd (List.split fdvs) in
                    let ftys = snd (List.split fds) in
                    let rty = with_pos pos rty in
                    let all = ProgramManipulation.(
                        arrow pos (productn pos ftys) rty
                    ) in
                    internalize_ty va all
                    ^^ va --- arrow vp ty
                    ^^ is_productn vp vs
                    ^^ let rec fields fs fds =
                         match fs, fds with
                         | [], [] ->
                            pure []
                         | ({ value = LId x } as l, e) :: fs, (l', _) :: fds ->
                            if l.value <> l'.value then
                              type_error l.position (
                                  Printf.sprintf "`%s' is an invalid label." x
                                );
                            let (_, v) =
                              try
                                List.find (fun ((l'', _), _) ->
                                    l''.value = l.value
                                  ) fdvs
                              with Not_found -> assert false (* By existn. *)
                            in
                            located (term env) e v ^& fields fs fds
                            <$$> fun (t, ts) -> (l, t) :: ts
                         | ({ value = LId x } as l, e) :: _, [] ->
                            type_error l.position (
                                Printf.sprintf "`%s' is an invalid label." x
                              )
                         | [], (({ value = LId x } as l), _) :: _ ->
                            type_error l.position (
                                Printf.sprintf "A value for `%s' is missing." x
                              )
                       in
                       fields fs fds)))
        <$$> fun fs -> with_pos pos (Record fs)
     | _ ->
        assert false (* By lookup_projection. *)
     end

  | Tuple [] ->
     (ty --- unit_type) <$$> fun () -> with_pos pos (Tuple [])

  | Record [] ->
     assert false (* By syntax. *)

and pattern
: type a kind.
  _ -> _ -> _ -> kind kind_descriptor
  -> _ -> kind pattern ->
  ((string * a) list -> a pattern located) co
= fun env v xvs kd pos -> function
  | PVar (Id x, a) ->
     let v' = try List.assoc x xvs with _ -> assert false in
     (match a with
      | None -> ctrue
      | Some a ->
         match kd with
         | Monomorphic -> (* a is a ty. *)
            internalize_ty v' a
         | Polymorphic -> (* a is a type scheme. *)
            let TypeScheme (_, ty) = a in
            internalize_ty v' ty.value)
     ^^ v -- v'
     <$$> fun () ->
     fun (inferred_schemes : (string * a) list) ->
     let s =
       try List.assoc x inferred_schemes with _ -> assert false
     in
     with_pos pos (PVar (Id x, Some s))

   | PTuple ps ->
      existn ps (fun xps ->
          (is_productn v (snd (List.split xps)))
          ^& conjs (List.map (fun (p, v) ->
                        located (pattern env v xvs kd) p) xps))
      <$$> fun (_, ps) inferred ->
      let ps = List.map (fun p -> p inferred) ps in
      with_pos pos (PTuple ps)

end

let initial_env : environment = AST.(

  Environment [

      (let a = TVId "'a" and b = TVId "'b" in (TCId "->", [a; b], Abstract));

      (TCId "int", [], Abstract);

      (TCId "unit", [], Abstract);

    ]
)

let process basename ast =
  try
    let program_constraint = ConstraintGeneration.program initial_env ast in
    let global_constraint = let0 program_constraint in
    snd (Solver.solve false global_constraint)
  with
  | Unify (ty1, ty2) ->
     type_error Position.dummy (
         Printf.sprintf
           "Incompatible types: %s <> %s\n"
           (Printer.string_of Printer.ty ty1)
           (Printer.string_of Printer.ty ty2))

  | UnboundLabel ({ value = LId l; position }) ->
     type_error position (Printf.sprintf "Unbound label `%s'." l)
