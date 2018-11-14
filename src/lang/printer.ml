open PPrint
open PPrintEngine
open PPrintCombinators
open AST

let printer_error pos msg =
  Error.error "printing" pos msg

let ( ++ ) a b =
  a ^^ break 1 ^^ b

let located f x =
  f (Position.value x)

let located' f x =
  f (Position.position x) (Position.value x)

let optionally f x =
  match x with
  | None -> empty
  | Some y -> f y

let kwd = string

let sym = string

let comma_separated f xs = separate_map (sym "," ^^ break 1) f xs

let semi_separated f xs = separate_map (sym ";" ^^ break 1) f xs

let break_separated f xs = separate_map (break 1) f xs

let rec program ocaml ds =
  separate_map (hardline ^^ hardline) (toplevel_definition ocaml) ds

and toplevel_definition ocaml = function
  | ToplevelValue vd ->
     group (value_definition ocaml vd)
  | TypeDefinition td ->
     type_introduction td

and type_introduction (ts, t, td) =
  group (
      kwd "type"
      ^^ group (group (type_parameters ts)
                ++ located type_constructor_identifier t)
      ++ type_definition td
    )

and signature ocaml s =
  break_separated (declaration ocaml) s

and declaration ocaml = function
  | DeclareValue (x, s, ats) ->
     group (kwd "val"
            ^^ attributes ocaml ats
            ++ identifier x ++ sym ":" ++ type_scheme s)

  | DeclareType td ->
     type_introduction td

and type_definition = function
  | RecordType fs ->
     sym "=" ++ braces (semi_separated field_declaration fs)
  | Abstract ->
     empty

and type_parameters = function
  | [] ->
     empty
  | [t] ->
     break 1 ^^ located type_variable_identifier t
  | ts ->
     break 1 ^^ parens (comma_separated (located type_variable_identifier) ts)

and field_declaration (f, t) =
  group (located label f ^^ sym ":" ++ located ty t)

and value_definition ocaml = function
  | SimpleValue (p, t, ats) ->
     nest 2 (
       group (
         group (kwd "let "
                ^^ (attributes ocaml ats)
                ^^ located (pattern false type_scheme) p)
         ++ sym "=")
       ++ located' (term ocaml) t
     )

and attributes ocaml = function
  | [] ->
     empty
  | ats ->
     group (break_separated (attribute ocaml) ats ^^ break 1)

and attribute ocaml = function
  | Recursive ->
     kwd "rec"

and forbidden_in ocaml pos =
  if ocaml then printer_error pos "This construction is forbidden in OCaml."

and term ocaml pos = function
  | Var x ->
     identifier x
  | Lit l ->
     literal l
  | Let (vd, t) ->
     group (value_definition ocaml vd ++ kwd "in")
     ++ group (located' (term ocaml) t)
  | Lam (p, t) ->
     nest 2 (
         group (group (kwd "fun" ++ argument ocaml p) ++ sym "->")
         ++ group (located' (term ocaml) t)
     )
  | App (t, u) ->
     group (located (may_paren_term ocaml pos `LeftOfApp) t
            ++ located (may_paren_term ocaml pos `RightOfApp) u)
  | Proj (t, l) ->
     located (may_paren_term ocaml pos `UnderProj) t
     ^^ sym "." ^^ located label l
  | Record fs ->
     braces (semi_separated (field ocaml pos) fs)
  | Tuple  ts  ->
     group (PPrintOCaml.tuple (
         List.map (located (may_paren_term ocaml pos `UnderTuple)) ts
       ))

and may_paren_term ocaml pos context t =
  if match context, t with
     | (`LeftOfApp | `UnderTuple),
       (Lam _ | Let _)
       -> true
     | (`RightOfApp | `UnderNil | `UnderMove | `UnderDiff | `UnderBang
        | `UnderDerive | `UnderCached | `UnderProj | `Field),
       t ->
        (match t with Lit _ | Var _ | Tuple _ -> false | _ -> true)
     | _ -> false
  then
    group (parens (term ocaml pos t))
  else
    term ocaml pos t

and argument ocaml p =
  located (pattern ocaml ty) p

and field ocaml pos (l, t) =
  group (group (located label l ++ sym "=")
         ++ group (located (may_paren_term ocaml pos `Field) t))

and pattern
: type a. _ -> (a -> document) -> a pattern -> document
= fun ocaml annotation -> function
  | PVar (x, None) ->
     identifier x
  | PVar (x, Some a) ->
     if ocaml then
       identifier x
     else
       group (identifier x ++ sym ": " ^^ annotation a)
  | PTuple ps ->
     group (parens (comma_separated (located (pattern ocaml annotation)) ps))

and literal = function
  | LInt x ->
     string (string_of_int x)

and ty = function
  | TyVar x ->
     type_variable_identifier x
  | TyApp (t, []) ->
     group (located type_constructor_identifier t)
  | TyApp ({ Position.value = TCId "->" }, [t1; t2]) ->
     group (group (may_paren_ty t1 ++ sym "->") ++ located ty t2)
  | TyApp ({ Position.value = TCId "*" }, [t1; t2]) ->
     group (group (may_paren_ty t1 ++ sym "*" ++ may_paren_ty t2))
  | TyApp (t, ts) ->
     group (PPrintOCaml.tuple (List.map (located ty) ts)
            ++ located type_constructor_identifier t)

and may_paren_ty t =
  match Position.value t with
  | TyApp ({ Position.value = TCId "->" }, [t1; t2]) ->
     parens (located ty t)
  | _ ->
     located ty t

and type_scheme_annotation s =
  break 1 ^^ group (sym ":" ++ type_scheme s)

and type_scheme = function
  | TypeScheme ([], t) ->
     located ty t
  | TypeScheme (ts, t) ->
     group (
         separate_map (blank 1) (located type_variable_identifier) ts
         ^^ sym "." ++ located ty t
       )

and identifier = function Id x -> string x

and type_variable_identifier = function TVId x -> string ("'" ^ x)

and type_constructor_identifier = function TCId x -> string x

and label = function LId l -> string l

let save ?(ocaml = false) filename ast =
  let cout = open_out filename in
  ToChannel.pretty 0.8 100 cout (program ocaml ast);
  output_string cout "\n";
  close_out cout

let string_of what x =
  let b = Buffer.create 13 in
  ToBuffer.pretty 0.8 100 b (what x);
  Buffer.contents b

let trace_pass basename f ext ast =
  let xast = f basename ast in
  if !Options.trace then save (basename ^ "." ^ ext) xast;
  xast
