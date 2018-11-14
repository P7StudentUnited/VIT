%{ (* -*- tuareg -*- *)

open AST

let parse_error pos msg =
  Error.error "Parsing" pos msg

let rec lam pos ps t =
  match ps with
  | [] -> t
  | p :: ps -> Lam (p, Position.with_pos pos (lam pos ps t))

%}

(** Keywords. *)
%token TYPE LET REC IN FUN

(** Punctuation. *)
%token EQUAL RARROW COLON COMMA SEMICOLON DOT

(** Structural symbols. *)
%token LBRACE RBRACE LPAREN RPAREN EOF

(** Identifiers. *)
%token<string> ID TVID

(** Literals. *)
%token<int> INT

%start<AST.program> program

%nonassoc IN COLON
%right RARROW
%left ID INT
%left LPAREN
%nonassoc RPAREN

%%

program: ds=toplevel_definition* EOF
{
  ds
}
| e=located(error)
{
  parse_error (Position.position e) "Syntax error."
}

toplevel_definition:
vd=value_definition
{
  ToplevelValue vd
}
| t=type_introduction
{
  TypeDefinition t
}

type_introduction:
TYPE
  ts=type_parameters
  tycon=located(type_constructor_identifier)
  td=type_definition
{
  (ts, tycon, td)
}

type_parameters:
t=located(type_variable_identifier)
{
  [t]
}
| LPAREN
  ts=separated_nonempty_list(COMMA, located(type_variable_identifier))
  RPAREN
{
  ts
}
| /* empty */
{
  []
}

type_definition:
EQUAL LBRACE fs=separated_nonempty_list(SEMICOLON, field_declaration) RBRACE
{
  RecordType fs
}

field_declaration:
l=located(label) COLON t=located(typ)
{
  (l, t)
}

value_definition:
LET r=REC? x=located(pattern(type_scheme)) 
EQUAL t=located(term)
{
  let ats = if r = None then [] else [Recursive] in
  SimpleValue (x, t, ats)
}
| LET r=REC? x=located(identifier) p=arguments EQUAL t=term
{
  let pos = Position.lex_join $startpos(p) $endpos(t) in
  let f = Position.map (fun x -> PVar (x, None)) x in
  let ats = if r = None then [] else [Recursive] in
  SimpleValue (f, Position.with_pos pos (lam pos p t), ats)
}

arguments: a=argument
{
  [a]
}
| a=argument p=arguments
{
  a :: p
}

term:
vd=value_definition IN t=located(term)
{
  Let (vd, t)
}
| t=located(term) u=located(simple_term)
{
  App (t, u)
}
| FUN a=argument+ RARROW t=term
{
  let pos = Position.lex_join $startpos $endpos(t) in
  lam pos a t
}
| LBRACE
  fs=separated_nonempty_list(SEMICOLON, field_definition)
  RBRACE
{
  Record fs
}
| s=simple_term
{
  s
}

simple_term:
x=identifier
{
  Var x
}
| x=INT
{
  Lit (LInt x)
}
| s=located(simple_term) DOT l=located(label)
{
  Proj (s, l)
}
| LPAREN ts=separated_nonempty_list(COMMA, located(term)) RPAREN
{
  match ts with
  | [] -> assert false (* By grammar. *)
  | [t] -> Position.value t
  | ts -> Tuple ts
}

field_definition: l=located(label) EQUAL t=located(term)
{
  (l, t)
}

typ:
alpha=type_variable_identifier
{
  TyVar alpha
}
| ity=located(typ) RARROW oty=located(typ)
{
  let pos = Position.lex_join $startpos $endpos in
  ProgramManipulation.arrow pos ity oty
}
| tycon=located(type_constructor_identifier)
{
  TyApp (tycon, [])
}
| LPAREN arg=located(typ) RPAREN
  tycon=located(type_constructor_identifier)
{
  TyApp (tycon, [arg])
}
| LPAREN
  arg=located(typ) COMMA args=separated_nonempty_list(COMMA, located(typ)) 
  RPAREN
  tycon=located(type_constructor_identifier)
{
  TyApp (tycon, arg :: args)
}
| LPAREN t=typ RPAREN
{
  t
}

type_scheme:
ty=located(typ)
{
  TypeScheme ([], ty)
}
| ts=nonempty_list(located (type_variable_identifier)) DOT ty=located(typ)
{
  TypeScheme (ts, ty)
}

argument:
x=located(pattern(typ))
{
  x
}

pattern(kind):
x=identifier
{
  PVar (x, None)
}
| x=identifier COLON a=kind
{
  PVar (x, Some a)
}
| LPAREN ps=separated_nonempty_list(COMMA, located(pattern(kind))) RPAREN
{
  PTuple ps
}

identifier: x=ID
{
  Id x
}

label: x=ID
{
  LId x
}

type_constructor_identifier: x=ID
{
  TCId x
}

type_variable_identifier: x=TVID
{
  TVId x
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
