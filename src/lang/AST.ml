open Position

type program = toplevel_definition list

and toplevel_definition =
  | ToplevelValue of value_definition
  | TypeDefinition of type_introduction

and type_introduction =
  type_variable_identifier located list
  * type_constructor_identifier located
  * type_definition

and type_definition =
  | RecordType of (label located * ty located) list
  | Abstract

and value_definition =
  | SimpleValue of
        polymorphic_pattern located
      * term located
      * attribute list

and attribute = Recursive

and signature = declaration list

and declaration =
  | DeclareValue of identifier * type_scheme * attribute list
  | DeclareType  of type_introduction

and term =
  | Var     of identifier
  | Lit     of literal
  | Let     of value_definition * term located
  | Lam     of monomorphic_pattern located * term located
  | App     of term located * term located
  | Proj    of term located * label located
  | Record  of (label located * term located) list
  | Tuple   of term located list

and 'kind pattern =
  | PVar   of identifier * 'kind option
  | PTuple of 'kind pattern located list

and 'kind kind_descriptor =
  | Monomorphic : ty kind_descriptor
  | Polymorphic : type_scheme kind_descriptor

and monomorphic_pattern = ty pattern

and polymorphic_pattern = type_scheme pattern

and literal =
  | LInt of int

and ty =
  | TyVar of type_variable_identifier
  | TyApp of type_constructor_identifier located * ty located list

and type_scheme =
  | TypeScheme of type_variable_identifier located list * ty located

and identifier = Id of string

and type_variable_identifier = TVId of string

and type_constructor_identifier = TCId of string

and label = LId of string
