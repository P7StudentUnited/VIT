(** API to the VIT language subsystem. *)

val parse : string -> AST.program
(** [parse content] turns a string into an abstract syntax tree if it
    is syntactically correct. *)

val print : AST.program -> string
(** [print p] represents the abstract syntax tree [p] is a human readable
    form. *)

val elaborate : AST.program -> AST.program
(** [elaborate p] translate the implicitly typed program [p] into an
    explicitly typed program if [p] is well-typed. *)
