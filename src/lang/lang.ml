open Options
open Printer

let configuration =
  Error.resume_on_error ()

let parse content =
  let lexbuf = Lexing.from_string content in
  Parser.program Lexer.token lexbuf

let elaborate iast =
  Elaboration.process "buffer" iast

let print ast =
  Printer.(string_of (program false) ast)
