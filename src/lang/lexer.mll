{ (* -*- tuareg -*- *)
  open Parser

  let lexical_error lexbuf msg =
    Error.error "Lexing" (Position.cpos lexbuf) msg
}

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']

let identifier = ['a'-'z''0'-'9''_']+

let uidentifier = ['A'-'Z']['a'-'z''0'-'9''_']*

let number = digit+

rule token = parse
| blank {
  token lexbuf
}
| newline {
  Lexing.new_line lexbuf;
  token lexbuf
}
| "(*" {
  comment 0 lexbuf
}
| ":" {
  COLON
}
| ";" {
  SEMICOLON
}
| "," {
  COMMA
}
| "=" {
  EQUAL
}
| "type" {
  TYPE
}
| "let" {
  LET
}
| "rec" {
  REC
}
| "in" {
  IN
}
| "fun" {
  FUN
}
| "->" {
  RARROW
}
| "." {
  DOT
}
| "'" (identifier as s) {
  TVID s
}
| "{"
{
  LBRACE
}
| "}"
{
  RBRACE
}
| "("
{
  LPAREN
}
| ")"
{
  RPAREN
}
| number as i {
  INT (int_of_string i)
}
| identifier as s {
  ID s
}
| eof {
  EOF
}
| _ as c {
  lexical_error lexbuf (
    Printf.sprintf "Unexpected character `%c'." c
  )
}

and comment level = parse
| "(*"
{
  comment (level + 1) lexbuf
}
| "*)"
{
  if level = 0 then token lexbuf else comment (level - 1) lexbuf
}
| eof
{
  lexical_error lexbuf "Unclosed comment."
}
| _
{
  comment level lexbuf
}
