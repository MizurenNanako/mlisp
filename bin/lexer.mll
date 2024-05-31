{
    open Lexical.Error
    open Lexical.Token
}

let ws = [' ']
let ns = ['\n']
let reserved = ['(' ')' '\'']
let atom = (_ # ws # ns # reserved)+
let quotemark = "\'"

rule get_token = parse
| ';' { skip_line lexbuf; get_token lexbuf }
| ' '+ { get_token lexbuf }
| '\n' { Lexing.new_line lexbuf; get_token lexbuf }
| "(" { LP }
| ")" { RP }
| quotemark { QUOTE }
| eof { EOF }
| '\"' { fetch_string (Buffer.create 17) lexbuf }
| atom as s { Tstring s }
| _ as c { raise (LexicalError (Printf.sprintf "unsupported character: %c" c)) }

and skip_line = parse
| '\n' { Lexing.new_line lexbuf }
| eof {  }
| _ { skip_line lexbuf }

and fetch_string buf = parse
| '\"' { Tstring (Buffer.contents buf) }
| '\n' { Lexing.new_line lexbuf; fetch_string buf lexbuf }
| "\\\"" { Buffer.add_char buf '\"'; fetch_string buf lexbuf }
| "\\n" { Buffer.add_char buf '\n'; fetch_string buf lexbuf }
| "\\t" { Buffer.add_char buf '\t'; fetch_string buf lexbuf }
| eof { raise (LexicalError "unexpected eof in string") }
| _ as c { Buffer.add_char buf c; fetch_string buf lexbuf }