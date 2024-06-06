{
    open Lexical.Error
    open Lexical.Token
    open Lexical
}

let ws = [' ']
let ns = ['\n']
let reserved = ['(' ')' '\'' ';']
let atom = (_ # ws # ns # reserved)+
let quotemark = "\'"

rule get_token = parse
| ';' { skip_line lexbuf; get_token lexbuf }
| ' '+ { get_token lexbuf }
| '\n' { Lexing.new_line lexbuf; get_token lexbuf }
| "(" { LP (Range.of_lexbuf lexbuf) }
| ")" { RP (Range.of_lexbuf lexbuf) }
| quotemark { QUOTE (Range.of_lexbuf lexbuf) }
| eof { EOF }
| '\"' 
{ 
    let p1 = lexbuf.lex_start_p in
    let s = fetch_string (Buffer.create 17) lexbuf in
    let p2 = lexbuf.lex_curr_p in
    Tstring (s, (p1, p2))
}
| atom as s { Tstring (s, Range.of_lexbuf lexbuf) }
| _ as c { raise (LexicalError (Printf.sprintf "unsupported character: %c" c)) }

and skip_line = parse
| '\n' { Lexing.new_line lexbuf }
| eof {  }
| _ { skip_line lexbuf }

and fetch_string buf = parse
| '\"' { Buffer.contents buf }
| '\n' { Lexing.new_line lexbuf; fetch_string buf lexbuf }
| "\\\"" { Buffer.add_char buf '\"'; fetch_string buf lexbuf }
| "\\n" { Buffer.add_char buf '\n'; fetch_string buf lexbuf }
| "\\t" { Buffer.add_char buf '\t'; fetch_string buf lexbuf }
| eof { raise (LexicalError "unexpected eof in string") }
| _ as c { Buffer.add_char buf c; fetch_string buf lexbuf }