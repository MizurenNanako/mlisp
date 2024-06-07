{
    open Lexical.Error
    open Lexical.Token
    open Lexical.Literal
    open Lexical
}

let ws = [' ']
let ns = ['\n']
let reserved = ['(' ')' '\'' ';']
let atom = (_ # ws # ns # reserved)+
let quotemark = "\'"

let zero          = '0'
let identifier    = ['A'-'Z' 'a'-'z' '_' '$']+ ['A'-'Z' 'a'-'z' '0'-'9' '_' '$']*
let oct_char      = ['0'-'7']
let hex_char      = ['0'-'9' 'A'-'F' 'a'-'f']
let numbody       = ['0'-'9' '\'']
let literal_dec   = ['+' '-']? ['1'-'9'] numbody* | zero
let literal_oct   = zero ['0'-'9']+
let literal_hex   = zero ['x' 'X'] hex_char*
let literal_bin   = zero ['b' 'B'] ['0' '1']*
let wholenumber   = ['+' '-']? ['1'-'9'] numbody*
let fraction      = numbody+
let significand   = (wholenumber "." fraction) | ("." fraction) | (wholenumber ".")
let exponent      = ['e' 'E' 'p' 'P'] ['+' '-']? ['0'-'9']+
let literal_real  = (significand exponent? | wholenumber exponent)

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
(* car cdr cadr ... cxxxxxr *)
| 'c' (['a' 'd']+ as s) 'r' 
{
    let ss = String.fold_right (fun c acc -> 
        match c with 
        | 'a' -> false :: acc 
        | 'd' -> true :: acc 
        | _ -> acc) s []
    in
    CXR (ss, Range.of_lexbuf lexbuf)
}
| (literal_real as s) { Treal (float_of_string s, Range.of_lexbuf lexbuf) }
| (literal_dec as s) { Tint ((int_of_dec s).data, Range.of_lexbuf lexbuf) }
| (literal_oct as s) { Tint ((int_of_oct s).data, Range.of_lexbuf lexbuf) }
| (literal_hex as s) { Tint ((int_of_hex s).data, Range.of_lexbuf lexbuf) }
| (literal_bin as s) { Tint ((int_of_bin s).data, Range.of_lexbuf lexbuf) }
| "true" { Tbool (true, Range.of_lexbuf lexbuf) }
| "false" { Tbool (false, Range.of_lexbuf lexbuf) }
| atom as s { Tterm (s, Range.of_lexbuf lexbuf) }
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