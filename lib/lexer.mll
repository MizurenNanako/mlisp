{
    open Lexical.Error
    open Lexical.Token
    open Lexical.Literal
    open Lexical

    let lbstk = Stack.create ()
    let lb () = Stack.top lbstk

    let start_p () = (lb ()).Lexing.lex_start_p
    let curr_p () = (lb ()).Lexing.lex_curr_p
    
    let _run fname = 
        let fch = In_channel.open_text fname in
        let lexbuf = Lexing.from_channel fch in
        Lexing.set_filename lexbuf fname;
        Stack.push lexbuf lbstk

    let new_line () = 
        (* let lbb = Stack.pop lbstk in
        Lexing.new_line lbb;
        Stack.push lbb lbstk *)
        Lexing.new_line (lb ())
}

let reserved      = ['|' ':' '[' ']' '(' ')' '{' '}' ' ' '\t' '\n' ',' '?']
let num       = ['0'-'9']
let identifier    = (_ # reserved # num # ['\"']) (_ # reserved)*
let oct_char      = ['0'-'7']
let hex_char      = ['0'-'9' 'A'-'F' 'a'-'f']
(* let numbody       = ['0'-'9'] *)
let numbody       = ['0'-'9' '\'']
let literal_dec   = ['+' '-']? ['1'-'9'] numbody* | '0'
(* let literal_dec   = ['1'-'9'] numbody* *)
let literal_oct   = '0' ['0'-'9']+
let literal_hex   = '0' ['x' 'X'] hex_char*
let literal_bin   = '0' ['b' 'B'] ['0' '1']*
(* let wholenumber   = ['+' '-']? ['1'-'9'] numbody* *)
let wholenumber   = ['+' '-']? numbody+
let fraction      = numbody+
let significand   = (wholenumber "." fraction) | ("." fraction) | (wholenumber ".")
let exponent      = ['e' 'E' 'p' 'P'] ['+' '-']? ['0'-'9']+
let literal_real  = (significand exponent? | wholenumber exponent)

rule get_token = parse
| '#' [^ '\n']* { get_token (lb ()) }
| [' ' '\t']+ { get_token (lb ()) }
| '\n' { Lexing.new_line (lb ()); get_token (lb ()) }
| '`' ([^ '\n']* as fn)
{
    let dirpath = Filename.dirname (lb ()).lex_curr_p.pos_fname in
    _run (dirpath ^ "/" ^ fn); get_token (lb ()) 
}
| "\"" 
{
    let stpos = (lb ()).lex_start_p in
    let s = get_str (Buffer.create 17) (lb ()) in
    let edpos = (lb ()).lex_curr_p in
    let r = stpos, edpos in
    Tstr (s, r)
}
| "|" { Tbar (lb ()).lex_start_p }
| ":" { Tcolon (lb ()).lex_start_p }
(* | ";" { Tsemi (lb ()).lex_start_p } *)
| "[" { Tlb (lb ()).lex_start_p }
| "]" { Trb (lb ()).lex_start_p }
| "(" { Tlp (lb ()).lex_start_p }
| ")" { Trp (lb ()).lex_start_p }
| "{" { Tlc (lb ()).lex_start_p }
| "}" { Trc (lb ()).lex_start_p }
(* | "?" { Tquest (lb ()).lex_start_p } *)
| "," { Tcomma (lb ()).lex_start_p }
| "=" { Teq (lb ()).lex_start_p }
| ":=" { Tbind (lb ()).lex_start_p }
| "->" { Tto (lb ()).lex_start_p }
| "=>" { Tin (lb ()).lex_start_p }
| (literal_real as s) { Tf64 (float_of_string s, Range.of_lexbuf (lb ())) }
| (literal_dec as s) { Ti64 ((int_of_dec s).data, Range.of_lexbuf (lb ())) }
| (literal_oct as s) { Ti64 ((int_of_oct s).data, Range.of_lexbuf (lb ())) }
| (literal_hex as s) { Ti64 ((int_of_hex s).data, Range.of_lexbuf (lb ())) }
| (literal_bin as s) { Ti64 ((int_of_bin s).data, Range.of_lexbuf (lb ())) }
| identifier as id { Tid (id, Range.of_lexbuf (lb ())) }
| eof 
{
    let la = Stack.pop lbstk in
    if Stack.is_empty lbstk then (
        Stack.push la lbstk; (* last lexbuf *)
        Teof
    )
    else get_token (lb ())
}
| _ as c 
{ 
    raise @@ 
    LexicalError (
        Printf.sprintf "unexpected charactor: %c" c, 
        (lb ()).lex_curr_p) 
}

and get_str buf = parse
| '\"' { Buffer.contents buf }
| '\n' { Lexing.new_line (lb ()); Buffer.add_char buf '\n'; get_str buf (lb ()) }
| "\\t" { Buffer.add_char buf '\t'; get_str buf (lb ()) }
| "\\n" { Buffer.add_char buf '\n'; get_str buf (lb ()) }
| "\\\"" { Buffer.add_char buf '\"'; get_str buf (lb ()) }
| eof { raise @@ LexicalError ("unterminated string", (lb ()).lex_curr_p) }
| _ as c { Buffer.add_char buf c; get_str buf (lb ()) }

{
    let init fname = 
        _run fname; 
        fun () -> 
            (
                (* print_int (Stack.length lbstk); *)
                get_token (lb ())
            )
}