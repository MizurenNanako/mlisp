%{
    open Lexical
    open Syntactics.AST
%}

%token <Lexical.Range.t> LP
%token <Lexical.Range.t> RP
%token <string * Lexical.Range.t> Tstring
%token <Lexical.Range.t> QUOTE
%token <bool list * Lexical.Range.t> CXR
%token EOF


%start <t list> program
%%

program:
| l = sexp*; EOF { l }

sexp:
| r = QUOTE; q = sexp { List ([Atom ("quote", r); q], Range.join r (get_rng q)) }
| a = sexp_atom { a }
| l = sexp_list { l }

sexp_atom:
| s = Tstring { Atom (fst s, snd s) }

sexp_list:
| r1 = LP; cxr = CXR; a = sexp; r2 = RP
{
    let l, r = cxr in
    let rng = Range.join r1 r2 in
    let ll = List.fold_left (fun acc b ->
        match b with
        | false -> List ([Atom ("car", r); acc], rng)
        | true -> List ([Atom ("cdr", r); acc], rng)) a l
    in
    ll
}
| r1 = LP; l = sexp* ; r2 = RP { List (l, (Range.join r1 r2)) }



