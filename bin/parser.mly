%{
    open Lexical
    open Syntactics.AST
%}

%token <Lexical.Range.t> LP
%token <Lexical.Range.t> RP
%token <Lexical.Range.t> QUOTE
%token <string * Lexical.Range.t> Tterm
%token <string * Lexical.Range.t> Tstring
%token <int64 * Lexical.Range.t> Tint
%token <float * Lexical.Range.t> Treal
%token <bool * Lexical.Range.t> Tbool
%token <bool list * Lexical.Range.t> CXR

%token EOF

%start <t list> program
%%

program:
| l = sexp*; EOF { l }

sexp:
| r = QUOTE; q = sexp { List ([Atom (Aterm "quote", r); q], Range.join r (get_rng q)) }
| a = sexp_atom { a }
| l = sexp_list { l }

sexp_atom:
| s = Tstring { let a, r = s in Atom (Astring a, r) }
| s = Tbool { let a, r = s in Atom (Abool a, r) }
| s = Tint { let a, r = s in Atom (Aint a, r) }
| s = Treal { let a, r = s in Atom (Areal a, r) }
| s = Tterm { let a, r = s in Atom (Aterm a, r) }

sexp_list:
| r1 = LP; cxr = CXR; a = sexp; r2 = RP
{
    let l, r = cxr in
    let rng = Range.join r1 r2 in
    let ll = List.fold_left (fun acc b ->
        match b with
        | false -> List ([Atom (Aterm "car", r); acc], rng)
        | true -> List ([Atom (Aterm "cdr", r); acc], rng)) a l
    in
    ll
}
| r1 = LP; l = sexp* ; r2 = RP { List (l, (Range.join r1 r2)) }



