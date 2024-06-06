%{
    open Lexical
    open Syntactics.AST
%}

%token <Lexical.Range.t> LP
%token <Lexical.Range.t> RP
%token <string * Lexical.Range.t> Tstring
%token <Lexical.Range.t> QUOTE
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
| r1 = LP; l = sexp* ; r2 = RP { List (l, (Range.join r1 r2)) }



