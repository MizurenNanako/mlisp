%{
    open Syntactics.AST
%}

%token LP
%token RP
%token <string> Tstring
%token QUOTE
%token EOF


%start <t list> program
%%

program:
| l = sexp*; EOF { l }

sexp:
| QUOTE; q = sexp { List [Atom "quote"; q] }
| a = sexp_atom { a }
| l = sexp_list { l }

sexp_atom:
| s = Tstring { Atom s }

sexp_list:
| LP; l = sexp* ; RP { List l }


