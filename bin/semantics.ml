(** We simply do type check by evaluate the LISP program *)
module Eval = struct
  module A = Syntactics.AST
  open Typing.Anno

  type ty = t

  exception StructureError of string * Lexical.Range.t

  let report_error msg range =
    Printf.eprintf "TypeError: %s at %s" msg
      (Lexical.Range.to_string range)

  let rec _tran (ast : A.t) : ty =
    match ast with
    | Atom (a, _) -> _tran_mach a
    | List ([], _) -> Clist []
    | List ([ Atom (Aterm "quote", _); e ], _) ->
        Clist [ Cterm "quote"; _tran_hold e ]
    | List ([ Atom (Aterm "atom", _); e ], _) ->
        Clist [ Cterm "atom"; _tran e ]
    | List ([ Atom (Aterm "eq", _); e1; e2 ], _) ->
        Clist [ Cterm "eq"; _tran e1; _tran e2 ]
    | List ([ Atom (Aterm "car", _); e ], _) ->
        Clist [ Cterm "car"; _tran e ]
    | List ([ Atom (Aterm "cdr", _); e ], _) ->
        Clist [ Cterm "cdr"; _tran e ]
    | List ([ Atom (Aterm "cons", _); e1; e2 ], _) ->
        Clist [ Cterm "cons"; _tran e1; _tran e2 ]
    | List (Atom (Aterm "cond", _) :: tl, _) ->
        Clist (Cterm "cond" :: _tran_cond tl)
    | List (Atom (Aterm name, _) :: tl, _) ->
        Clist (Cterm name :: List.map _tran tl)
    | List (_, range) ->
        raise
        @@ StructureError
             ( "Unknown noncode structure in code, maybe you forgot \
                to quote?",
               range )

  and _tran_cond = function
    | [] -> []
    | List ([ pred; expr ], _) :: tl ->
        Clist [ _tran pred; _tran expr ] :: _tran_cond tl
    | Atom (_, range) :: _ | List (_, range) :: _ ->
        raise
        @@ StructureError
             ( "condition list should have shape (predicate, \
                expression)",
               range )

  and _tran_mach = function
    | Abool true -> ctrue
    | Abool false -> cfalse
    | Aint i -> Cint i
    | Areal a -> Creal a
    | Astring s -> Cstring s
    | Aterm s -> Cterm s

  and _tran_hold = function
    | Atom (a, _) -> _tran_mach a
    | List (l, _) -> Clist (List.map _tran_hold l)

  let transform (ast : A.t) : ty = _tran ast
end
