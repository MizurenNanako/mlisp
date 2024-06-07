(** We simply do type check by evaluate the LISP program *)
module Eval = struct
  module A = Syntactics.AST
  open Typing.Anno

  type ty = t

  exception TypeError of Lexical.Range.t

  let report_error msg range =
    Printf.eprintf "TypeError: %s at %s" msg
      (Lexical.Range.to_string range)

  (* let rec _tran (ast:A.t) (env:env) =
     match ast with
     | Atom  -> *)

  (* let transform (ast : A.t) (env : env) : ty = _tran ast env *)
end
