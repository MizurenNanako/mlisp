module Util = struct
  let report msg rng =
    Printf.eprintf "StructureError: \"%s\" at %s\n" msg
      (Lexical.Range.to_string rng)
end

(** [Env] is the semantic environment *)
module Env = struct
  type t = (string * Typing.AST.t) list

  let find name t : Typing.AST.t option =
    match List.find_opt (fun p -> fst p = name) t with
    | Some n -> Some (snd n)
    | None -> None

  let insert (name, node) t : t = (name, node) :: t
end

(** [StaticCheck] module implants 
    the static check mechanism adopted from original LISP paper. *)
module StaticCheck = struct
  exception VarNotDefined of string * Lexical.Range.t

  type env = Env.t
  type vartype = Typing.MachineType.t

  module A = Syntactics.AST
  open Typing.AST

  let rec check_with_env (raw_ast : A.t) env : t * env =
    match raw_ast with
    | A.Atom (name, range) -> (
        match Typing.smart_atom name with
        | Atom (T_term s) ->
            let found =
              match Env.find s env with
              | Some v -> v
              | None ->
                  Util.report "VarNotDefined" range;
                  Atom T_err
            in
            (found, env)
        | _ as a -> (a, env))
    (* quote *)
    | A.List ([ Atom ("quote", _); e ], _) ->
        (List [ Intrinsic I_quote; hold_cnv e ], env)
    | A.List (Atom ("quote", _) :: _, range) ->
        Util.report "quote only allow one argument" range;
        (Atom T_err, env)
    (* atom *)
    | A.List ([ Atom ("atom", _); e1 ], _) ->
        let s, env = check_with_env e1 env in
        (List [ Intrinsic I_atom; s ], env)
    | A.List (Atom ("atom", _) :: _, range) ->
        Util.report "atom only allow one argument" range;
        (Atom T_err, env)
    (* eq *)
    | A.List ([ Atom ("eq", _); e1; e2 ], _) ->
        let s1, env = check_with_env e1 env in
        let s2, env = check_with_env e2 env in
        (List [ Intrinsic I_eq; s1; s2 ], env)
    | A.List (Atom ("eq", _) :: _, range) ->
        Util.report "eq only allow two arguments" range;
        (Atom T_err, env)
    (* car *)
    | A.List ([ Atom ("car", _); e1 ], _) ->
        let s, env = check_with_env e1 env in
        (List [ Intrinsic I_car; s ], env)
    | A.List (Atom ("car", _) :: _, range) ->
        Util.report "car only allow one argument" range;
        (Atom T_err, env)
    (* cdr *)
    | A.List ([ Atom ("cdr", _); e1 ], _) ->
        let s, env = check_with_env e1 env in
        (List [ Intrinsic I_cdr; s ], env)
    | A.List (Atom ("cdr", _) :: _, range) ->
        Util.report "cdr only allow one argument" range;
        (Atom T_err, env)
    (* cons *)
    | A.List ([ Atom ("cons", _); e1; e2 ], _) ->
        let s1, env = check_with_env e1 env in
        let s2, env = check_with_env e2 env in
        (List [ Intrinsic I_cons; s1; s2 ], env)
    | A.List (Atom ("cons", _) :: _, range) ->
        Util.report "cons only allow two arguments" range;
        (Atom T_err, env)
    (* cond *)
    | _ -> (Atom T_unit, env)

  and hold_cnv e =
    match e with
    | Atom (name, _) -> Typing.smart_atom name
    | List (lst, _) -> List (List.map hold_cnv lst)

  let check_one (raw_ast : A.t) = check_with_env raw_ast []

  let check_list (raw_ast_list : A.t list) : t list * env =
    let rec loop acc l e =
      match l with
      | [] -> (List.rev acc, e)
      | hd :: tl ->
          let app, e' = check_with_env hd e in
          loop (app :: acc) tl e'
    in
    loop [] raw_ast_list []
end
