module Util = struct
  let report msg rng =
    Printf.eprintf "StructureError: \"%s\" at %s\n" msg
      (Lexical.Range.to_string rng)
end

(** [Env] is the semantic environment *)
module Env = struct
  type t = (string * Typing.AST.t) list

  let find (name : string) t : Typing.AST.t option =
    List.assoc_opt name t

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
    (* raw atom
       raw atom can be check directly,
       for they must be evaluated,
       otherwise will appear in quote.
    *)
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
    | A.List (Atom ("cond", _) :: condpairlist, _) ->
        let checkedlist = check_cond condpairlist env in
        (List (Intrinsic I_cond :: checkedlist), env)
    (* user *)
    | A.List (Atom (name, _) :: arglist, _) ->
        let found =
          match Env.find name env with
          | Some _ -> Atom (T_term name)
          | None -> Atom (T_term name)
        in
        (List (found :: check_list arglist env), env)
    (* list starts with label *)
    (* list starts with lambda list *)
    | A.List
        ( A.List ([ Atom ("lambda", _); List (arglist, _); e ], _)
          :: paramlist,
          range ) ->
        let n, sarglist =
          Syntactics.string_list_of_atom_list arglist
        in
        if n = -1 then (
          Util.report "parameter list of lambda can only be atom"
            range;
          (Atom T_unit, env))
        else
          let m = List.length paramlist in
          if n > m then (
            Util.report
              (Printf.sprintf
                 "this lambda need %i parameters, only %i args \
                  provided"
                 n m)
              range;
            (* failed check, discard. *)
            (Atom T_unit, env))
          else if n < m then (
            Util.report
              (Printf.sprintf
                 "this lambda only need %i parameters, %i args will \
                  be discarded"
                 n (m - n))
              range;
            (* acceptable, discard tail *)
            let sarglist = Common.take_first_n n sarglist in
            check_with_env e
              (List.append
                 (List.combine sarglist (check_list paramlist env))
                 env))
          else
            check_with_env e
              (List.append
                 (List.combine sarglist (check_list paramlist env))
                 env)
    | _ -> (Atom T_unit, env)

  (* at this stage, they are the same. *)
  and check_list arglist env = check_cond arglist env

  and check_cond (raw_ast : A.t list) (env : env) : t list =
    let rec checker acc lst env : t list =
      match lst with
      | [] -> List.rev acc
      | a :: tl ->
          let checked, env = check_with_env a env in
          checker (checked :: acc) tl env
    in
    checker [] raw_ast env

  (** [hold_cnv] just translate A.AST to T.AST, without checking. *)
  and hold_cnv e =
    match e with
    | Atom (name, _) -> Typing.smart_atom name
    | List (lst, _) -> List (List.map hold_cnv lst)

  (** [check_one raw_ast] transform A.AST to T.AST *)
  let check_one (raw_ast : A.t) = check_with_env raw_ast []

  (** [check_program] check list of sexpr with side effect *)
  let check_program (raw_ast_list : A.t list) : t list * env =
    let rec loop acc l e =
      match l with
      | [] -> (List.rev acc, e)
      | hd :: tl ->
          let app, e' = check_with_env hd e in
          loop (app :: acc) tl e'
    in
    loop [] raw_ast_list []
end
