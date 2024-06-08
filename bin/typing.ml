(** This module actually implants LISP primitives *)
module Anno = struct
  open Sexplib0.Sexp_conv

  type id = string [@@deriving sexp]

  (** because LISP is a really highly free language
  it should be designed dynamic typed. *)
  type t =
    (* These types are for atom *)
    | Cerr of string
    | Cint of int64
    | Creal of float
    | Cstring of string
    (* pure symbol when held *)
    | Cterm of id
    | Ctrue
    | Clist of t list  (** [Clist []] is nil *)
  [@@deriving sexp]

  let ctrue = Ctrue
  let cfalse = Clist []
  let to_cbool = function true -> ctrue | false -> cfalse

  let cnvparam paramlist =
    List.map (function Cterm id -> id | _ -> "_") paramlist

  type env = (id * t) list

  let assoc name (env : env) =
    match List.assoc_opt name env with
    | Some v -> v
    | None -> Cterm name

  let rec pushenv (idlst : id list) (tlist : t list) (envlst : env) =
    match idlst with
    | [] -> envlst
    | hd :: tl -> (
        match tlist with
        | [] -> (hd, Clist []) :: envlst
        | hd' :: tl' -> pushenv tl tl' ((hd, hd') :: envlst))

  let atom t =
    match t with
    | Ctrue | Clist [] | Cint _ | Creal _ | Cstring _ | Cterm _ ->
        ctrue
    | Cerr _ | Clist _ -> cfalse

  let eq a b =
    match (a, b) with
    | Clist [], Clist [] -> ctrue
    | Cint a, Cint b -> to_cbool (a = b)
    | Creal a, Creal b -> to_cbool (a = b)
    | Cstring a, Cstring b -> to_cbool (a = b)
    | Cterm a, Cterm b -> to_cbool (a = b)
    | _ -> cfalse

  let car = function
    | Clist [] -> Clist []
    | Clist (a :: _) -> a
    | _ -> Cerr "car"

  let cdr = function Clist (_ :: tl) -> Clist tl | _ -> Cerr "cdr"
  let cons x = function Clist l -> Clist (x :: l) | _ -> Cerr "cons"

  let add (acc : t) (n : t) =
    match (acc, n) with
    | Clist [], _ -> n
    | Cint a, Cint b -> Cint Int64.(add a b)
    | Creal a, Creal b -> Creal (a +. b)
    | Cint a, Creal b -> Cint Int64.(add a (Int64.of_float b))
    | Creal a, Cint b -> Creal (a +. Int64.to_float b)
    | Cstring a, Cstring b -> Cstring (a ^ b)
    | Cstring a, Cint b -> Cstring (a ^ Int64.to_string b)
    | Cstring a, Creal b -> Cstring (a ^ string_of_float b)
    | Cstring a, Cterm b -> Cstring (a ^ b)
    | Cstring a, Ctrue -> Cstring (a ^ "true")
    | Cstring a, Clist [] -> Cstring (a ^ "false")
    | _ -> Cerr "add"

  let rec _eval (e : t) (a : env) : t * env =
    match e with
    | (Cerr _ as x)
    | (Cint _ as x)
    | (Creal _ as x)
    | (Cstring _ as x)
    | (Ctrue as x) ->
        (x, a)
    | Cterm id -> (assoc id a, a)
    | Clist l -> (
        match l with
        | [] -> (Clist [], a)
        | Cterm "quote" :: e :: _ -> (e, a)
        | Cterm "atom" :: e :: _ -> (atom (_eval' e a), a)
        | Cterm "eq" :: e1 :: e2 :: _ ->
            (eq (_eval' e1 a) (_eval' e2 a), a)
        | Cterm "car" :: e :: _ -> (car e, a)
        | Cterm "cdr" :: e :: _ -> (cdr e, a)
        | Cterm "cons" :: e1 :: e2 :: _ ->
            (cons (_eval' e1 a) (_eval' e2 a), a)
        | Cterm "cond" :: tl -> (evcon tl a, a)
        (* algebra *)
        | Cterm "add" :: tl ->
            (List.fold_left add (Clist []) (evlis tl a), a)
        (* name, label and lambda *)
        | Cterm name :: tl -> _eval (Clist (assoc name a :: tl)) a
        | Clist (Cterm "label" :: Cterm id :: e :: _) :: tl ->
            _eval
              (Clist (e :: tl))
              ((id, Clist [ Cterm "label"; Cterm id; e ]) :: a)
        | Clist (Cterm "bind" :: Cterm id :: e :: _) :: tl ->
            _eval (Clist tl) ((id, _eval' e a) :: a)
        | Clist (Cterm "lambda" :: Clist param :: e :: _) :: args ->
            _eval e (pushenv (cnvparam param) (evlis args a) a)
        | _ -> (Cerr "eval", a))

  and _eval' e a = fst (_eval e a)

  and evcon c a =
    match c with
    | Clist [ p; v ] :: tl ->
        if _eval' p a = ctrue then v else evcon tl a
    | _ -> Cerr "evcon"

  and evlis m a =
    let eeval e = _eval' e a in
    List.map eeval m

  let mklst l = Clist l
  let eval env cmd = _eval cmd env

  let evalseq env cmds =
    let rec q vlst cmds env =
      match cmds with
      | [] -> List.rev vlst
      | cmd :: tl ->
          let e, a = _eval cmd env in
          q (e :: vlst) tl a
    in
    q [] cmds env
end
