(** This module actually implants LISP primitives *)
module Anno = struct
  type id = string

  (** because LISP is a really highly free language
  it should be designed dynamic typed. *)
  type t =
    (* These types are for atom *)
    | Cerr
    | Cint of int64
    | Creal of float
    | Cstring of string
    (* unlike other dialects, here is boolean *)
    | Ctrue
    | Cfalse
    (* pure symbol when held *)
    | Cterm of id
    (* for lambda, store parameter names and body-expr *)
    | Clambda of id list * t
    | Clist of t list  (** [Clist []] is nil *)

  let to_cbool = function true -> Ctrue | false -> Cfalse

  let cnvparam paramlist =
    List.map (function Cterm id -> id | _ -> "_") paramlist

  type env = (id * t) list

  let assoc name (env : env) =
    match List.assoc_opt name env with Some v -> v | None -> Cerr

  let rec pushenv (idlst : id list) (tlist : t list) (envlst : env) =
    match idlst with
    | [] -> envlst
    | hd :: tl -> (
        match tlist with
        | [] -> (hd, Clist []) :: envlst
        | hd' :: tl' -> pushenv tl tl' ((hd, hd') :: envlst))

  let atom t =
    match t with
    | Clist []
    | Ctrue | Cfalse | Cint _ | Creal _ | Cstring _ | Cterm _ ->
        Ctrue
    | Clambda _ | Cerr | Clist _ -> Cfalse

  let eq a b =
    match (a, b) with
    | Clist [], Clist [] -> Ctrue
    | Ctrue, Ctrue -> Ctrue
    | Cfalse, Ctrue -> Cfalse
    | Ctrue, Cfalse -> Cfalse
    | Cfalse, Cfalse -> Ctrue
    | Cint a, Cint b -> to_cbool (a = b)
    | Creal a, Creal b -> to_cbool (a = b)
    | Cstring a, Cstring b -> to_cbool (a = b)
    | Cterm a, Cterm b -> to_cbool (a = b)
    | _ -> Cfalse

  let car = function
    | Clist [] -> Clist []
    | Clist (a :: _) -> a
    | _ -> Cerr

  let cdr = function Clist (_ :: tl) -> Clist tl | _ -> Cerr
  let cons x = function Clist l -> Clist (x :: l) | _ -> Cerr

  let add (n : t) (acc : t) =
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
    | Cstring a, Cfalse -> Cstring (a ^ "false")
    | _ -> Cerr

  let rec eval (e : t) (a : env) : t =
    match e with
    | (Cerr as x)
    | (Cint _ as x)
    | (Creal _ as x)
    | (Cstring _ as x)
    | (Cfalse as x)
    | (Ctrue as x)
    | (Clambda _ as x) ->
        x
    | Cterm id -> assoc id a
    | Clist l -> (
        match l with
        | [] -> Clist []
        | Cterm "quote" :: e :: _ -> e
        | Cterm "atom" :: e :: _ -> atom (eval e a)
        | Cterm "eq" :: e1 :: e2 :: _ -> eq e1 e2
        | Cterm "car" :: e :: _ -> car e
        | Cterm "cdr" :: e :: _ -> cdr e
        | Cterm "cons" :: e1 :: e2 :: _ -> cons e1 e2
        | Cterm "cond" :: tl -> evcon tl a
        (* algebra *)
        | Cterm "add" :: tl ->
            List.fold_right add (evlis tl a) (Clist [])
        (* name, label and lambda *)
        | Cterm name :: tl -> eval (Clist (assoc name a :: tl)) a
        | Clist (Cterm "label" :: Cterm id :: e :: _) :: tl ->
            eval
              (Clist (e :: tl))
              ((id, Clist [ Cterm "label"; Cterm id; e ]) :: a)
        | Clist (Cterm "bind" :: Cterm id :: e :: _) :: tl ->
            eval (Clist tl) ((id, eval e a) :: a)
        | Clist (Cterm "lambda" :: Clist param :: e :: _) :: args ->
            eval e (pushenv (cnvparam param) (evlis args a) a)
        | _ -> Cerr)

  and evcon c a =
    match c with
    | Clist [ p; v ] :: tl ->
        if eval p a = Ctrue then v else evcon tl a
    | _ -> Cerr

  and evlis m a =
    let eeval e = eval e a in
    List.map eeval m
end
