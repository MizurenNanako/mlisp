(** [MachineType] types are only type that have a basic 
      machine representation. These types can be represented 
      directly by machine on some storage material like memory. 
      And thus they can only be constructed by some intrinsic
      constructors, and can only be manipulated with some 
      builtin functions. 

      All [MachineType] types are "constructed" from [T_term],
      and can be "destructed" to [T_term].
  *)
module MachineType = struct
  type t =
    (* err, to introduce type error *)
    | T_err
    (* unit, should equiv to empty list *)
    | T_unit
    (* unconstructed id type *)
    | T_term of string
    (* interger types *)
    | T_i32 of int32
    | T_i64 of int64
    (* floating point types *)
    | T_f64 of float
    (* string *)
    | T_str of string
    (* char *)
    | T_char of char
    (* byte sequence *)
    | T_bytes of bytes
    (* boolean *)
    | T_bool of bool

  (* constructors *)
  let ctor_i32 s =
    match Int32.of_string_opt s with
    | Some i -> T_i32 i
    | None -> T_unit

  let ctor_i64 s =
    match Int64.of_string_opt s with
    | Some i -> T_i64 i
    | None -> T_unit

  let ctor_f64 s =
    match Float.of_string_opt s with
    | Some i -> T_f64 i
    | None -> T_unit

  let ctor_str s = T_str s

  let ctor_char s =
    match String.length s with 1 -> T_char s.[0] | _ -> T_unit

  let ctor_bytes s = T_bytes (String.to_bytes s)

  let ctor_bool = function
    | "true" -> T_bool true
    | "false" -> T_bool false
    | _ -> T_unit
end

(** [Typing.AST] is the true internal high order AST. 
    And since in LISP, type is equiv to quoted list, so AST is type. *)
module AST = struct
  type atom = MachineType.t

  type intrinstc =
    (* from original paper *)
    | I_quote
    | I_atom
    | I_eq
    | I_car
    | I_cdr
    | I_cons
    | I_cond
    | I_lambda
    | I_label
    | I_defun
    (* extended subst, alpha reduction *)
    | I_alpha
    (* extended cond, cond sugar *)
    | I_if
    (* list sugar *)
    | I_list
      (* append takes two lists
         and returns their concatenation *)
    | I_append
    (* pair takes two lists of the same length
       and returns a list of two element lists
       containing successive pairs of
       an element from each *)
    | I_pair
    (* assoc takes an atom x and a list y
       of the form created by pair
       and returns the second element of
       the first list in y whose first element is x *)
    | I_assoc
    (* numeric, supports all types but will check *)
    | I_neg
    | I_add
    | I_sub
    | I_mul
    | I_div
    | I_mod
    | I_round
    | I_floot
    | I_ceil
    | I_pow
    | I_sqrt
    | I_log
    (* boolean and compare *)
    | I_true
    | I_false
    | I_not
    | I_band
    | I_land
    | I_bor
    | I_lor
    | I_xor
    | I_lt
    | I_gt
    | I_leq
    | I_geq
    | I_eeq
    (* machine *)
    | I_shl
    | I_shr
    | I_lshr
    (* machine type constructor
       unit need no constructor, it is empty list *)
    | C_i32
    | C_i64
    | C_f64
    | C_str
    | C_char
    | C_bytes

  (** The ast type, this one won't preserve position information,
      which should be discarded in precession of static type 
      checking and [Syntactis.AST] -> [Typing.AST] translating. 
      
      However, keywords and type construct is done at this part,
      making it possible to implant macro. *)
  type t = Atom of atom | Intrinsic of intrinstc | List of t list
end

let smart_atom name : AST.t =
  match name with
  | "quote" -> Intrinsic I_quote
  | "atom" -> Intrinsic I_atom
  | "eq" -> Intrinsic I_eq
  | "car" -> Intrinsic I_car
  | "cdr" -> Intrinsic I_cdr
  | "cons" -> Intrinsic I_cons
  | "cond" -> Intrinsic I_cond
  | "lambda" -> Intrinsic I_lambda
  | "label" -> Intrinsic I_label
  | "defun" -> Intrinsic I_defun
  | "alpha" -> Intrinsic I_alpha
  | "if" -> Intrinsic I_if
  | "list" -> Intrinsic I_list
  | "append" -> Intrinsic I_append
  | "pair" -> Intrinsic I_pair
  | "assoc" -> Intrinsic I_assoc
  | "neg" -> Intrinsic I_neg
  | "add" -> Intrinsic I_add
  | "sub" -> Intrinsic I_sub
  | "mul" -> Intrinsic I_mul
  | "div" -> Intrinsic I_div
  | "mod" -> Intrinsic I_mod
  | "round" -> Intrinsic I_round
  | "floot" -> Intrinsic I_floot
  | "ceil" -> Intrinsic I_ceil
  | "pow" -> Intrinsic I_pow
  | "sqrt" -> Intrinsic I_sqrt
  | "log" -> Intrinsic I_log
  | "true" -> Intrinsic I_true
  | "false" -> Intrinsic I_false
  | "not" -> Intrinsic I_not
  | "band" -> Intrinsic I_band
  | "land" -> Intrinsic I_land
  | "bor" -> Intrinsic I_bor
  | "lor" -> Intrinsic I_lor
  | "xor" -> Intrinsic I_xor
  | "lt" -> Intrinsic I_lt
  | "gt" -> Intrinsic I_gt
  | "leq" -> Intrinsic I_leq
  | "geq" -> Intrinsic I_geq
  | "eeq" -> Intrinsic I_eeq
  | "shl" -> Intrinsic I_shl
  | "shr" -> Intrinsic I_shr
  | "lshr" -> Intrinsic I_lshr
  | _ as s -> Atom (T_term s)
