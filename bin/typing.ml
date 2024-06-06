(** [Typing.AST] is the true internal high order AST. 
    And since in LISP, type is equiv to quoted list, so AST is type. *)
module AST = struct
  type atom = Native.MachineType.t

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
    (* extended subst *)
    | I_alpha (* alpha reduction*)
    (* extended cond *)
    | I_if (* cond sugar *)
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
    (* boolean and compare *)
    | I_true
    | I_false
    | I_not
    | I_band
    | I_land
    | I_bor
    | I_lor
    | I_xor
    (* machine *)
    | I_shl
    | I_shr
    | I_lshr

  (** The ast type, this one won't preserve position information,
      which should be discarded in precession of static type 
      checking and [Syntactis.AST] -> [Typing.AST] translating. *)
  type t = Atom of atom | Intrinsic of intrinstc | List of t list
end
