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
    (* unit *)
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
