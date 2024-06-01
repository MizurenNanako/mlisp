# Minimal LISP Implantation with OCaml

This project illustrates a minimal implantation of McCarthy's original LISP with OCaml.

It supports comment, and atom which including spaces quoted with double quotes, and the seven basic operations included in original LISP.

It will support native code generation and some m-expr syntax in the future. It's semantic checks are performed after the syntactic parsing, by trying to deduce he types of all atoms. Though LISP don't have a type system, it o specified some operations to be undefined, so the type checking system basically checks the operations are vaild or not, via some kind of evaluation.
