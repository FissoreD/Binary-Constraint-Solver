open Base

type t = int * int [@@deriving sexp_of, compare, hash]
