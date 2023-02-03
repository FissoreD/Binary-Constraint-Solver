open Base
module DLL = DoublyLinkedList

module Tuple = struct
  type t = int * int [@@deriving sexp_of, compare, hash]
end

type 'a relation = 'a DLL.dll_node -> 'a DLL.dll_node -> bool

(* module StrMap = Map.Make (String) *)

type 'a table_type = (int * int, unit) Hashtbl.t
type 'a domain = 'a DLL.t

type 'a graph = {
  tbl : 'a table_type;
  relation : 'a relation;
  (* A dict from a domain name to all domains depending on it *)
  constraint_binding : (string, 'a domain DLL.t) Hashtbl.t;
  domains : (string, 'a DLL.t) Hashtbl.t;
}

let find_relation tbl (a : 'a DLL.dll_node) (b : 'a DLL.dll_node) =
  Hashtbl.mem tbl (a.id, b.id) || Hashtbl.mem tbl (b.id, a.id)

let build_constraint () : 'a graph =
  let tbl = Hashtbl.create (module Tuple) in
  {
    tbl;
    relation = find_relation tbl;
    constraint_binding = Hashtbl.create (module String);
    domains = Hashtbl.create (module String);
  }

let add_constraint (support : 'a graph) d1 v1 d2 v2 =
  let add_if_absent (d1 : 'a domain) (d2 : 'a domain) =
    match Hashtbl.find support.constraint_binding d1.name with
    | None ->
        Hashtbl.add support.constraint_binding ~key:d1.name
          ~data:(DLL.singleton "" d2)
        |> ignore;
        Hashtbl.add support.domains ~key:d1.name ~data:d1 |> ignore
    | Some e -> DLL.add_if_absent (fun e -> phys_equal e.value d2) d2 e
  in
  let get d v = DLL.find_by_value v d in
  let a, b = (get d1 v1, get d2 v2) in
  Hashtbl.add support.tbl ~key:(a.id, b.id) ~data:() |> ignore;
  add_if_absent d1 d2;
  add_if_absent d2 d1

let get_domain graph name =
  match Hashtbl.find graph.domains name with
  | None -> invalid_arg (Printf.sprintf "The domain %s not exists" name)
  | Some e -> e

let get_constraint_binding graph (domain : 'a DLL.t) =
  match Hashtbl.find graph.constraint_binding domain.name with
  | None -> invalid_arg (Printf.sprintf "The domain %s not exists" domain.name)
  | Some e -> e

let print_string_domain ?(is_rev = false) (d : string domain) =
  Stdio.printf "%s : " d.name;
  (if is_rev then DLL.iter_rev else DLL.iter)
    (fun { value; _ } -> Stdio.printf "%s;" value)
    d;
  Stdio.print_endline ""

let print_domains f { domains; _ } =
  Stdio.print_endline "-- Start Domains --";
  Hashtbl.iter domains ~f;
  Stdio.print_endline "--- End Domains ---"

let print_string_domains ?(is_rev = false) =
  print_domains (print_string_domain ~is_rev)

let loop_domains f g = Hashtbl.iter g.domains ~f
let list_domains g = Hashtbl.data g.domains
let relation g a b = g.relation a b
