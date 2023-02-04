open Base
module DLL = DoublyLinkedList

type 'a relation = 'a DLL.node -> 'a DLL.node -> bool
type 'a table_type = (int * int) Hash_set.t
type 'a domain = 'a DLL.t

type 'a graph = {
  tbl : 'a table_type;
  relation : 'a relation;
  constraint_binding : (int, 'a domain DLL.t) Hashtbl.t;
  domains : (string, 'a DLL.t) Hashtbl.t;
}

let find_relation tbl (a : 'a DLL.node) (b : 'a DLL.node) =
  Hash_set.mem tbl (a.id, b.id) || Hash_set.mem tbl (b.id, a.id)

let build_graph () : 'a graph =
  let tbl = Hash_set.create (module Tuple) in
  {
    tbl;
    relation = find_relation tbl;
    constraint_binding = Hashtbl.create (module Int);
    domains = Hashtbl.create (module String);
  }

let add_constraint (graph : 'a graph) d1 v1 d2 v2 =
  let add_if_absent (d1 : 'a domain) (d2 : 'a domain) =
    let dom =
      Hashtbl.find_or_add graph.constraint_binding d1.id_dom ~default:(fun _ ->
          Hashtbl.add_exn graph.domains ~key:d1.name ~data:d1;
          DLL.empty "")
    in
    DLL.add_if_absent (fun e -> phys_equal e.value d2) d2 dom
  in
  let get d v = DLL.find_by_value v d in
  let a, b = (get d1 v1, get d2 v2) in
  Hash_set.add graph.tbl (a.id, b.id);
  add_if_absent d1 d2;
  add_if_absent d2 d1

let get_domain graph name =
  match Hashtbl.find graph.domains name with
  | None -> invalid_arg (Printf.sprintf "The domain %s not exists" name)
  | Some e -> e

let get_constraint_binding graph (domain : 'a DLL.t) =
  match Hashtbl.find graph.constraint_binding domain.id_dom with
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
