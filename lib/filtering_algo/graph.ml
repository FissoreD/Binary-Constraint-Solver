open Base
module DLL = DoublyLinkedList

type 'a domain = 'a DLL.t
type 'a value = 'a DLL.node
type 'a relation = 'a value -> 'a value -> bool
type 'a table_type = (int * int) Hash_set.t

type 'a graph = {
  tbl : 'a table_type;
  relation : 'a relation;
  constraint_binding : ('a domain, 'a domain DLL.t) Hashtbl.t;
  domains : 'a domain Hash_set.t;
}

module Value = struct
  type t = string value

  let compare (x : t) (y : t) = compare x.id y.id
  let sexp_of_t (x : t) = sexp_of_int x.id
  let hash (x : t) = x.id
end

module ValuePair = struct
  type t = string value * string domain

  let compare ((a, b) : t) ((c, d) : t) =
    match compare a.id c.id with 0 -> compare b.id_dom d.id_dom | e -> e

  let sexp_of_t (x : t) = sexp_of_int (((fst x).id * 1001) + (snd x).id_dom)
  let hash (x : t) = hash_int (((fst x).id * 1001) + (snd x).id_dom)
end

module Domain = struct
  type t = string domain

  let compare (x : t) (y : t) = compare x.id_dom y.id_dom
  let sexp_of_t (x : t) = sexp_of_int x.id_dom
  let hash (x : t) = x.id_dom
end

let find_relation tbl (a : 'a value) (b : 'a value) =
  Hash_set.mem tbl (a.id, b.id) || Hash_set.mem tbl (b.id, a.id)

let build_graph () : 'a graph =
  let tbl = Hash_set.create (module Tuple) in
  {
    tbl;
    relation = find_relation tbl;
    constraint_binding = Hashtbl.create (module Domain);
    domains = Hash_set.create (module Domain);
  }

let add_constraint (graph : 'a graph) d1 v1 d2 v2 =
  let add_if_absent (d1 : 'a domain) (d2 : 'a domain) =
    let dom =
      Hashtbl.find_or_add graph.constraint_binding d1 ~default:(fun _ ->
          Hash_set.add graph.domains d1;
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
  match
    Hash_set.find graph.domains ~f:(fun (e : 'a domain) ->
        String.equal e.name name)
  with
  | None -> invalid_arg (Printf.sprintf "The domain %s not exists" name)
  | Some e -> e

let get_constraint_binding graph (domain : 'a domain) =
  match Hashtbl.find graph.constraint_binding domain with
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
  Hash_set.iter domains ~f;
  Stdio.print_endline "--- End Domains ---"

let print_string_domains ?(is_rev = false) =
  print_domains (print_string_domain ~is_rev)

let loop_domains f g = Hash_set.iter g.domains ~f
let list_domains g = Hash_set.to_list g.domains
let relation g a b = g.relation a b
