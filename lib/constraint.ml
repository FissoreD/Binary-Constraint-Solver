(** 
  Constraints are represented by a dll D of pair of nodes. 
  Two values v1, v2 support each other if ther is a pair 
  (a,b) or (b,a) in D.
*)

module DLL = DoublyLinkedList

type 'a cell_type = 'a DLL.dll_node * 'a DLL.dll_node
type 'a relation = 'a DLL.dll_node -> 'a DLL.dll_node -> bool

type 'a table_type =
  ( string * string * string * string,
    'a DLL.dll_node * 'a DLL.dll_node )
  Hashtbl.t

type 'a domain = 'a DLL.t

type 'a graph = {
  tbl : 'a table_type;
  relation : 'a relation;
  (* A dict from a domain name to all domains depending on it *)
  constraint_binding : (string, 'a domain DLL.t) Hashtbl.t;
  domains : (string, 'a DLL.t) Hashtbl.t;
}

let make_tuple (a : 'a DLL.dll_node) (b : 'a DLL.dll_node) =
  (a.value, a.dll_father.name, b.value, b.dll_father.name)

let find_relation tbl (a : 'a DLL.dll_node) (b : 'a DLL.dll_node) =
  Hashtbl.mem tbl (make_tuple a b) || Hashtbl.mem tbl (make_tuple b a)

let build_constraint () : 'a graph =
  let tbl = Hashtbl.create 1024 in
  {
    tbl;
    relation = find_relation tbl;
    constraint_binding = Hashtbl.create 1024;
    domains = Hashtbl.create 1024;
  }

let add_constraint (support : 'a graph) d1 v1 d2 v2 =
  let add_if_absent (d1 : 'a domain) (d2 : 'a domain) =
    match Hashtbl.find_opt support.constraint_binding d1.name with
    | None ->
        Hashtbl.add support.constraint_binding d1.name (DLL.singleton "" d2);
        Hashtbl.add support.domains d1.name d1
    | Some e -> DLL.add_if_absent (fun e -> e.value == d2) d2 e |> ignore
  in
  let get d v = Option.get (DLL.find_by_value v d) in
  let a, b = (get d1 v1, get d2 v2) in
  Hashtbl.add support.tbl (make_tuple a b) (a, b);
  add_if_absent d1 d2;
  add_if_absent d2 d1

let get_domain graph name =
  match Hashtbl.find_opt graph.domains name with
  | None -> invalid_arg (Printf.sprintf "The domain %s not exists" name)
  | Some e -> e

let get_constraint_binding graph (domain : 'a DLL.t) =
  match Hashtbl.find_opt graph.constraint_binding domain.name with
  | None -> invalid_arg (Printf.sprintf "The domain %s not exists" domain.name)
  | Some e -> e

let print_string_domain ?(is_rev = false) (d : string domain) =
  Printf.printf "%s : " d.name;
  (if is_rev then DLL.iter_rev else DLL.iter)
    (fun { value; _ } -> Printf.printf "%s;" value)
    d;
  print_newline ()

let print_domains f { domains; _ } =
  print_endline "-- Start Domains --";
  Hashtbl.iter (fun _ v -> f v) domains;
  print_endline "--- End Domains ---"

let print_string_domains ?(is_rev = false) =
  print_domains (print_string_domain ~is_rev)
