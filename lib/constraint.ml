(** 
  Constraints are represented by a dll D of pair of nodes. 
  Two values v1, v2 support each other if ther is a pair 
  (a,b) or (b,a) in D.
*)

module DLL = DoublyLinkedList

type 'a cell_type = 'a DLL.dll_node * 'a DLL.dll_node
type 'a relation = 'a DLL.dll_node -> 'a DLL.dll_node -> bool
type 'a table_type = 'a cell_type DLL.t
type 'a domain = 'a DLL.t

type 'a supports = {
  tbl : 'a table_type;
  relation : 'a relation;
  constraint_binding : (string, 'a domain DLL.t) Hashtbl.t;
  domains : (string, 'a DLL.t) Hashtbl.t;
}

let find_relation tbl (a : 'a DLL.dll_node) (b : 'a DLL.dll_node) =
  let has n1 n2 = a == n1 && b == n2 in
  let find_fun (n1, n2) = has n1 n2 || has n2 n1 in
  if DLL.is_empty tbl then false else DLL.exsist (fun a -> find_fun a.value) tbl

let build_constraint () : 'a supports =
  let tbl = DLL.empty "constr" in
  {
    tbl;
    relation = find_relation tbl;
    constraint_binding = Hashtbl.create 1024;
    domains = Hashtbl.create 1024;
  }

let add_constraint (support : 'a supports) d1 v1 d2 v2 =
  let get d v = Option.get (DLL.find_by_value v d) in
  let a, b = (get d1 v1, get d2 v2) in
  DLL.append (a, b) support.tbl |> ignore;
  (match Hashtbl.find_opt support.constraint_binding d1.name with
  | None ->
      Printf.printf "Adding %s to %s\n" d1.name d2.name;
      Hashtbl.add support.constraint_binding d1.name (DLL.singleton "" d2);
      Hashtbl.add support.domains d1.name d1
  | Some e -> DLL.add_if_absent (fun e -> e.value == d2) d2 e |> ignore);
  match Hashtbl.find_opt support.constraint_binding d2.name with
  | None ->
      Printf.printf "Adding %s to %s\n" d2.name d1.name;
      Hashtbl.add support.constraint_binding d2.name (DLL.singleton "" d1);
      Hashtbl.add support.domains d2.name d2
  | Some e -> DLL.add_if_absent (fun e -> e.value == d1) d1 e |> ignore

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
  print_endline "--- End Domains --"

let print_string_domains = print_domains print_string_domain
