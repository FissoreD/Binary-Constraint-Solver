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
  domains : 'a domain DLL.t;
  constraint_binding : ('a domain * 'a domain DLL.t) DLL.t;
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
    domains = DLL.empty "domain list";
    constraint_binding = DLL.empty "constr_binding";
  }

let add_constraint (support : int supports) a b =
  DLL.append (a, b) support.tbl |> ignore;
  DLL.add_if_absent
    (fun e -> e.value == a.dll_father)
    a.dll_father support.domains
  |> ignore;
  DLL.add_if_absent
    (fun e -> e.value == b.dll_father)
    b.dll_father support.domains
  |> ignore;
  DLL.add_assoc a.dll_father b.dll_father support.constraint_binding;
  DLL.add_assoc b.dll_father a.dll_father support.constraint_binding
