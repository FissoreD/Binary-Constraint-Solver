(** 
  Constraints are represented by a dll D of pair of nodes. 
  Two values v1, v2 support each other if ther is a pair 
  (a,b) or (b,a) in D.
*)

module DLL = DoublyLinkedList

type 'a cell_type = 'a DLL.dll_node * 'a DLL.dll_node
type 'a relation = 'a DLL.dll_node -> 'a DLL.dll_node -> bool
type 'a table_type = 'a cell_type DLL.t
type 'a supports = { tbl : 'a table_type; relation : 'a relation }

let find_relation tbl (a : 'a DLL.dll_node) (b : 'a DLL.dll_node) =
  let has n1 n2 = a == n1 && b == n2 in
  let find_fun (n1, n2) = has n1 n2 || has n2 n1 in
  if DLL.is_empty tbl then false else DLL.exsist (fun a -> find_fun a.value) tbl

let build_constraint () : 'a supports =
  let tbl = ref None in
  { tbl; relation = find_relation tbl }

let add_constraint { tbl; _ } a b = DLL.append (a, b) tbl
