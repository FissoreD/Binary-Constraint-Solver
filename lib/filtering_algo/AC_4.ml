module AC_4 : Arc_consistency.Arc_consistency = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList
  module MapInt = Map.Make (Int)
  module MapStr = Map.Make (String)

  type 'a double_connection = {
    node : 'a DLL.dll_node;
    mutable assoc : 'a double_connection DLL.dll_node option;
  }

  type 'a remove_in_domain = string DLL.dll_node list
  type 'a cell_type = 'a double_connection DLL.t MapStr.t

  type 'a data_struct = ('a DLL.dll_node * 'a cell_type) MapInt.t
  (** For each constraint and each value there is a linked list of supports  *)

  type 'a stack_operation = {
    to_remove_in_domain : 'a remove_in_domain;
    to_remove_sibling : 'a double_connection DLL.dll_node list;
    input : 'a DLL.dll_node;
  }

  let loop_into_map f m = MapStr.iter (fun _ v -> DLL.iter_value f v) m

  let print_data_struct (c : 'a data_struct) =
    MapInt.iter
      (fun _ ((a, b) : 'a DLL.dll_node * 'a cell_type) ->
        Printf.printf "%s is supported by " (Arc_consistency.make_name a);
        loop_into_map
          (fun e -> Printf.printf "%s " (Arc_consistency.make_name e.node))
          b;
        print_newline ())
      c

  let initialization ?(print = false) (graph : 'a Constraint.graph) =
    let graph = Arc_consistency.clean_domains ~print graph in
    let data_struct : 'a data_struct ref = ref MapInt.empty in
    Constraint.loop_domains
      (fun d ->
        DLL.iter
          (fun v ->
            let tbl = ref MapStr.empty in
            DLL.iter_value
              (fun (n : 'a DLL.t) ->
                tbl := MapStr.add n.name (DLL.empty "") !tbl)
              (Constraint.get_constraint_binding graph d);
            data_struct := MapInt.add v.id (v, !tbl) !data_struct)
          d)
      graph;
    let find_pair (elt : 'a DLL.dll_node) (e2 : 'a DLL.dll_node) =
      let _, supp = MapInt.find elt.id !data_struct in
      MapStr.find e2.dll_father.name supp
    in
    Hashtbl.iter
      (fun _ ((a : string DLL.dll_node), (b : string DLL.dll_node)) ->
        if a.is_in && b.is_in then (
          let x, y = (find_pair a b, find_pair b a) in
          DLL.append { node = b; assoc = None } x;
          DLL.append { node = a; assoc = None } y;
          (DLL.get_last x).value.assoc <- Some (DLL.get_last y);
          (DLL.get_last y).value.assoc <- Some (DLL.get_last x)))
      graph.tbl;
    !data_struct

  let revise (input : 'a DLL.dll_node) (data_struct : 'a data_struct) =
    let _, supported = MapInt.find input.id data_struct in
    let to_remove_in_domain = ref [] in
    let to_remove_sibling = ref [] in
    loop_into_map
      (fun ({ node; assoc } : 'a double_connection) ->
        let sibling = Option.get assoc in
        DLL.remove sibling;
        to_remove_sibling := sibling :: !to_remove_sibling;
        if DLL.is_empty sibling.dll_father then
          to_remove_in_domain := node :: !to_remove_in_domain)
      supported;
    {
      to_remove_in_domain = !to_remove_in_domain;
      to_remove_sibling = !to_remove_sibling;
      input;
    }

  let back_track { to_remove_sibling; input; _ } =
    List.iter DLL.insert to_remove_sibling;
    DLL.insert input

  let get_to_remove { to_remove_in_domain; _ } = to_remove_in_domain
end

include AC_4