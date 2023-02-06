open Base

module AC_4 = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a double_connection = {
    node : 'a Graph.value;
    mutable assoc : 'a double_connection DLL.node option;
  }

  type 'a remove_in_domain = string Graph.value list

  type 'a cell_type =
    ( 'a Graph.domain,
      'a double_connection DLL.t,
      Graph.Domain.comparator_witness )
    Map.t

  type 'a data_struct =
    ('a Graph.value, 'a cell_type, Graph.Value.comparator_witness) Map.t

  type 'a stack_operation = 'a double_connection DLL.node list

  let name = "AC-4"
  let loop_into_map f m = Map.iter m ~f:(fun v -> DLL.iter_value f v)

  let print_data_struct (c : 'a data_struct) =
    Map.iteri c ~f:(fun ~key ~data ->
        Stdio.printf "%s is supported by " (Arc_consistency.make_name key);
        loop_into_map
          (fun e -> Stdio.printf "%s " (Arc_consistency.make_name e.node))
          data;
        Stdio.print_endline "")

  let initialization ?(print = false) (graph : 'a Graph.graph) =
    let graph = Arc_consistency.clean_domains ~print graph in
    let data_struct : 'a data_struct ref =
      ref (Map.empty (module Graph.Value))
    in
    Graph.loop_domains
      (fun d ->
        DLL.iter
          (fun v ->
            let tbl = ref (Map.empty (module Graph.Domain)) in
            DLL.iter_value
              (fun (dom : 'a DLL.t) ->
                tbl := Map.add_exn !tbl ~key:dom ~data:(DLL.empty ""))
              (Graph.get_constraint_binding graph d);
            data_struct := Map.add_exn !data_struct ~key:v ~data:!tbl)
          d)
      graph;
    let find_pair (elt : 'a Graph.value) (e2 : 'a Graph.value) =
      let supp = Map.find_exn !data_struct elt in
      Map.find_exn supp e2.father
    in
    Graph.loop_domains
      (fun d1 ->
        DLL.iter
          (fun v1 ->
            DLL.iter_value
              (DLL.iter (fun v2 ->
                   if Graph.relation graph v1 v2 then
                     let x, y = (find_pair v1 v2, find_pair v2 v1) in
                     if DLL.not_exist_by_value (fun e -> phys_equal e.node v2) x
                     then (
                       DLL.append { node = v2; assoc = None } x;
                       DLL.append { node = v1; assoc = None } y;
                       (DLL.get_last x).value.assoc <- Some (DLL.get_last y);
                       (DLL.get_last y).value.assoc <- Some (DLL.get_last x))))
              (Graph.get_constraint_binding graph d1))
          d1)
      graph;
    !data_struct

  let revise (input : 'a Graph.value) (data_struct : 'a data_struct) =
    let supported = Map.find_exn data_struct input in
    let to_remove_in_domain = ref [] in
    let removed_in_support = ref [] in
    loop_into_map
      (fun ({ node; assoc } : 'a double_connection) ->
        let sibling = Option.value_exn assoc in
        DLL.remove sibling;
        removed_in_support := sibling :: !removed_in_support;
        if DLL.is_empty sibling.father then
          to_remove_in_domain := node :: !to_remove_in_domain)
      supported;
    (!removed_in_support, !to_remove_in_domain)

  let back_track removed_in_suport = List.iter ~f:DLL.insert removed_in_suport
end

include AC_4