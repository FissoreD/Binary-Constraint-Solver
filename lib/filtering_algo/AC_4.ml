open Base

module AC_4 = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a double_connection = {
    node : 'a DLL.node;
    mutable assoc : 'a double_connection DLL.node option;
  }

  type 'a remove_in_domain = string DLL.node list

  type 'a cell_type =
    (int, 'a double_connection DLL.t, Int.comparator_witness) Map.t

  type 'a data_struct =
    (int, 'a DLL.node * 'a cell_type, Int.comparator_witness) Map.t

  type 'a stack_operation = 'a double_connection DLL.node list

  let name = "AC-4"
  let loop_into_map f m = Map.iter m ~f:(fun v -> DLL.iter_value f v)

  let print_data_struct (c : 'a data_struct) =
    Map.iter c ~f:(fun ((a, b) : 'a DLL.node * 'a cell_type) ->
        Stdio.printf "%s is supported by " (Arc_consistency.make_name a);
        loop_into_map
          (fun e -> Stdio.printf "%s " (Arc_consistency.make_name e.node))
          b;
        Stdio.print_endline "")

  let initialization ?(print = false) (graph : 'a Graph.graph) =
    let graph = Arc_consistency.clean_domains ~print graph in
    let data_struct : 'a data_struct ref = ref (Map.empty (module Int)) in
    Graph.loop_domains
      (fun d ->
        DLL.iter
          (fun v ->
            let tbl = ref (Map.empty (module Int)) in
            DLL.iter_value
              (fun (n : 'a DLL.t) ->
                tbl := Map.add_exn !tbl ~key:n.id_dom ~data:(DLL.empty ""))
              (Graph.get_constraint_binding graph d);
            data_struct := Map.add_exn !data_struct ~key:v.id ~data:(v, !tbl))
          d)
      graph;
    let find_pair (elt : 'a DLL.node) (e2 : 'a DLL.node) =
      let _, supp = Map.find !data_struct elt.id |> Option.value_exn in
      Map.find supp e2.dll_father.id_dom |> Option.value_exn
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

  let revise (input : 'a DLL.node) (data_struct : 'a data_struct) =
    let _, supported = Map.find_exn data_struct input.id in
    let to_remove_in_domain = ref [] in
    let removed_in_support = ref [] in
    loop_into_map
      (fun ({ node; assoc } : 'a double_connection) ->
        let sibling = Option.value_exn assoc in
        DLL.remove sibling;
        removed_in_support := sibling :: !removed_in_support;
        if DLL.is_empty sibling.dll_father then
          to_remove_in_domain := node :: !to_remove_in_domain)
      supported;
    (!removed_in_support, !to_remove_in_domain)

  let back_track removed_in_suport = List.iter ~f:DLL.insert removed_in_suport
end

include AC_4