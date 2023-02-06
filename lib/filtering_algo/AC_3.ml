module AC_3 : Arc_consistency.Arc_consistency = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a remove_in_domain = string Graph.value list
  type 'a stack_operation = unit
  type 'a data_struct = 'a Graph.graph

  let name = "AC-3"
  let print_data_struct _ = print_endline "No data structure for AC-3"
  let initialization = Arc_consistency.clean_domains

  let revise (v1 : 'a Graph.value) (graph : 'a data_struct) =
    let to_remove_in_domain : 'a Graph.value list ref = ref [] in
    DLL.iter_value
      (DLL.iter (fun current ->
           if DLL.not_exist (Graph.relation graph current) v1.father then
             to_remove_in_domain := current :: !to_remove_in_domain))
      (Graph.get_constraint_binding graph v1.father);
    ((), !to_remove_in_domain)

  let back_track _ = ()
end

include AC_3