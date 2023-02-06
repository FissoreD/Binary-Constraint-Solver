module AC_3 : Arc_consistency.Arc_consistency = struct
  module DLL = DoublyLinkedList

  type 'a stack_operation = unit
  type 'a data_struct = 'a Graph.graph

  let print_data_struct _ = print_endline "No data structure for AC-3"
  let initialization = Arc_consistency.clean_domains

  let revise (v1 : 'a Graph.value) (graph : 'a data_struct) =
    let delta_domains : 'a Graph.value list ref = ref [] in
    DLL.iter_value
      (DLL.iter (fun v2 ->
           if DLL.not_exist (Graph.relation graph v2) v1.father then
             delta_domains := v2 :: !delta_domains))
      (Graph.get_constraint_binding graph v1.father);
    ((), !delta_domains)

  let back_track _ = ()
end

include AC_3