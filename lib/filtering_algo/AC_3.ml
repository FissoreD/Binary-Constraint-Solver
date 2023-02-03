module AC_3 : Arc_consistency.Arc_consistency = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a remove_in_domain = string DLL.node list
  type 'a stack_operation = unit
  type 'a data_struct = 'a Constraint.graph

  let name = "AC-3"
  let print_data_struct _ = print_endline "No data structure for AC-3"
  let initialization = Arc_consistency.clean_domains

  let revise (d1 : 'a DLL.node) (graph : 'a data_struct) =
    let to_remove_in_domain : 'a DLL.node list ref = ref [] in
    DLL.iter_value
      (DLL.iter (fun current ->
           if DLL.not_exist (Constraint.relation graph current) d1.dll_father
           then to_remove_in_domain := current :: !to_remove_in_domain))
      (Constraint.get_constraint_binding graph d1.dll_father);
    ((), !to_remove_in_domain)

  let back_track _ = ()
end

include AC_3