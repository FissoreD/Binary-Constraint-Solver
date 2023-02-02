module AC_3 : Arc_consistency.Arc_consistency = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a remove_in_domain = string DLL.dll_node list
  type 'a stack_operation = unit
  type 'a data_struct = 'a Constraint.graph

  let print_data_struct _ = print_endline "No data structure for AC-3"
  let initialization = Arc_consistency.clean_domains

  (** For each value v in d1 it should exist a support in d2, otherwise we remove v from d1,
  returns the list of filtered values.
  If the list is empty, then no modification has been performed on d1 *)
  let revise (d1 : 'a DLL.dll_node) (support : 'a data_struct) =
    let to_remove_in_domain : 'a DLL.dll_node list ref = ref [] in
    DLL.iter_value
      (DLL.iter (fun current ->
           if DLL.not_exist (support.relation current) d1.dll_father then
             to_remove_in_domain := current :: !to_remove_in_domain))
      (Constraint.get_constraint_binding support d1.dll_father);
    ((), !to_remove_in_domain)

  let back_track _ = ()
end

include AC_3