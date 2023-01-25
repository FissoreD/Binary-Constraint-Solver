module AC_3 : Filtrage.Arc_Consistency = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a remove_in_domain = string DLL.dll_node list

  type 'a stack_operation = {
    to_remove_in_domain : 'a DLL.dll_node list;
    input : 'a DLL.dll_node;
  }

  type 'a data_struct = 'a Constraint.graph

  let print_data_struct = ignore
  let initialization = Fun.id

  (** For each value v in d1 it should exist a support in d2, otherwise we remove v from d1,
  returns the list of filtered values.
  If the list is empty, then no modification has been performed on d1 *)
  let revise (d1 : 'a DLL.dll_node) (support : 'a data_struct) =
    let to_remove_in_domain : 'a DLL.dll_node list ref = ref [] in
    DLL.iter_value
      (DLL.iter (fun current ->
           let remove_current =
             DLL.not_exsist (fun e -> support.relation current e) d1.dll_father
           in
           if remove_current then
             to_remove_in_domain := current :: !to_remove_in_domain))
      (Constraint.get_constraint_binding support d1.dll_father);
    { input = d1; to_remove_in_domain = !to_remove_in_domain }

  let back_track { to_remove_in_domain; input } =
    List.iter DLL.insert to_remove_in_domain;
    DLL.insert input

  let get_to_remove { to_remove_in_domain; _ } = to_remove_in_domain
end

include AC_3