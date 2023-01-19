module DLL = DoublyLinkedList

type stack_operation = {
  to_remove_in_domain : int DLL.dll_node list;
  input : int DLL.dll_node;
}

(** 
  For each value v in d1 it should exist a support in d2, otherwise we remove v from d1,
  returns the list of filtered values.
  If the list is empty, then no modification has been performed on d1
*)
let revise (d1 : 'a DLL.dll_node) (support : 'a Constraint.supports) =
  let to_remove_in_domain : int DLL.dll_node list ref = ref [] in
  (match !(d1.dll_father.content) with
  | None -> ()
  | Some _ ->
      DLL.iter
        (fun (d2 : int DoublyLinkedList.t DoublyLinkedList.dll_node) ->
          DLL.iter
            (fun current ->
              let remove_current =
                DLL.not_exsist
                  (fun e -> support.relation current e)
                  d1.dll_father
              in
              if remove_current then
                to_remove_in_domain := current :: !to_remove_in_domain)
            d2.value)
        ((Option.get
            (DLL.find_assoc (( == ) d1.dll_father) support.constraint_binding))
           .value |> snd));
  { input = d1; to_remove_in_domain = !to_remove_in_domain }

let back_track { to_remove_in_domain; input } =
  List.iter DLL.insert to_remove_in_domain;
  DLL.insert input
