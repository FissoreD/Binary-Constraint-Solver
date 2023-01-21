module AC_2001 : Filtrage.Algo_Filtrage = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a double_connection = {
    value : 'a DLL.dll_node;
    mutable assoc : 'a double_connection DLL.dll_node option;
  }

  type 'a remove_in_domain = string DLL.dll_node list
  type 'a cell_type = 'a DLL.dll_node * 'a double_connection DLL.t

  type 'a compteurs = 'a cell_type DLL.t
  (** For each constraint and each value there is a linked list of supports  *)

  type 'a stack_operation = {
    to_remove_in_domain : 'a remove_in_domain;
    to_remove_in_support : 'a cell_type DLL.dll_node;
    to_remove_sibling : 'a double_connection DLL.dll_node list;
    input : 'a DLL.dll_node;
  }

  let print_compteurs (c : 'a compteurs) =
    DLL.iter
      (fun ({ value; _ } : 'a cell_type DLL.dll_node) ->
        let a, b = value in
        Printf.printf "%s is supported by " a.value;
        DLL.iter (fun e -> Printf.printf "%s " e.value.value.value) b;
        print_newline ())
      c

  let build_support ({ tbl; _ } : 'a Constraint.supports) =
    let compteurs : 'a compteurs = DLL.empty "compteur" in
    DLL.iter
      (fun node ->
        (* The support ab *)
        let a, b = node.value in
        let add_new_pair elt =
          match DLL.find_assoc (( == ) elt) compteurs with
          | None -> DLL.append (elt, DLL.empty "") compteurs
          | Some e -> e
        in
        let x, y = (add_new_pair a, add_new_pair b) in
        let last_x = DLL.append { value = b; assoc = None } (snd x.value) in
        let last_y = DLL.append { value = a; assoc = None } (snd y.value) in
        if DLL.is_empty last_y.dll_father || DLL.is_empty last_x.dll_father then
          invalid_arg "It is none";
        last_x.value.assoc <- Some last_y;
        last_y.value.assoc <- Some last_x)
      tbl;
    compteurs

  (** 
  For each value v in d1 it should exist a support in d2, otherwise we remove v from d1,
  returns the list of filtered values.
  If the list is empty, then no modification has been performed on d1
*)
  let revise (node_to_remove : 'a DLL.dll_node) (compteurs : 'a compteurs) =
    (* Look for the support to remove in compteurs *)
    match DLL.find (fun e -> fst e.value == node_to_remove) compteurs with
    | None -> raise (Not_in_support "AC_2001")
    | Some remove ->
        let to_remove_in_domain : 'a DLL.dll_node list ref = ref [] in
        let to_remove_sibling = ref [] in
        (* We remove the support since it exists *)
        DLL.remove remove;
        DLL.iter
          (fun (current : 'a double_connection DLL.dll_node) ->
            let sibling = Option.get current.value.assoc in
            (* We remove the support from the values depending on node_to_remove *)
            DLL.remove sibling;
            to_remove_sibling := sibling :: !to_remove_sibling;
            if
              (* Here we remove the value from the other domain since the value has no more support *)
              DLL.not_exsist
                (fun e -> e.value.value.dll_father == node_to_remove.dll_father)
                sibling.dll_father
            then
              to_remove_in_domain := current.value.value :: !to_remove_in_domain)
          (snd remove.value);
        {
          to_remove_in_domain = !to_remove_in_domain;
          to_remove_in_support = remove;
          to_remove_sibling = !to_remove_sibling;
          input = node_to_remove;
        }

  let back_track
      { to_remove_in_domain; to_remove_in_support; to_remove_sibling; input } =
    List.iter DLL.insert to_remove_in_domain;
    DLL.insert to_remove_in_support;
    List.iter DLL.insert to_remove_sibling;
    DLL.insert input

  let get_to_remove { to_remove_in_domain; _ } = to_remove_in_domain
end

include AC_2001