module AC_4 : Filtrage.Arc_Consistency = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a double_connection = {
    value : 'a DLL.dll_node;
    mutable assoc : 'a double_connection DLL.dll_node option;
  }

  type 'a remove_in_domain = string DLL.dll_node list
  type 'a cell_type = 'a DLL.dll_node * 'a double_connection DLL.t

  type 'a data_struct = 'a cell_type DLL.t
  (** For each constraint and each value there is a linked list of supports  *)

  type 'a stack_operation = {
    to_remove_in_domain : 'a remove_in_domain;
    to_remove_in_support : 'a cell_type DLL.dll_node;
    to_remove_sibling : 'a double_connection DLL.dll_node list;
    input : 'a DLL.dll_node;
  }

  let print_data_struct (c : 'a data_struct) =
    print_endline "---------------------";
    DLL.iter_value
      (fun ((a, b) : 'a cell_type) ->
        Printf.printf "%s is supported by " a.value;
        DLL.iter_value (fun e -> Printf.printf "%s " e.value.value) b;
        print_newline ())
      c;
    print_endline "---------------------"

  let initialization (g : 'a Constraint.graph) =
    let data_struct : 'a data_struct = DLL.empty "compteur" in
    Hashtbl.iter
      (fun _ (d : 'a DoublyLinkedList.t) ->
        DLL.iter (fun v -> DLL.append (v, DLL.empty "") data_struct |> ignore) d)
      g.domains;
    let add_new_pair elt =
      DLL.find_assoc (( == ) elt) data_struct |> Option.get
    in
    Hashtbl.iter
      (fun _ (a, b) ->
        let x, y = (add_new_pair a, add_new_pair b) in
        let last_x = DLL.append { value = b; assoc = None } (snd x.value) in
        let last_y = DLL.append { value = a; assoc = None } (snd y.value) in
        last_x.value.assoc <- Some last_y;
        last_y.value.assoc <- Some last_x)
      g.tbl;
    data_struct

  (** For each value v in d1 it should exist a support in d2, otherwise we remove v from d1,
  returns the list of filtered values.
  If the list is empty, then no modification has been performed on d1 *)
  let revise (node_to_remove : 'a DLL.dll_node) (data_struct : 'a data_struct) =
    (* Look for the support to remove in data_struct *)
    match DLL.find (fun e -> fst e.value == node_to_remove) data_struct with
    | None ->
        print_endline node_to_remove.value;
        print_data_struct data_struct;
        raise (Not_in_support "AC_4")
    | Some remove ->
        let to_remove_in_domain = ref [] in
        let to_remove_sibling = ref [] in
        (* We remove the support since it exists *)
        DLL.remove remove;
        DLL.iter_value
          (fun (current : 'a double_connection) ->
            let sibling = Option.get current.assoc in
            (* We remove the support from the values depending on node_to_remove *)
            DLL.remove sibling;
            to_remove_sibling := sibling :: !to_remove_sibling;
            if
              (* Here we remove the value from the other domain since the value has no more support *)
              DLL.not_exsist
                (fun e -> e.value.value.dll_father == node_to_remove.dll_father)
                sibling.dll_father
            then to_remove_in_domain := current.value :: !to_remove_in_domain)
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

include AC_4