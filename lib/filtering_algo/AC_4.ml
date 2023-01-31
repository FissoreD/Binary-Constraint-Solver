module AC_4 : Arc_consistency.Arc_consistency = struct
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
    DLL.iter_value
      (fun ((a, b) : 'a cell_type) ->
        Printf.printf "%s is supported by " a.value;
        DLL.iter_value (fun e -> Printf.printf "%s " e.value.value) b;
        print_newline ())
      c

  let initialization ?(print = false) (graph : 'a Constraint.graph) =
    let graph = Arc_consistency.clean_domains ~print graph in
    let data_struct : 'a data_struct = DLL.empty "compteur" in
    Hashtbl.iter
      (fun _ (d : 'a DoublyLinkedList.t) ->
        DLL.iter (fun v -> DLL.append (v, DLL.empty "") data_struct |> ignore) d)
      graph.domains;
    let add_new_pair elt =
      Option.get (DLL.find_assoc (( == ) elt) data_struct)
    in
    Hashtbl.iter
      (fun _ ((a : string DLL.dll_node), (b : string DLL.dll_node)) ->
        if a.is_in && b.is_in then (
          let x, y = (add_new_pair a, add_new_pair b) in
          let last_x = DLL.append { value = b; assoc = None } (snd x.value) in
          let last_y = DLL.append { value = a; assoc = None } (snd y.value) in
          last_x.value.assoc <- Some last_y;
          last_y.value.assoc <- Some last_x))
      graph.tbl;
    data_struct

  (** For each value v in d1 it should exist a support in d2, otherwise we remove v from d1,
  returns the list of filtered values.
  If the list is empty, then no modification has been performed on d1 *)
  let revise (input : 'a DLL.dll_node) (data_struct : 'a data_struct) =
    (* Look for the support to remove in data_struct *)
    match DLL.find (fun e -> fst e.value == input) data_struct with
    | None -> raise (Not_in_support "AC_4")
    | Some to_remove_in_support ->
        let to_remove_in_domain = ref [] in
        let to_remove_sibling = ref [] in
        (* We remove the support since it exists *)
        DLL.remove to_remove_in_support;
        DLL.iter_value
          (fun (current : 'a double_connection) ->
            let sibling = Option.get current.assoc in
            (* We remove the support from the values depending on node_to_remove *)
            DLL.remove sibling;
            to_remove_sibling := sibling :: !to_remove_sibling;
            if
              (* Here we remove the value from the other domain since the value has no more support *)
              DLL.not_exist
                (fun e -> e.value.value.dll_father == input.dll_father)
                sibling.dll_father
            then to_remove_in_domain := current.value :: !to_remove_in_domain)
          (snd to_remove_in_support.value);
        {
          to_remove_in_domain = !to_remove_in_domain;
          to_remove_in_support;
          to_remove_sibling = !to_remove_sibling;
          input;
        }

  let back_track { to_remove_in_support; to_remove_sibling; input; _ } =
    DLL.insert to_remove_in_support;
    List.iter DLL.insert to_remove_sibling;
    DLL.insert input

  let get_to_remove { to_remove_in_domain; _ } = to_remove_in_domain
end

include AC_4