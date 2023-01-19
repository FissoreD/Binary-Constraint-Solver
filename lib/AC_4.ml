module DLL = DoublyLinkedList

type 'a double_connection = {
  value : 'a DLL.dll_node;
  mutable assoc : 'a double_connection DLL.dll_node option;
  mutable node_in_support : int cell_type DLL.dll_node option;
}

and 'a cell_type = 'a DLL.dll_node * 'a double_connection DLL.t

and 'a compteurs = 'a cell_type DLL.t
(**
  For each constraint and each value there is a linked list of supports  
*)

type stack_operation = {
  to_remove_in_domain : int DLL.dll_node list;
  to_remove_in_support : int cell_type DLL.dll_node list;
  to_remove_sibling : int double_connection DLL.dll_node list;
  input : int DLL.dll_node;
}

let print_compteurs =
  DLL.iter (fun { value; _ } ->
      let (a : int DLL.dll_node), b = value in
      Printf.printf "%d : length : %d -- " a.value (DLL.length b);
      DLL.iter
        (fun e ->
          let x = e.value.value in
          Printf.printf "%d " x.value)
        b;
      print_newline ())

let build_support ({ tbl; _ } : 'a Constraint.supports) =
  let compteurs : int compteurs = DLL.empty "compteur" in
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
      let last_x =
        DLL.append
          { value = b; assoc = None; node_in_support = None }
          (snd x.value)
      in
      let last_y =
        DLL.append
          { value = a; assoc = None; node_in_support = None }
          (snd y.value)
      in
      if DLL.is_empty last_y.dll_father || DLL.is_empty last_x.dll_father then
        invalid_arg "It is none";
      last_x.value.assoc <- Some last_y;
      last_x.value.node_in_support <- Some y;
      last_y.value.assoc <- Some last_x;
      last_y.value.node_in_support <- Some x)
    tbl;
  compteurs

(** 
  For each value v in d1 it should exist a support in d2, otherwise we remove v from d1,
  returns the list of filtered values.
  If the list is empty, then no modification has been performed on d1
*)
let revise (node_to_remove : 'a DLL.dll_node) (compteurs : 'a compteurs) =
  print_endline "ciao";
  (* Look for the support to remove in compteurs *)
  let to_remove_in_domain : int DLL.dll_node list ref = ref [] in
  let to_remove_sibling = ref [] in
  let to_remove_in_support = ref [] in
  (match DLL.find (fun e -> fst e.value == node_to_remove) compteurs with
  | None -> ()
  | Some remove ->
      (* We remove the support since it exists *)
      Printf.printf "Loop %d times \n" (DLL.length (snd remove.value));
      DLL.remove remove;
      to_remove_in_support := [ remove ];
      DLL.iter
        (fun (current : 'a double_connection DLL.dll_node) ->
          Printf.printf "--- %s ---\n" current.value.value.dll_father.name;
          let sibling = Option.get current.value.assoc in
          (* We remove the support from the values depending on node_to_remove *)
          DLL.remove sibling;
          to_remove_sibling := sibling :: !to_remove_sibling;
          (* Here we remove the value from the other domain since the value has no support anymore *)
          if DLL.is_empty sibling.dll_father then
            to_remove_in_domain := current.value.value :: !to_remove_in_domain)
        (* !(snd e.value) is the dll of values depending on e*)
        (snd remove.value));
  {
    to_remove_in_domain = !to_remove_in_domain;
    to_remove_in_support = !to_remove_in_support;
    to_remove_sibling = !to_remove_sibling;
    input = node_to_remove;
  }

let back_track
    { to_remove_in_domain; to_remove_in_support; to_remove_sibling; input } =
  List.iter DLL.insert to_remove_in_domain;
  List.iter DLL.insert to_remove_in_support;
  List.iter DLL.insert to_remove_sibling;
  DLL.insert input
