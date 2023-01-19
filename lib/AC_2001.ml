module DLL = DoublyLinkedList

type 'a cell_type = int DLL.dll_node * int DLL.dll_node DLL.t

and 'a compteurs = int cell_type DLL.t
(**
  For each constraint and each value there is a linked list of supports  
*)

let print_compteurs (t : int compteurs) : unit =
  DLL.iter
    (fun ({ value; _ } : int cell_type DLL.dll_node) ->
      let a, b = value in
      Printf.printf "length : %d _ %d -- " (DLL.length b) a.value;
      DLL.iter
        (fun (e : int DoublyLinkedList.dll_node DoublyLinkedList.dll_node) ->
          let x = e in
          Printf.printf "%d " x.value.value)
        b;
      print_newline ())
    t

let build_support ({ relation; domains; _ } : int Constraint.supports) =
  let added = DLL.empty "added" in
  (* let assoc = DLL.empty () in *)
  let compteurs : int compteurs = DLL.empty "compteur" in
  DLL.iter
    (fun (domain : int DLL.t DLL.dll_node) ->
      let rec aux (a : int DLL.dll_node) =
        DLL.iter
          (fun next_domain ->
            if next_domain != domain then
              DLL.iter
                (fun b ->
                  if relation a b then
                    let add_new_pair elt =
                      match DLL.find_assoc (( == ) elt) compteurs with
                      | None -> DLL.append (elt, DLL.empty "") compteurs
                      | Some e -> e
                    in
                    let can_add (elt : 'a DLL.dll_node) =
                      DLL.not_exsist (fun e -> e.value == elt) added
                    in
                    let y = add_new_pair b in
                    if can_add a then (
                      DLL.append a added |> ignore;
                      DLL.append a (snd y.value) |> ignore))
                next_domain.value;
            match a.next with None -> () | Some e -> aux e)
          domains
      in
      match !(domain.value.content) with
      | None -> ()
      | Some domain -> aux domain.first)
    domains;
  compteurs

(** 
  For each value v in d1 it should exist a support in d2, otherwise we remove v from d1,
  returns the list of filtered values.
  If the list is empty, then no modification has been performed on d1
*)
let revise (node_to_remove : int DLL.dll_node) (compteurs : int compteurs)
    (support : int Constraint.supports) =
  (* Look for the support to remove in compteurs *)
  let removed_list : int DLL.dll_node list ref = ref [] in
  match DLL.find (fun e -> fst e.value == node_to_remove) compteurs with
  | None -> !removed_list
  | Some remove ->
      (* We remove the support since it exists *)
      DLL.remove remove;
      DLL.iter
        (fun current ->
          let sibling = current.value in
          match
            match node_to_remove.next with
            | None -> None
            | Some next ->
                DLL.find_from (fun e -> support.relation e sibling) next
          with
          | None -> removed_list := sibling :: !removed_list
          | Some e ->
              let x =
                Option.get (DLL.find (fun r -> fst r.value == e) compteurs)
              in
              (* TODO here we are adding, so in back-track we should remove ! *)
              DLL.append sibling (snd x.value) |> ignore)
        (* !(snd e.value) is the dll of values depending on e*)
        (snd remove.value);
      !removed_list
