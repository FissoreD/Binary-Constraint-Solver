module AC_6 : Filtrage.Algo_Filtrage = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a cell_type = {
    value : 'a DLL.dll_node;
    is_supporting : 'a DLL.dll_node DLL.t;
    is_supported : 'a DLL.dll_node DLL.dll_node DLL.t;
  }

  type 'a compteurs = 'a Constraint.supports * (string, 'a cell_type) Hashtbl.t
  (** For each constraint and each value there is a linked list of supports *)

  type 'a stack_operation = {
    removed_in_domain : 'a DLL.dll_node list;
    removed_in_support : 'a cell_type;
    removed_from_is_supported : 'a DLL.dll_node DLL.dll_node list;
    appended_in_support : 'a DLL.dll_node DLL.dll_node list;
    input : 'a DLL.dll_node;
    compteurs : 'a compteurs;
  }

  type 'a remove_in_domain = string DLL.dll_node list

  let get_to_remove { removed_in_domain; _ } = removed_in_domain

  let make_name (node : 'a DLL.dll_node) =
    Printf.sprintf "%s,%s" node.dll_father.name node.value

  let print_compteurs ((_, x) : 'a compteurs) : unit =
    Hashtbl.iter
      (fun _ (e : 'a cell_type) ->
        Printf.printf "%s supports [ " e.value.value;
        DLL.iter
          (fun (e : 'a DLL.dll_node DLL.dll_node) ->
            Printf.printf "%s " e.value.value)
          e.is_supporting;
        print_endline "]")
      x

  let build_support (graph : 'a Constraint.supports) : 'a compteurs =
    let compteurs : (string, 'a cell_type) Hashtbl.t = Hashtbl.create 1024 in
    let domain_list = Hashtbl.to_seq_values graph.domains |> List.of_seq in
    let empty_cell v =
      { value = v; is_supporting = DLL.empty ""; is_supported = DLL.empty "" }
    in
    (* Add all values to the compteurs *)
    Hashtbl.iter
      (fun _ dom ->
        DLL.iter
          (fun v -> Hashtbl.add compteurs (make_name v) (empty_cell v))
          dom)
      graph.domains;
    let should_add v1 v2 =
      let rec aux (v : string DLL.dll_node) =
        if v == v1 then true
        else if
          DLL.exsist
            (fun e -> e.value == v2)
            (Hashtbl.find compteurs (make_name v)).is_supporting
        then false
        else match v.next with None -> true | Some e -> aux e
      in
      aux (DLL.get v1.dll_father.content).first
    in
    let rec aux (l : 'a DLL.t list) =
      match l with
      | [] -> ()
      | d1 :: tl ->
          DLL.iter
            (fun v1 ->
              let dom1 = Hashtbl.find compteurs (make_name v1) in
              List.iter
                (fun d2 ->
                  DLL.iter
                    (fun v2 ->
                      if graph.relation v1 v2 then (
                        let dom2 = Hashtbl.find compteurs (make_name v2) in
                        (if should_add v2 v1 then
                         let x = DLL.append v1 dom2.is_supporting in
                         DLL.append x dom1.is_supported |> ignore);
                        if should_add v1 v2 then
                          let x = DLL.append v2 dom1.is_supporting in
                          DLL.append x dom2.is_supported |> ignore))
                    d2)
                tl;
              aux tl)
            d1
    in
    aux domain_list;
    (graph, compteurs)

  (** 
  For each value v in d1 it should exist a support in d2, otherwise we remove v from d1,
  returns the list of filtered values.
  If the list is empty, then no modification has been performed on d *)
  let revise (node_to_remove : 'a DLL.dll_node)
      ((graph, compteurs) as g : 'a compteurs) : 'a stack_operation =
    (* Look for the support to remove in compteurs *)
    match Hashtbl.find_opt compteurs (make_name node_to_remove) with
    | None -> raise (Not_in_support "AC_6")
    | Some node ->
        let removed_in_domain = ref [] in
        let appended_in_support = ref [] in
        let removed_from_is_supported = ref [] in
        (* We remove the support since it exists *)
        DLL.remove node.value;
        Hashtbl.remove compteurs (make_name node.value);
        (* Remove node_to_remove from all the node supporting it *)
        DLL.iter
          (fun e ->
            removed_from_is_supported := e.value :: !removed_from_is_supported;
            DLL.remove e.value)
          node.is_supported;
        DLL.iter
          (fun current ->
            let sibling = current.value in
            match
              Option.bind node_to_remove.next
                (DLL.find_from (fun e -> graph.relation e sibling))
            with
            | None -> removed_in_domain := sibling :: !removed_in_domain
            | Some e ->
                (* Here there exists a next of node_to_remove linked to current *)
                let dom = Hashtbl.find compteurs (make_name e) in
                let appended = DLL.append sibling dom.is_supporting in
                appended_in_support := appended :: !appended_in_support)
          node.is_supporting;
        {
          removed_in_domain = !removed_in_domain;
          removed_in_support = node;
          appended_in_support = !appended_in_support;
          input = node_to_remove;
          compteurs = g;
          removed_from_is_supported = !removed_from_is_supported;
        }

  let back_track
      {
        removed_in_domain;
        removed_in_support;
        appended_in_support;
        removed_from_is_supported;
        input;
        compteurs;
      } =
    List.iter DLL.insert removed_in_domain;
    List.iter DLL.insert removed_from_is_supported;
    Hashtbl.add (snd compteurs)
      (make_name removed_in_support.value)
      removed_in_support;
    List.iter DLL.remove appended_in_support;
    DLL.insert input
end

include AC_6