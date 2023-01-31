module AC_6 : Arc_consistency.Arc_consistency = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a cell_type = {
    value : 'a DLL.dll_node;
    is_supporting : 'a DLL.dll_node DLL.t;
    is_supported : 'a DLL.dll_node DLL.dll_node DLL.t;
  }

  type 'a data_struct = 'a Constraint.graph * (string, 'a cell_type) Hashtbl.t
  (** For each constraint and each value there is a linked list of supports *)

  type 'a remove_in_domain = string DLL.dll_node list

  type 'a stack_operation = {
    removed_in_domain : 'a remove_in_domain;
    removed_in_support : 'a cell_type;
    removed_from_is_supported : 'a DLL.dll_node DLL.dll_node list;
    appended_in_support : 'a DLL.dll_node DLL.dll_node list;
    input : 'a DLL.dll_node;
    data_struct : 'a data_struct;
  }

  let get_to_remove { removed_in_domain; _ } = removed_in_domain

  let print_data_struct ((_, x) : 'a data_struct) : unit =
    Hashtbl.iter
      (fun _ (e : 'a cell_type) ->
        Printf.printf "%s supports [ " e.value.value;
        DLL.iter_value
          (fun (e : 'a DLL.dll_node) -> Printf.printf "%s " e.value)
          e.is_supporting;
        print_endline "]")
      x

  let initialization ?(print = false) (graph : 'a Constraint.graph) :
      'a data_struct =
    let graph = Arc_consistency.clean_domains ~print graph in
    let data_struct : (string, 'a cell_type) Hashtbl.t = Hashtbl.create 1024 in
    let domain_list = Hashtbl.to_seq_values graph.domains |> List.of_seq in
    let empty_cell v =
      { value = v; is_supporting = DLL.empty ""; is_supported = DLL.empty "" }
    in
    let add_compteur v =
      Hashtbl.add data_struct (Arc_consistency.make_name v) (empty_cell v)
    in
    (* Add all values of every domains to the data_struct *)
    Hashtbl.iter (fun _ dom -> DLL.iter add_compteur dom) graph.domains;

    let should_add v1 v2 =
      let rec aux (v : string DLL.dll_node) =
        if v == v1 then true
        else if
          DLL.exist
            (fun e -> e.value == v2)
            (Hashtbl.find data_struct (Arc_consistency.make_name v))
              .is_supporting
        then false
        else match v.next with None -> true | Some e -> aux e
      in
      aux (DLL.get v1.dll_father.content).first
    in

    let rec aux = function
      | [] -> ()
      | d1 :: tl ->
          DLL.iter
            (fun v1 ->
              let dom1 =
                Hashtbl.find data_struct (Arc_consistency.make_name v1)
              in
              List.iter
                (fun d2 ->
                  DLL.iter
                    (fun v2 ->
                      if graph.relation v1 v2 then (
                        let dom2 =
                          Hashtbl.find data_struct
                            (Arc_consistency.make_name v2)
                        in
                        (if should_add v2 v1 then
                         let x = DLL.append v1 dom2.is_supporting in
                         DLL.append x dom1.is_supported |> ignore);
                        if should_add v1 v2 then
                          let x = DLL.append v2 dom1.is_supporting in
                          DLL.append x dom2.is_supported |> ignore))
                    d2)
                tl)
            d1;
          aux tl
    in
    aux domain_list;
    (graph, data_struct)

  (** For each value v in d1 it should exist a support in d2, otherwise we remove v from d1,
  returns the list of filtered values.
  If the list is empty, then no modification has been performed on d *)
  let revise (node_to_remove : 'a DLL.dll_node)
      ((graph, data_struct) as g : 'a data_struct) : 'a stack_operation =
    (* Look for the support to remove in data_struct *)
    match
      Hashtbl.find_opt data_struct (Arc_consistency.make_name node_to_remove)
    with
    | None -> raise (Not_in_support "AC_6")
    | Some node ->
        let removed_in_domain = ref [] in
        let appended_in_support = ref [] in
        let removed_from_is_supported = ref [] in
        (* We remove the support *)
        Hashtbl.remove data_struct (Arc_consistency.make_name node.value);
        (* Remove node_to_remove from all the node supporting it *)
        DLL.iter_value
          (fun value ->
            removed_from_is_supported := value :: !removed_from_is_supported;
            DLL.remove value)
          node.is_supported;
        DLL.iter_value
          (fun current ->
            match
              Option.bind node_to_remove.next
                (DLL.find_from (graph.relation current))
            with
            | None -> removed_in_domain := current :: !removed_in_domain
            | Some e ->
                (* Here there exists a next of node_to_remove linked to current *)
                let dom =
                  Hashtbl.find data_struct (Arc_consistency.make_name e)
                in
                let appended = DLL.append current dom.is_supporting in
                appended_in_support := appended :: !appended_in_support)
          node.is_supporting;
        {
          removed_in_domain = !removed_in_domain;
          removed_in_support = node;
          appended_in_support = !appended_in_support;
          input = node_to_remove;
          data_struct = g;
          removed_from_is_supported = !removed_from_is_supported;
        }

  let back_track
      {
        removed_in_support;
        appended_in_support;
        removed_from_is_supported;
        input;
        data_struct;
        _;
      } =
    List.iter DLL.insert removed_from_is_supported;
    Hashtbl.add (snd data_struct)
      (Arc_consistency.make_name removed_in_support.value)
      removed_in_support;
    List.iter DLL.remove appended_in_support;
    DLL.insert input
end

include AC_6