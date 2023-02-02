module AC_6 : Arc_consistency.Arc_consistency = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a cell_type = {
    value : 'a DLL.dll_node;
    is_supporting : (string, 'a DLL.dll_node DLL.t) Hashtbl.t;
  }

  type 'a data_struct = 'a Constraint.graph * (int, 'a cell_type) Hashtbl.t
  type 'a remove_in_domain = string DLL.dll_node list

  type 'a stack_operation = {
    removed_in_domain : 'a remove_in_domain;
    appended_in_support : 'a DLL.dll_node DLL.dll_node list;
    input : 'a DLL.dll_node;
  }

  let get_to_remove { removed_in_domain; _ } = removed_in_domain
  let loop_into_map f m = Hashtbl.iter (fun _ v -> DLL.iter f v) m

  let print_data_struct ((_, x) : 'a data_struct) : unit =
    Hashtbl.iter
      (fun _ (e : 'a cell_type) ->
        Printf.printf "%s is supporting " (Arc_consistency.make_name e.value);
        loop_into_map
          (fun e -> Printf.printf "%s " (Arc_consistency.make_name e.value))
          e.is_supporting;
        print_endline "")
      x

  let initialization ?(print = false) graph : 'a data_struct =
    let graph = Arc_consistency.clean_domains ~print graph in
    let data_struct : (int, 'a cell_type) Hashtbl.t = Hashtbl.create 1024 in
    let empty_cell v = { value = v; is_supporting = Hashtbl.create 2048 } in
    let add_compteur (v : 'a DLL.dll_node) =
      Hashtbl.add data_struct v.id (empty_cell v)
    in
    Constraint.loop_domains (fun dom -> DLL.iter add_compteur dom) graph;

    let exception Found in
    Constraint.loop_domains
      (fun d1 ->
        DLL.iter
          (fun v1 ->
            let dom1 = Hashtbl.find data_struct v1.id in
            DLL.iter_value
              (fun (d2 : string DLL.t) ->
                try
                  DLL.iter
                    (fun v2 ->
                      if graph.relation v1 v2 then (
                        let dom2 = Hashtbl.find data_struct v2.id in
                        Hashtbl.add dom2.is_supporting d1.name (DLL.empty "");
                        if not (Hashtbl.mem dom1.is_supporting d2.name) then
                          Hashtbl.add dom1.is_supporting d2.name (DLL.empty "");
                        DLL.append v1 (Hashtbl.find dom2.is_supporting d1.name);
                        raise Found))
                    d2
                with Found -> ())
              (Constraint.get_constraint_binding graph d1))
          d1)
      graph;

    (graph, data_struct)

  let revise (node_to_remove : string DLL.dll_node)
      ((graph, data_struct) : 'a data_struct) : 'a stack_operation =
    match Hashtbl.find_opt data_struct node_to_remove.id with
    | None -> raise (Not_in_support "AC_6")
    | Some node ->
        let removed_in_domain = ref [] in
        let appended_in_support = ref [] in
        loop_into_map
          (fun (current : 'a DLL.dll_node DLL.dll_node) ->
            let current = current.value in
            let node_in_support = Hashtbl.find data_struct current.id in
            match
              DLL.find_from_next
                (graph.relation node_in_support.value)
                node_to_remove
            with
            | None -> removed_in_domain := current :: !removed_in_domain
            | Some e ->
                let dom = Hashtbl.find data_struct e.id in
                let d =
                  Hashtbl.find dom.is_supporting current.dll_father.name
                in
                DLL.append current d;
                appended_in_support := DLL.get_last d :: !appended_in_support)
          node.is_supporting;
        {
          removed_in_domain = !removed_in_domain;
          appended_in_support = !appended_in_support;
          input = node_to_remove;
        }

  let back_track { appended_in_support; input; _ } =
    List.iter DLL.remove appended_in_support;
    DLL.insert input
end

include AC_6