open Base

module AC_6 : Arc_consistency.Arc_consistency = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a cell_type = { value : 'a DLL.node; is_supporting : 'a DLL.node DLL.t }
  type 'a s_list = (int, 'a cell_type) Hashtbl.t
  type 'a data_struct = 'a Graph.graph * 'a s_list
  type 'a remove_in_domain = string DLL.node list
  type 'a stack_operation = 'a DLL.node DLL.node list

  let name = "AC-6"
  let loop_into_map f v = DLL.iter f v

  let print_data_struct ((_, x) : 'a data_struct) : unit =
    Hashtbl.iter
      ~f:(fun (e : 'a cell_type) ->
        Stdio.printf "%s is supporting " (Arc_consistency.make_name e.value);
        loop_into_map
          (fun e -> Stdio.printf "%s " (Arc_consistency.make_name e.value))
          e.is_supporting;
        Stdio.print_endline "")
      x

  let initialization ?(print = false) graph : 'a data_struct =
    let graph = Arc_consistency.clean_domains ~print graph in
    let data_struct : (int, 'a cell_type) Hashtbl.t =
      Hashtbl.create (module Int)
    in
    let empty_cell v = { value = v; is_supporting = DLL.empty "" } in
    let add_compteur (v : 'a DLL.node) =
      Hashtbl.add_exn data_struct ~key:v.id ~data:(empty_cell v)
    in
    Graph.loop_domains (fun dom -> DLL.iter add_compteur dom) graph;

    let exception Found in
    Graph.loop_domains
      (fun d1 ->
        DLL.iter
          (fun v1 ->
            DLL.iter_value
              (fun (d2 : string DLL.t) ->
                try
                  DLL.iter
                    (fun v2 ->
                      if Graph.relation graph v1 v2 then (
                        let dom2 = Hashtbl.find_exn data_struct v2.id in
                        DLL.append v1 dom2.is_supporting;
                        raise Found))
                    d2
                with Found -> ())
              (Graph.get_constraint_binding graph d1))
          d1)
      graph;
    (graph, data_struct)

  let revise (node_to_remove : string DLL.node)
      ((graph, s_list) : 'a data_struct) =
    match Hashtbl.find s_list node_to_remove.id with
    | None -> raise (Not_in_support "AC_6")
    | Some node ->
        let removed_in_domain = ref [] in
        let appended_in_support = ref [] in
        loop_into_map
          (fun (current : 'a DLL.node DLL.node) ->
            let current = current.value in
            match
              DLL.find_from_next (Graph.relation graph current) node_to_remove
            with
            | None -> removed_in_domain := current :: !removed_in_domain
            | Some e ->
                let dom = Hashtbl.find_exn s_list e.id in
                DLL.append current dom.is_supporting;
                appended_in_support :=
                  DLL.get_last dom.is_supporting :: !appended_in_support)
          node.is_supporting;
        (!appended_in_support, !removed_in_domain)

  let back_track appended_in_support =
    List.iter ~f:DLL.remove appended_in_support
end

include AC_6