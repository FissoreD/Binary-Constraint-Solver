open Base

module AC_6 = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a remove_in_domain = string Graph.value list

  type 'a cell = {
    s_list : ('a Graph.value * 'a cell) DLL.t;
    last : ('a Graph.domain, 'a Graph.value DLL.t) Hashtbl.t;
  }

  type 'a int_struct = ('a Graph.value, 'a cell) Hashtbl.t
  type 'a data_struct = 'a Graph.graph * 'a int_struct

  type 'a stack_operation =
    ('a Graph.value * 'a cell) DLL.node list * 'a Graph.value DLL.node list

  let name = "AC-6"

  let print_data_struct ((_, x) : 'a data_struct) : unit =
    Hashtbl.iteri
      ~f:(fun ~key ~data ->
        Stdio.printf "%s last is : " (Arc_consistency.make_name key);

        Hashtbl.iter
          ~f:
            (DLL.iter_value (fun e ->
                 Stdio.printf "%s " (Arc_consistency.make_name e)))
          data.last;
        Stdio.printf "and the s list is : ";
        DLL.iter_value
          (fun data ->
            Stdio.printf "%s " (Arc_consistency.make_name (fst data)))
          data.s_list;
        Stdio.print_endline "")
      x

  let find (ds : string int_struct) (v : 'a DLL.node) = Hashtbl.find_exn ds v

  let initialization ?(print = false) graph : 'a data_struct =
    let graph = Arc_consistency.clean_domains ~print graph in
    let data_struct : 'a int_struct = Hashtbl.create (module Graph.Value) in

    let make_empty_cell () =
      { s_list = DLL.empty ""; last = Hashtbl.create (module Graph.Domain) }
    in
    Graph.loop_domains
      (DLL.iter (fun v ->
           Hashtbl.add_exn data_struct ~key:v ~data:(make_empty_cell ())))
      graph;

    let exception Found in
    Graph.loop_domains
      (fun d1 ->
        DLL.iter
          (fun v1 ->
            let ds1 = find data_struct v1 in
            DLL.iter_value
              (fun (d2 : string Graph.domain) ->
                try
                  DLL.iter
                    (fun v2 ->
                      if Graph.relation graph v1 v2 then (
                        let ds2 = find data_struct v2 in
                        DLL.append (v1, ds1) ds2.s_list;
                        Hashtbl.add_exn ~key:d2 ~data:(DLL.singleton "" v2)
                          ds1.last;
                        raise Found))
                    d2
                with Found -> ())
              (Graph.get_constraint_binding graph d1))
          d1)
      graph;
    (graph, data_struct)

  let revise (node_to_remove : string Graph.value)
      ((graph, int_str) : 'a data_struct) :
      string stack_operation * string remove_in_domain =
    let node = Hashtbl.find_exn int_str node_to_remove in
    let removed_in_domain = ref [] in
    let s_list_modif = ref [] in
    let last_modif = ref [] in
    let father = node_to_remove.father in
    DLL.iter_value
      (fun ((value, { last; _ }) as supported) ->
        match
          DLL.find_from_next (Graph.relation graph value) node_to_remove
        with
        | None -> removed_in_domain := value :: !removed_in_domain
        | Some next_supp ->
            let last = Hashtbl.find_exn last father in
            let new_last = (Hashtbl.find_exn int_str next_supp).s_list in
            DLL.append supported new_last;
            DLL.prepend next_supp last;
            s_list_modif := DLL.get_last new_last :: !s_list_modif;
            last_modif := DLL.get_first last :: !last_modif)
      node.s_list;
    ((!s_list_modif, !last_modif), !removed_in_domain)

  let back_track (appended_in_support, modif_last) =
    List.iter ~f:DLL.remove appended_in_support;
    List.iter ~f:DLL.remove modif_last
end

include AC_6