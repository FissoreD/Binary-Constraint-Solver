open Base

module AC_6 = struct
  module DLL = DoublyLinkedList

  type 'a remove_in_domain = string Graph.value list
  type 'a cell = { s_list : 'a Graph.value DLL.t; last : 'a Graph.value DLL.t }

  type 'a int_struct =
    ('a Graph.value, ('a Graph.domain, 'a cell) Hashtbl.t) Hashtbl.t

  type 'a data_struct = 'a Graph.graph * 'a int_struct

  type 'a stack_operation =
    'a Graph.value DLL.node list * 'a Graph.value DLL.node list

  let find (ds : 'a int_struct) v1 d2 =
    Hashtbl.find_exn (Hashtbl.find_exn ds v1) d2

  let print_data_struct ((_, x) : 'a data_struct) : unit =
    Hashtbl.iteri
      ~f:(fun ~key ~data ->
        Stdio.printf "%s last is : " (Arc_consistency.make_name key);
        Hashtbl.iteri
          ~f:(fun ~key:_ ~data ->
            Stdio.printf "%s "
              (Arc_consistency.make_name (DLL.get_first data.last).value))
          data;
        Stdio.printf "and the s list is : ";
        Hashtbl.iteri
          ~f:(fun ~key:_ ~data ->
            DLL.iter_value
              (fun e -> Stdio.printf "%s " (Arc_consistency.make_name e))
              data.s_list)
          data;
        Stdio.print_endline "")
      x

  let initialization ?(verbose = false) graph : 'a data_struct =
    let graph = Arc_consistency.clean_domains ~verbose graph in
    let data_struct : 'a int_struct = Hashtbl.create (module Graph.Value) in
    let add_compteur (v : 'a Graph.value) =
      Hashtbl.add_exn data_struct ~key:v
        ~data:
          (let tbl = Hashtbl.create (module Graph.Domain) in
           DLL.iter_value
             (fun d ->
               Hashtbl.add_exn tbl ~key:d
                 ~data:{ s_list = DLL.empty ""; last = DLL.empty "" })
             (Graph.get_constraint_binding graph v.father);
           tbl)
    in
    Graph.loop_domains (fun dom -> DLL.iter add_compteur dom) graph;

    let exception Found in
    Graph.loop_domains
      (fun d1 ->
        DLL.iter
          (fun v1 ->
            DLL.iter_value
              (fun (d2 : string Graph.domain) ->
                try
                  let ds1 = find data_struct v1 d2 in
                  DLL.iter
                    (fun v2 ->
                      if Graph.relation graph v1 v2 then (
                        let ds2 = find data_struct v2 d1 in
                        DLL.append v1 ds2.s_list;
                        DLL.prepend v2 ds1.last;
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
    let appended_in_support = ref [] in
    let modif_last = ref [] in
    Hashtbl.iter
      ~f:(fun (supp_dom : string cell) ->
        DLL.iter_value
          (fun supported ->
            let first = find int_str supported node_to_remove.father in
            if phys_equal node_to_remove (DLL.get_first first.last).value then
              match
                DLL.find_from_next
                  (Graph.relation graph supported)
                  node_to_remove
              with
              | None -> removed_in_domain := supported :: !removed_in_domain
              | Some e ->
                  let dom = find int_str e supported.father in
                  DLL.append supported dom.s_list;
                  DLL.prepend e first.last;
                  appended_in_support :=
                    DLL.get_last dom.s_list :: !appended_in_support;
                  modif_last := DLL.get_first first.last :: !modif_last)
          supp_dom.s_list)
      node;
    ((!appended_in_support, !modif_last), !removed_in_domain)

  let back_track (appended_in_support, modif_last) =
    List.iter ~f:DLL.remove appended_in_support;
    List.iter ~f:DLL.remove modif_last
end

include AC_6