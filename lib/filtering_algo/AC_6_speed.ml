open Base

module AC_6 = struct
  module DLL = DoublyLinkedList

  type 'a remove_in_domain = string Graph.value list

  type 'a cell = {
    s_list : ('a Graph.value * 'a cell) DLL.t;
    last : ('a Graph.domain, ('a Graph.value * 'a cell) DLL.t) Hashtbl.t;
  }

  type 'a int_struct =
    ('a Graph.domain, ('a Graph.value * 'a cell) DLL.t) Hashtbl.t

  type 'a data_struct = 'a Graph.graph * 'a int_struct

  type 'a stack_operation =
    ('a Graph.value * 'a cell) DLL.node list
    * ('a Graph.value * 'a cell) DLL.node list
    * (string DLL.node * string cell) DLL.node list

  let print_data_struct ((_, x) : 'a data_struct) : unit =
    Hashtbl.iteri
      ~f:(fun ~key:_ ~data ->
        DLL.iter_value
          (fun ((key, data) : 'a Graph.value * 'a cell) ->
            Stdio.printf "%s last is : " (Arc_consistency.make_name key);
            Hashtbl.iter
              ~f:
                (DLL.iter_value (fun e ->
                     Stdio.printf "%s " (Arc_consistency.make_name (fst e))))
              data.last;
            Stdio.printf "and the s list is : ";
            DLL.iter_value
              (fun data ->
                Stdio.printf "%s " (Arc_consistency.make_name (fst data)))
              data.s_list;
            Stdio.print_endline "")
          data)
      x

  let find (ds : string int_struct) (v : 'a DLL.node) (d : 'a DLL.t) =
    (Hashtbl.find_exn ds d |> DLL.find_assoc (phys_equal v) |> Option.value_exn)
      .value

  let initialization ?(verbose = false) graph : 'a data_struct =
    let graph = Arc_consistency.clean_domains ~verbose graph in
    let data_struct : 'a int_struct = Hashtbl.create (module Graph.Domain) in

    let make_empty_cell () =
      { s_list = DLL.empty ""; last = Hashtbl.create (module Graph.Domain) }
    in
    Graph.loop_domains
      (fun dom ->
        Hashtbl.add_exn data_struct ~key:dom
          ~data:
            (let l = DLL.empty "" in
             DLL.iter (fun e -> DLL.append (e, make_empty_cell ()) l) dom;
             l))
      graph;

    let exception Found in
    Graph.loop_domains
      (fun d1 ->
        DLL.iter
          (fun v1 ->
            let ds1 = find data_struct v1 d1 in
            DLL.iter_value
              (fun (d2 : string Graph.domain) ->
                try
                  DLL.iter
                    (fun v2 ->
                      if Graph.relation graph v1 v2 then (
                        let ds2 = find data_struct v2 d2 in
                        DLL.append ds1 (snd ds2).s_list;
                        Hashtbl.add_exn ~key:d2 ~data:(DLL.singleton "" ds2)
                          (snd ds1).last;
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
    let node =
      Hashtbl.find_exn int_str node_to_remove.father
      |> DLL.find_assoc (phys_equal node_to_remove)
      |> Option.value_exn
    in
    let removed_in_domain = ref [] in
    let appended_in_support = ref [] in
    let removed_in_domain_bis = ref [ node ] in
    let modif_last = ref [] in
    DLL.remove node;
    DLL.iter
      (fun v ->
        let (value, supp_dom) : 'a Graph.value * 'a cell = v.value in
        let xx = Hashtbl.find_exn supp_dom.last node_to_remove.father in
        if phys_equal node_to_remove (fst (DLL.get_first xx).value) then
          match
            DLL.find_from_next
              (fun (e : (string DLL.node * string cell) DLL.node) ->
                (fst e.value).is_in && Graph.relation graph value (fst e.value))
              node
          with
          | None ->
              DLL.remove v;
              removed_in_domain_bis := v :: !removed_in_domain_bis;
              removed_in_domain := value :: !removed_in_domain
          | Some next_supp ->
              DLL.append v.value (snd next_supp.value).s_list;
              DLL.prepend next_supp.value xx;
              appended_in_support :=
                DLL.get_last (snd next_supp.value).s_list
                :: !appended_in_support;
              modif_last := DLL.get_first xx :: !modif_last)
      (snd node.value).s_list;
    ( (!appended_in_support, !modif_last, !removed_in_domain_bis),
      !removed_in_domain )

  let back_track (appended_in_support, modif_last, removed_in_domain_bis) =
    List.iter ~f:DLL.remove appended_in_support;
    List.iter ~f:DLL.remove modif_last;
    List.iter ~f:DLL.insert removed_in_domain_bis
end

include AC_6