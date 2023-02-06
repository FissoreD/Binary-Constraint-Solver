open Base

module AC_2001 = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a remove_in_domain = string Graph.value list
  type 'a map_value = 'a Graph.value * 'a Graph.value DLL.t
  type 'a last = (int * int, 'a map_value) Hashtbl.t
  type 'a data_struct = { last : 'a last; graph : 'a Graph.graph }
  type 'a stack_operation = 'a Graph.value DLL.node list

  let name = "AC-2001"

  let print_data_struct ({ last; _ } : 'a data_struct) =
    Hashtbl.iter
      ~f:(fun ((e, v) : 'a map_value) ->
        Stdio.printf "node : %s, support : " (Arc_consistency.make_name e);
        DLL.iter_value
          (fun (v : 'a Graph.value) ->
            Stdio.printf "%s " (Arc_consistency.make_name v))
          v;
        Stdio.print_endline "")
      last

  let initialization ?(print = false) (graph : 'a Graph.graph) =
    let exception Found in
    let graph = Arc_consistency.clean_domains ~print graph in
    let last : string last = Hashtbl.create ~size:2048 (module Tuple) in

    let domain_list = Graph.list_domains graph in
    List.iter
      ~f:
        (DLL.iter (fun v1 ->
             List.iter
               ~f:(fun d2 ->
                 try
                   (DLL.iter (fun v2 ->
                        if Graph.relation graph v1 v2 then (
                          Hashtbl.add_exn last ~key:(v1.id, v2.father.id_dom)
                            ~data:(v1, DLL.singleton "" v2);
                          raise Found)))
                     d2
                 with Found -> ())
               domain_list))
      domain_list;
    { last; graph }

  let revise (v1 : 'a Graph.value) ({ last; graph } : 'a data_struct) =
    let to_remove_in_domain : 'a Graph.value list ref = ref [] in
    let undo_assoc = ref [] in
    DLL.iter_value
      (DLL.iter (fun v2 ->
           let last_value =
             DLL.get_first
               (snd (Hashtbl.find_exn last (v2.id, v1.father.id_dom)))
           in
           if phys_equal last_value.value v1 then
             match DLL.find_from_next (Graph.relation graph v2) v1 with
             | None -> to_remove_in_domain := v2 :: !to_remove_in_domain
             | Some next ->
                 DLL.prepend next last_value.father;
                 undo_assoc := DLL.get_first last_value.father :: !undo_assoc))
      (Graph.get_constraint_binding graph v1.father);
    (!undo_assoc, !to_remove_in_domain)

  let back_track undo_assoc = List.iter ~f:DLL.remove undo_assoc
end

include AC_2001