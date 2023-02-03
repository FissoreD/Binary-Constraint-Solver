open Base

module AC_2001 = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a remove_in_domain = string DLL.dll_node list
  type 'a map_value = 'a DLL.dll_node * (int, 'a DLL.dll_node DLL.t) Hashtbl.t
  type 'a last = (int, 'a map_value) Hashtbl.t
  type 'a data_struct = { last : 'a last; graph : 'a Constraint.graph }
  type 'a stack_operation = 'a DLL.dll_node DLL.dll_node list

  let name = "AC-2001"

  let print_data_struct ({ last; _ } : 'a data_struct) =
    Hashtbl.iter
      ~f:(fun ((e, v) : 'a map_value) ->
        Stdio.printf "node : %s, support : " (Arc_consistency.make_name e);
        Hashtbl.iter
          ~f:(fun (v : 'a DLL.dll_node DLL.t) ->
            Stdio.printf "%s "
              (Arc_consistency.make_name (DLL.get_first v).value))
          v;
        Stdio.print_endline "")
      last

  let initialization ?(print = false) (graph : 'a Constraint.graph) =
    let exception Found in
    let graph = Arc_consistency.clean_domains ~print graph in
    let last : string last = Hashtbl.create ~size:2048 (module Int) in
    let domain_list = Constraint.list_domains graph in
    let add_compteur (v : 'a DLL.dll_node) =
      Hashtbl.add_exn last ~key:v.id
        ~data:(v, Hashtbl.create ~size:2048 (module Int))
    in
    (* Add all values of every domains to the data_struct *)
    Constraint.loop_domains (fun dom -> DLL.iter add_compteur dom) graph;

    (* Add all values of every domains to the data_struct *)
    List.iter
      ~f:
        (DLL.iter (fun v1 ->
             List.iter
               ~f:(fun d2 ->
                 try
                   (DLL.iter (fun v2 ->
                        if Constraint.relation graph v1 v2 then (
                          Hashtbl.add_exn
                            (snd (Hashtbl.find_exn last v1.id))
                            ~key:v2.dll_father.id_dom
                            ~data:(DLL.singleton "" v2);
                          raise Found)))
                     d2
                 with Found -> ())
               domain_list))
      domain_list;
    { last; graph }

  let revise (v1 : 'a DLL.dll_node) ({ last; graph } : 'a data_struct) =
    let to_remove_in_domain : 'a DLL.dll_node list ref = ref [] in
    let undo_assoc = ref [] in
    DLL.iter_value
      (DLL.iter (fun v2 ->
           let last_value =
             DLL.get_first
               (Hashtbl.find_exn
                  (snd (Hashtbl.find_exn last v2.id))
                  v1.dll_father.id_dom)
           in
           if phys_equal last_value.value v1 then
             match DLL.find_from_next (Constraint.relation graph v2) v1 with
             | None -> to_remove_in_domain := v2 :: !to_remove_in_domain
             | Some next ->
                 DLL.prepend next last_value.dll_father;
                 undo_assoc :=
                   DLL.get_first last_value.dll_father :: !undo_assoc))
      (Constraint.get_constraint_binding graph v1.dll_father);
    (!undo_assoc, !to_remove_in_domain)

  let back_track undo_assoc = List.iter ~f:DLL.remove undo_assoc
end

include AC_2001