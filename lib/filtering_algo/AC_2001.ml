module AC_2001 : Arc_consistency.Arc_consistency = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a remove_in_domain = string DLL.dll_node list

  type 'a map_value =
    'a DLL.dll_node * (string, 'a DLL.dll_node DLL.t) Hashtbl.t

  type 'a last = (int, 'a map_value) Hashtbl.t
  type 'a data_struct = { last : 'a last; graph : 'a Constraint.graph }

  type 'a stack_operation = {
    to_remove_in_domain : 'a remove_in_domain;
    undo_assoc : 'a DLL.dll_node DLL.dll_node list;
    input : 'a DLL.dll_node;
  }

  let print_data_struct ({ last; _ } : 'a data_struct) =
    Hashtbl.iter
      (fun _ ((e, v) : 'a map_value) ->
        Printf.printf "node : %s, support : " (Arc_consistency.make_name e);
        Hashtbl.iter
          (fun _ (v : 'a DLL.dll_node DLL.t) ->
            Printf.printf "%s "
              (Arc_consistency.make_name (DLL.get_first v).value))
          v;
        print_endline "")
      last

  let initialization ?(print = false) (graph : 'a Constraint.graph) :
      'a data_struct =
    let exception Found in
    let graph = Arc_consistency.clean_domains ~print graph in
    let last : string last = Hashtbl.create 1024 in
    let domain_list = Hashtbl.to_seq_values graph.domains |> List.of_seq in
    let add_compteur (v : 'a DLL.dll_node) =
      Hashtbl.add last v.id (v, Hashtbl.create 2048)
    in
    (* Add all values of every domains to the data_struct *)
    Hashtbl.iter (fun _ dom -> DLL.iter add_compteur dom) graph.domains;

    (* Add all values of every domains to the data_struct *)
    List.iter
      (DLL.iter (fun v1 ->
           List.iter
             (fun d2 ->
               try
                 (DLL.iter (fun v2 ->
                      if graph.relation v1 v2 then (
                        Hashtbl.add
                          (snd (Hashtbl.find last v1.id))
                          v2.dll_father.name (DLL.singleton "" v2);
                        raise Found)))
                   d2
               with Found -> ())
             domain_list))
      domain_list;
    { last; graph }

  (** For each value v in d1 it should exist a support in d2, otherwise we remove v from d1,
  returns the list of filtered values.
  If the list is empty, then no modification has been performed on d1 *)
  let revise (v1 : 'a DLL.dll_node) ({ last; graph } : 'a data_struct) =
    let to_remove_in_domain : 'a DLL.dll_node list ref = ref [] in
    let undo_assoc = ref [] in
    DLL.iter_value
      (DLL.iter (fun v2 ->
           let _, last_supp = Hashtbl.find last v2.id in
           let old =
             DLL.get_first (Hashtbl.find last_supp v1.dll_father.name)
           in
           if old.value == v1 then
             match DLL.find_from_next (graph.relation v2) v1 with
             | None -> to_remove_in_domain := v2 :: !to_remove_in_domain
             | Some next ->
                 DLL.prepend next old.dll_father;
                 undo_assoc := DLL.get_first old.dll_father :: !undo_assoc))
      (Constraint.get_constraint_binding graph v1.dll_father);
    {
      input = v1;
      to_remove_in_domain = !to_remove_in_domain;
      undo_assoc = !undo_assoc;
    }

  let back_track { input; undo_assoc; _ } =
    List.iter DLL.remove undo_assoc;
    DLL.insert input

  let get_to_remove { to_remove_in_domain; _ } = to_remove_in_domain
end

include AC_2001