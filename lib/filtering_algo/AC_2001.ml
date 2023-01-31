module AC_2001 : Arc_consistency.Arc_consistency = struct
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a remove_in_domain = string DLL.dll_node list
  type 'a last = (string, 'a DLL.dll_node DLL.t) Hashtbl.t
  type 'a data_struct = 'a last * 'a Constraint.graph

  type 'a stack_operation = {
    to_remove_in_domain : 'a DLL.dll_node list;
    undo_assoc :
      ('a DLL.dll_node DLL.dll_node * 'a DLL.dll_node DLL.dll_node) list;
    input : 'a DLL.dll_node;
  }

  let make_name (node : 'a DLL.dll_node) =
    Printf.sprintf "%s,%s" node.dll_father.name node.value

  let print_data_struct ((last, _) : 'a data_struct) =
    Hashtbl.iter
      (fun k v ->
        Printf.printf "node : %s, support : %s\n" k
          (DLL.to_list v
          |> List.map (fun (v : 'a DLL.dll_node) -> v.value)
          |> List.fold_left (Printf.sprintf "%s - %s") ""))
      last

  let initialization (graph : 'a Constraint.graph) : 'a data_struct =
    let exception Found in
    let data_struct : string last = Hashtbl.create 1024 in
    let domain_list = Hashtbl.to_seq_values graph.domains |> List.of_seq in
    let add_compteur v = Hashtbl.add data_struct (make_name v) (DLL.empty "") in
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
                        DLL.append v2 (Hashtbl.find data_struct (make_name v1))
                        |> ignore;
                        raise Found)))
                   d2
               with Found -> ())
             domain_list))
      domain_list;
    (data_struct, graph)

  (** For each value v in d1 it should exist a support in d2, otherwise we remove v from d1,
  returns the list of filtered values.
  If the list is empty, then no modification has been performed on d1 *)
  let revise (v1 : 'a DLL.dll_node) ((last_map, graph) : 'a data_struct) =
    let rec find_assoc_from (v1 : 'a DLL.dll_node) (v2 : 'a DLL.dll_node) =
      match v1.next with
      | None -> None
      | Some v1' when graph.relation v1' v2 -> Some v1'
      | Some v1' -> find_assoc_from v1' v2
    in
    let to_remove_in_domain : 'a DLL.dll_node list ref = ref [] in
    let undo_assoc :
        ('a DLL.dll_node DLL.dll_node * 'a DLL.dll_node DLL.dll_node) list ref =
      ref []
    in
    DLL.iter_value
      (fun d ->
        DLL.iter
          (fun v2 ->
            let v2_name = make_name v2 in
            match Hashtbl.find_opt last_map v2_name with
            | None -> to_remove_in_domain := v2 :: !to_remove_in_domain
            | Some last -> (
                match
                  DLL.find
                    (fun (e : 'a DLL.dll_node DLL.dll_node) ->
                      e.value.dll_father == v1.dll_father)
                    last
                with
                | None -> to_remove_in_domain := v2 :: !to_remove_in_domain
                | Some old when old.value == v1 -> (
                    match find_assoc_from v1 v2 with
                    | None -> to_remove_in_domain := v2 :: !to_remove_in_domain
                    | Some next ->
                        DLL.remove old;
                        let appended = DLL.append next old.dll_father in
                        undo_assoc := (appended, old) :: !undo_assoc)
                | _ -> ()))
          d)
      (Constraint.get_constraint_binding graph v1.dll_father);
    {
      input = v1;
      to_remove_in_domain = !to_remove_in_domain;
      undo_assoc = !undo_assoc;
    }

  let back_track { to_remove_in_domain; input; undo_assoc } =
    List.iter
      (fun (v1, v2) ->
        DLL.remove v1;
        DLL.insert v2)
      undo_assoc;
    List.iter DLL.insert to_remove_in_domain;
    DLL.insert input

  let get_to_remove { to_remove_in_domain; _ } = to_remove_in_domain
end

include AC_2001