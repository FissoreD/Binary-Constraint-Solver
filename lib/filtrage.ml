module type Algo_Filtrage = sig
  module DLL = DoublyLinkedList

  type 'a compteurs
  type 'a stack_operation
  type 'a remove_in_domain = 'a DLL.dll_node list

  val print_compteurs : int compteurs -> unit
  val build_support : 'a Constraint.supports -> 'a compteurs
  val revise : 'a DLL.dll_node -> 'a compteurs -> 'a stack_operation
  val back_track : 'a stack_operation -> unit
  val get_to_remove : 'a stack_operation -> 'a remove_in_domain
end

module Make (AF : Algo_Filtrage) = struct
  module DLL = DoublyLinkedList

  let support = ref None
  let graph = ref None
  let get_support () = Option.get !support
  let get_graph () = Option.get !graph
  let stack_op : 'a AF.stack_operation Queue.t = Queue.create ()
  let add_stack = (Fun.flip Queue.add) stack_op
  let get_op () = Queue.take stack_op

  let build_support ?(verbose = false) graph' =
    support := Some (AF.build_support graph');
    graph := Some graph';
    if verbose then AF.print_compteurs (get_support ())

  let print_list_nodes l =
    List.iter (fun (e : int DLL.dll_node) -> Printf.printf "%d " e.value) l;
    print_newline ()

  (** Returns if a fail has occured, i.e. if there is an empty domain among those that have been modified *)
  let remove_by_value ?(verbose = false) value domain =
    match DLL.remove_by_value value domain with
    | None -> false
    | Some value_in_domain ->
        if DLL.is_empty domain then true
        else
          (* TODO: in revise stop if a domain becomes empty !! *)
          let filtered = AF.revise value_in_domain (get_support ()) in

          List.iter DLL.remove (AF.get_to_remove filtered);

          add_stack filtered;
          if verbose then (
            print_string "removed = ";
            print_list_nodes (AF.get_to_remove filtered));
          List.exists
            (fun (e : 'a DLL.dll_node) -> DLL.is_empty e.dll_father)
            (AF.get_to_remove filtered)

  let back_track () =
    if not (Queue.is_empty stack_op) then get_op () |> AF.back_track

  let print_compteurs () =
    print_endline "-- Start Compteurs --";
    AF.print_compteurs (get_support ());
    print_endline "--- End Compteurs ---"

  let print_domains () = Constraint.print_int_domains (get_graph ())
end
