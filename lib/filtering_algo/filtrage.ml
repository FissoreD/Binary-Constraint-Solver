module type Algo_Filtrage = sig
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a compteurs
  type 'a stack_operation
  type 'a remove_in_domain = string DLL.dll_node list

  val print_compteurs : string compteurs -> unit
  val build_support : string Constraint.supports -> string compteurs
  val revise : string DLL.dll_node -> string compteurs -> string stack_operation
  val back_track : string stack_operation -> unit
  val get_to_remove : string stack_operation -> string remove_in_domain
end

module Make (AF : Algo_Filtrage) = struct
  module DLL = DoublyLinkedList

  let support = ref None
  let graph = ref None
  let get_support () = Option.get !support
  let get_graph () = Option.get !graph
  let stack_op : 'a AF.stack_operation Stack.t = Stack.create ()
  let stack_back_nb : int Stack.t = Stack.create ()
  let add_stack = (Fun.flip Stack.push) stack_op
  let get_op () = Stack.pop stack_op

  let build_support ?(verbose = false) graph' =
    support := Some (AF.build_support graph');
    graph := Some graph';
    if verbose then AF.print_compteurs (get_support ())

  let print_list_nodes l =
    List.iter (fun (e : string DLL.dll_node) -> Printf.printf "%s " e.value) l;
    print_newline ()

  let remove_by_node ?(verbose = false) (node : 'a DLL.dll_node) =
    if verbose then
      Printf.printf "Removing %s from %s\n" node.value node.dll_father.name;
    DLL.remove node;
    let domain = node.dll_father in
    if DLL.is_empty domain then invalid_arg "The domain should not be emtpy !";
    (* TODO: in revise stop if a domain becomes empty !! *)
    let filtered = AF.revise node (get_support ()) in
    List.iter DLL.remove (AF.get_to_remove filtered);
    add_stack filtered;
    if verbose then (
      print_string "List of values having no more support = ";
      print_list_nodes (AF.get_to_remove filtered));
    List.exists
      (fun (e : 'a DLL.dll_node) -> DLL.is_empty e.dll_father)
      (AF.get_to_remove filtered)

  (** Returns if a fail has occured, i.e. if there is an empty domain among those that have been modified and the list of removed values *)
  let remove_by_value ?(verbose = false) value domain =
    match DLL.remove_by_value value domain with
    | None ->
        invalid_arg
          (Printf.sprintf "The value %s does not exists in the domain %s" value
             domain.name)
    | Some value_in_domain -> remove_by_node ~verbose value_in_domain

  let propagation ?(verbose = false) value domain =
    match DLL.find (fun e -> e.value = value) domain with
    | None ->
        invalid_arg
          (Printf.sprintf "Propagation: %s is not in %s" value domain.name)
    | Some node ->
        let cnt = ref 0 in
        let rec aux (to_remove : string DLL.dll_node) =
          incr cnt;
          let stop = remove_by_node ~verbose to_remove in
          if stop then invalid_arg "There exists an empty domain !";

          let last_push = AF.get_to_remove (Stack.top stack_op) in
          List.iter aux last_push
        in
        aux node;
        Stack.push !cnt stack_back_nb

  let back_track () =
    for _ = Stack.pop stack_back_nb downto 1 do
      get_op () |> AF.back_track
    done

  let print_compteurs () =
    print_endline "-- Start Compteurs --";
    AF.print_compteurs (get_support ());
    print_endline "--- End Compteurs ---"

  let print_domains () = Constraint.print_string_domains (get_graph ())
end
