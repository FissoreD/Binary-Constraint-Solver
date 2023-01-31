module type M = sig
  module DLL = DoublyLinkedList

  val initialization : ?verbose:bool -> string Constraint.graph -> unit

  val find_solution :
    ?debug:bool ->
    ?count_only:bool ->
    ?verbose:bool ->
    ?one_sol:bool ->
    unit ->
    unit

  val remove_by_value : ?verbose:bool -> string -> string -> unit
  val propagation_remove_by_value : ?verbose:bool -> string -> string -> unit
  val propagation_select_by_value : ?verbose:bool -> string -> string -> unit
end

module Make (AF : Arc_consistency.Arc_consistency) : M = struct
  module DLL = DoublyLinkedList

  exception Empty_domain

  let support = ref None
  let graph = ref None
  let get_data_struct () = Option.get !support
  let get_graph () = Option.get !graph

  let stack_op : 'a AF.stack_operation option Stack.t =
    let stack = Stack.create () in
    Stack.push None stack;
    stack

  let stack_remove_nb : 'a AF.stack_operation option Stack.t = Stack.create ()

  (* let counter_remove_prov = ref 0 *)
  let add_stack q = Stack.push (Some q) stack_op
  let get_op () = Option.get (Stack.pop stack_op)

  let to_str_node_list l =
    let to_str (e : 'a DLL.dll_node) =
      Printf.sprintf "(%s,%s)" e.dll_father.name e.value
    in
    let append a b = Printf.sprintf "%s, %s" (to_str b) a in
    let rec aux = function
      | [] -> ""
      | [ hd ] -> to_str hd
      | hd :: tl -> append (aux tl) hd
    in
    "[" ^ aux l ^ "]"

  let print_domains ?(is_rev = false) () =
    Constraint.print_string_domains ~is_rev (get_graph ())

  let print_list_nodes l = print_endline (to_str_node_list l)

  let initialization ?(verbose = false) graph' =
    support := Some (AF.initialization graph');
    graph := Some graph';
    if verbose then AF.print_data_struct (get_data_struct ())

  let remove_by_node ?(verbose = false) (node : 'a DLL.dll_node) =
    if node.is_in then (
      DLL.remove node;
      let unsupported = AF.revise node (get_data_struct ()) in
      add_stack @@ unsupported;
      if verbose then (
        MyPrint.print_color_str "red"
          (Printf.sprintf " * Removing %s from %s" node.value
             node.dll_father.name);
        print_string "List of values having no more support = ";
        print_list_nodes (AF.get_to_remove unsupported));
      if DLL.is_empty node.dll_father then raise Empty_domain)

  let rec propagation_remove_by_node ?(verbose = false) node =
    remove_by_node ~verbose node;
    let list_to_remove = Stack.top stack_op |> Option.get in
    List.iter
      (propagation_remove_by_node ~verbose)
      (AF.get_to_remove list_to_remove)

  let propagation_select_by_node ?(verbose = false) (v : 'a DLL.dll_node) =
    if verbose then
      MyPrint.print_color_str "blue"
        (Printf.sprintf "--> Selecting %s from %s" v.value v.dll_father.name);
    Stack.push (Stack.top stack_op) stack_remove_nb;
    DLL.iter
      (fun v' -> if v' != v then propagation_remove_by_node ~verbose v')
      v.dll_father

  let back_track_remove () =
    let top = Stack.pop stack_remove_nb in
    while top != Stack.top stack_op do
      AF.back_track (get_op ())
    done

  let find_solution ?(debug = false) ?(count_only = false) ?(verbose = false)
      ?(one_sol = false) () =
    let exception Stop_One_Sol in
    let domains =
      Hashtbl.to_seq_values (get_graph ()).domains
      |> List.of_seq |> List.sort compare
    in
    let number_of_fails = ref 0 in
    let number_of_solutions = ref 0 in
    let print_fail sol =
      MyPrint.print_color_str "red" ("A fail : " ^ to_str_node_list sol ^ " ...")
    in
    let print_sol sol =
      MyPrint.print_color_str "green"
        ("A solution : " ^ to_str_node_list sol ^ " !!")
    in
    let time = Sys.time () in
    let rec aux sol : string DLL.t list -> unit = function
      | [] ->
          if not count_only then print_sol sol;
          if one_sol then raise Stop_One_Sol;
          incr number_of_solutions
      | hd :: tl ->
          DLL.iter
            (fun v ->
              let sol = v :: sol in
              if debug then print_domains ();
              (try
                 propagation_select_by_node ~verbose v;
                 aux sol tl
               with Empty_domain ->
                 incr number_of_fails;
                 if not count_only then print_fail sol);
              back_track_remove ())
            hd
    in
    (try aux [] domains with Stop_One_Sol -> ());
    print_endline "------------------------------";
    MyPrint.print_color_str "red"
      (Printf.sprintf "The number of fails is %d" !number_of_fails);
    MyPrint.print_color_str "green"
      (Printf.sprintf "The number of solutions is %d" !number_of_solutions);
    MyPrint.print_color_str "gray"
      (Printf.sprintf "Time: %f" (Sys.time () -. time));
    print_endline "------------------------------"

  (** Returns if a fail has occured, i.e. if there is an empty domain among those that have been modified and the list of removed values *)
  let remove_by_value ?(verbose = false) value domain_name =
    match
      DLL.find_by_value value (Constraint.get_domain (get_graph ()) domain_name)
    with
    | None ->
        invalid_arg
          (Printf.sprintf "The value %s does not exists in the domain %s" value
             domain_name)
    | Some value_in_domain -> remove_by_node ~verbose value_in_domain

  let propagation_remove_by_value ?(verbose = false) value domain_name =
    let domain = Hashtbl.find (get_graph ()).domains domain_name in
    match DLL.find (fun e -> e.value = value) domain with
    | None ->
        invalid_arg
          ("Propagation remove: " ^ value ^ " is not in " ^ domain_name)
    | Some node -> propagation_remove_by_node ~verbose node

  let propagation_select_by_value ?(verbose = false) value domain_name =
    let domain = Hashtbl.find (get_graph ()).domains domain_name in
    match DLL.find (fun e -> e.value = value) domain with
    | None ->
        invalid_arg
          ("Propagation select: " ^ value ^ " is not in " ^ domain_name)
    | Some node -> propagation_select_by_node ~verbose node
end
