module type M = sig
  module DLL = DoublyLinkedList

  val initialization : ?verbose:bool -> string Constraint.graph -> unit
  val print_data_struct : unit -> unit
  val print_list_nodes : string DLL.dll_node list -> unit
  val print_domains : ?is_rev:bool -> unit -> unit
  val print_domain_stats : unit -> unit
  val remove_by_value : ?verbose:bool -> string -> string -> unit
  val propagation_remove_by_value : ?verbose:bool -> string -> string -> unit
  val propagation_select_by_value : ?verbose:bool -> string -> string -> unit
  val back_track_remove : unit -> unit
  val back_track_select : unit -> unit

  val find_solution :
    ?debug:bool ->
    ?count_only:bool ->
    ?verbose:bool ->
    ?one_sol:bool ->
    unit ->
    unit
end

module Make (AF : Arc_consistency.Arc_consistency) : M = struct
  module DLL = DoublyLinkedList

  exception Empty_domain

  let support = ref None
  let graph = ref None
  let get_data_struct () = Option.get !support
  let get_graph () = Option.get !graph
  let stack_op : 'a AF.stack_operation Stack.t = Stack.create ()
  let stack_remove_nb : int Stack.t = Stack.create ()
  let stack_select_nb : int Stack.t = Stack.create ()
  let counter_remove_prov = ref 0
  let counter_select_prov = ref 0
  let add_stack = (Fun.flip Stack.push) stack_op
  let get_op () = Stack.pop stack_op

  let initialization ?(verbose = false) graph' =
    support := Some (AF.initialization graph');
    graph := Some graph';
    if verbose then AF.print_data_struct (get_data_struct ())

  let to_str_node_list l =
    let to_str (e : 'a DLL.dll_node) =
      Printf.sprintf "(%s,%s)" e.dll_father.name e.value
    in
    let append a b = Printf.sprintf "%s, %s" a (to_str b) in
    let rec aux = function
      | [] -> ""
      | [ hd ] -> to_str hd
      | hd :: tl -> append (aux tl) hd
    in
    aux l

  let print_list_nodes l = print_endline (to_str_node_list l)

  let print_data_struct () =
    print_endline "-- Start data_struct --";
    AF.print_data_struct (get_data_struct ());
    print_endline "--- End data_struct ---"

  let print_domains ?(is_rev = false) () =
    Constraint.print_string_domains ~is_rev (get_graph ())

  let print_domain_stats () =
    Printf.printf "There are %d domains\n"
      (Hashtbl.length (get_graph ()).domains);
    Hashtbl.iter
      (fun k v ->
        let neigh_domains =
          List.fold_left
            (fun acc (e : string DLL.t) -> Printf.sprintf "%s,%s" e.name acc)
            "" (DLL.to_list v)
        in
        Printf.printf "%s is linked to %d domains. They are : %s\n" k
          (DLL.length v) neigh_domains)
      (get_graph ()).constraint_binding

  let remove_by_node ?(verbose = false) (node : 'a DLL.dll_node) =
    if verbose then
      Printf.printf " * Removing %s from %s\n" node.value node.dll_father.name;
    DLL.remove node;
    let domain = node.dll_father in
    if DLL.is_empty domain then raise Empty_domain;
    let filtered = AF.revise node (get_data_struct ()) in
    add_stack filtered;
    if verbose then (
      print_string "List of values having no more support = ";
      print_list_nodes (AF.get_to_remove filtered))

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

  let propagation_remove_by_node ?(verbose = false) node =
    counter_remove_prov := 0;
    let rec aux (to_remove : string DLL.dll_node) =
      remove_by_node ~verbose to_remove;
      incr counter_remove_prov;
      let last_push = AF.get_to_remove (Stack.top stack_op) in
      List.iter aux last_push
    in
    aux node;
    Stack.push !counter_remove_prov stack_remove_nb

  let propagation_remove_by_value ?(verbose = false) value domain_name =
    let domain = Hashtbl.find (get_graph ()).domains domain_name in
    match DLL.find (fun e -> e.value = value) domain with
    | None ->
        invalid_arg
          ("Propagation remove: " ^ value ^ " is not in " ^ domain_name)
    | Some node -> propagation_remove_by_node ~verbose node

  let propagation_select_by_node ?(verbose = false) (v : 'a DLL.dll_node) =
    if verbose then
      MyPrint.print_color_str "gray"
        (Printf.sprintf "--> Selecting %s from %s" v.value v.dll_father.name);
    counter_select_prov := 0;
    DLL.iter
      (fun (v' : 'a DLL.dll_node) ->
        if v' != v then (
          propagation_remove_by_node ~verbose v';
          incr counter_select_prov))
      v.dll_father;
    Stack.push !counter_select_prov stack_select_nb;
    if verbose then print_endline "<-- End selecting"

  let propagation_select_by_value ?(verbose = false) value domain_name =
    let domain = Hashtbl.find (get_graph ()).domains domain_name in
    match DLL.find (fun e -> e.value = value) domain with
    | None ->
        invalid_arg
          ("Propagation select: " ^ value ^ " is not in " ^ domain_name)
    | Some node -> propagation_select_by_node ~verbose node

  let back_track_remove () =
    for _ = Stack.pop stack_remove_nb downto 1 do
      AF.back_track (get_op ())
    done

  let back_track_select () =
    for _ = Stack.pop stack_select_nb downto 1 do
      back_track_remove ()
    done

  let find_solution ?(debug = false) ?(count_only = false) ?(verbose = false)
      ?(one_sol = false) () =
    debug |> ignore;
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
      MyPrint.print_color_str "blue"
        ("A solution : " ^ to_str_node_list sol ^ " !!")
    in
    let rec aux sol : string DLL.t list -> unit = function
      | [] ->
          if not count_only then print_sol sol;
          incr number_of_solutions;
          if one_sol then raise Stop_One_Sol
      | hd :: tl ->
          DLL.iter
            (fun v ->
              let sol = v :: sol in
              try
                (* print_data_struct ();
                   read_line () |> ignore; *)
                propagation_select_by_node ~verbose v;
                aux sol tl;
                back_track_select ()
              with Empty_domain ->
                for _ = !counter_select_prov downto 1 do
                  back_track_remove ()
                done;
                for _ = !counter_remove_prov downto 1 do
                  AF.back_track (get_op ())
                done;
                incr number_of_fails;
                if not count_only then print_fail sol)
            hd
    in
    (try aux [] domains with Stop_One_Sol -> ());
    print_endline "------------------------------";
    MyPrint.print_color_str "red"
      (Printf.sprintf "The number of fails is %d" !number_of_fails);
    MyPrint.print_color_str "green"
      (Printf.sprintf "The number of solutions is %d" !number_of_solutions);
    print_endline "------------------------------"
end
