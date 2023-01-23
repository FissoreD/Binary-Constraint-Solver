module type Algo_Filtrage = sig
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a compteurs
  type 'a stack_operation
  type 'a remove_in_domain = string DLL.dll_node list

  val print_compteurs : string compteurs -> unit
  val build_support : string Constraint.graph -> string compteurs
  val revise : string DLL.dll_node -> string compteurs -> string stack_operation
  val back_track : string stack_operation -> unit
  val get_to_remove : string stack_operation -> string remove_in_domain
end

module type M = sig
  module DLL = DoublyLinkedList

  val build_support : ?verbose:bool -> string Constraint.graph -> unit
  val print_list_nodes : string DLL.dll_node list -> unit
  val print_compteurs : unit -> unit
  val print_domains : ?is_rev:bool -> unit -> unit
  val print_domain_stats : unit -> unit
  val remove_by_value : ?verbose:bool -> string -> string -> unit
  val propagation_remove_by_value : ?verbose:bool -> string -> string -> unit
  val propagation_select_by_value : ?verbose:bool -> string -> string -> unit
  val back_track_remove : unit -> unit
  val back_track_select : unit -> unit
  val find_solution : ?verbose:bool -> unit -> unit
end

module Make (AF : Algo_Filtrage) : M = struct
  module DLL = DoublyLinkedList

  exception Empty_domain

  let support = ref None
  let graph = ref None
  let get_support () = Option.get !support
  let get_graph () = Option.get !graph
  let stack_op : 'a AF.stack_operation Stack.t = Stack.create ()
  let stack_remove_numb_nb : int Stack.t = Stack.create ()
  let stack_select_numb_nb : int Stack.t = Stack.create ()
  let counter_remove_prov = ref 0
  let counter_select_prov = ref 0
  let add_stack = (Fun.flip Stack.push) stack_op
  let get_op () = Stack.pop stack_op

  let build_support ?(verbose = false) graph' =
    support := Some (AF.build_support graph');
    graph := Some graph';
    if verbose then AF.print_compteurs (get_support ())

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

  let print_compteurs () =
    print_endline "-- Start Compteurs --";
    AF.print_compteurs (get_support ());
    print_endline "--- End Compteurs ---"

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
    let filtered = AF.revise node (get_support ()) in
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
    Stack.push !counter_remove_prov stack_remove_numb_nb

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
    Stack.push !counter_select_prov stack_select_numb_nb;
    if verbose then print_endline "<-- End selecting"

  let propagation_select_by_value ?(verbose = false) value domain_name =
    let domain = Hashtbl.find (get_graph ()).domains domain_name in
    match DLL.find (fun e -> e.value = value) domain with
    | None ->
        invalid_arg
          ("Propagation select: " ^ value ^ " is not in " ^ domain_name)
    | Some node -> propagation_select_by_node ~verbose node

  let back_track_remove () =
    for _ = Stack.pop stack_remove_numb_nb downto 1 do
      AF.back_track (get_op ())
    done

  let back_track_select () =
    for _ = Stack.pop stack_select_numb_nb downto 1 do
      back_track_remove ()
    done

  let find_solution ?(verbose = false) () =
    print_compteurs ();
    let domains = Hashtbl.to_seq_values (get_graph ()).domains |> List.of_seq in
    let number_of_fails = ref 0 in
    let number_of_solutions = ref 0 in
    let rec aux sol : string DLL.t list -> unit = function
      | [] ->
          incr number_of_solutions;
          MyPrint.print_color_str "blue"
            ("A solution : " ^ to_str_node_list sol ^ " !!")
      | hd :: tl ->
          DLL.iter
            (fun v ->
              (* MyPrint.print_color_str "blue" v.value; *)
              try
                propagation_select_by_node ~verbose v;
                aux (v :: sol) tl;
                back_track_select () (* print_domains () *)
              with Empty_domain ->
                for _ = !counter_select_prov downto 1 do
                  back_track_remove ()
                done;
                for _ = !counter_remove_prov downto 1 do
                  AF.back_track (get_op ())
                done;
                Printf.printf "%d %d\n" !counter_remove_prov
                  !counter_select_prov;
                (* assert (Stack.is_empty stack_remove_numb_nb);
                   assert (Stack.is_empty stack_select_numb_nb); *)
                incr number_of_fails;
                MyPrint.print_color_str "red"
                  ("A fail : " ^ to_str_node_list (v :: sol) ^ " ..."))
            hd
    in
    aux [] domains;
    MyPrint.print_color_str "red"
      (Printf.sprintf "The number of fails is %d" !number_of_fails);
    MyPrint.print_color_str "green"
      (Printf.sprintf "The number of solutions is %d" !number_of_solutions)
end
