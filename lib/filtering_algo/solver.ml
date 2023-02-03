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
  val print_data_struct : unit -> unit
end

let debug ?(msg = "") () =
  print_endline ("Debugging : " ^ msg);
  read_line () |> ignore

module Make (AF : Arc_consistency.Arc_consistency) : M = struct
  module DLL = DoublyLinkedList

  exception Empty_domain

  let no_more_supported = ref []
  let time_of_backtracks = ref 0.
  let time_of_revise = ref 0.
  let support = ref None
  let graph = ref None
  let get_data_struct () = Option.get !support
  let get_graph () = Option.get !graph

  type 'a stack_type = ('a AF.stack_operation * 'a DLL.dll_node) option Stack.t

  (* The internal operation of the filtering algos *)
  let internal_op : 'a stack_type =
    let stack = Stack.create () in
    Stack.push None stack;
    stack

  let backtrack_mem : 'a stack_type = Stack.create ()

  (* let counter_remove_prov = ref 0 *)
  let add_stack q = Stack.push (Some q) internal_op
  let get_op () = Option.get (Stack.pop internal_op)

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
  let print_data_struct () = AF.print_data_struct (get_data_struct ())

  let initialization ?(verbose = false) graph' =
    support := Some (AF.initialization graph');
    graph := Some graph';
    if verbose then (
      print_endline "The data structure is:";
      AF.print_data_struct (get_data_struct ());
      print_endline "The domains are";
      print_domains ();
      print_endline "-----------------------------")

  let remove_by_node ?(verbose = false) (node : 'a DLL.dll_node) =
    if node.is_in then (
      DLL.remove node;
      let t = Sys.time () in
      let stack_op, unsupported = AF.revise node (get_data_struct ()) in
      time_of_revise := Sys.time () -. t +. !time_of_revise;
      add_stack (stack_op, node);
      no_more_supported := unsupported;
      if verbose then (
        MyPrint.print_color_str "red"
          (Printf.sprintf " * Removing %s from %s" node.value
             node.dll_father.name);
        print_string "List of values having no more support = ";
        print_list_nodes unsupported);
      if DLL.is_empty node.dll_father then raise Empty_domain)

  let rec propagation_remove_by_node ?(verbose = false) (node : 'a DLL.dll_node)
      =
    if node.is_in then (
      remove_by_node ~verbose node;
      List.iter (propagation_remove_by_node ~verbose) !no_more_supported)

  let propagation_select_by_node ?(verbose = false) (v : 'a DLL.dll_node) =
    if verbose then
      MyPrint.print_color_str "blue"
        (Printf.sprintf "--> Selecting %s from %s" v.value v.dll_father.name);
    Stack.push (Stack.top internal_op) backtrack_mem;
    DLL.iter
      (fun v' -> if v' != v then propagation_remove_by_node ~verbose v')
      v.dll_father

  let back_track () =
    let top = Stack.pop backtrack_mem in
    let t = Sys.time () in
    while top != Stack.top internal_op do
      let internal_op, node = get_op () in
      AF.back_track internal_op;
      DLL.insert node
    done;
    time_of_backtracks := Sys.time () -. t +. !time_of_backtracks

  let find_solution ?(debug = false) ?(count_only = false) ?(verbose = false)
      ?(one_sol = false) () =
    let exception Stop_One_Sol in
    let domains = Constraint.list_domains (get_graph ()) |> List.sort compare in
    let number_of_fails = ref 0 in
    let number_of_solutions = ref 0 in
    let print_fail sol =
      MyPrint.print_color_str "red" ("A fail : " ^ to_str_node_list sol ^ " ...")
    in
    let print_sol sol =
      MyPrint.print_color_str "green"
        ("A solution : " ^ to_str_node_list sol ^ " !!")
    in
    if debug then AF.print_data_struct (get_data_struct ());
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
              back_track ())
            hd
    in
    (try aux [] domains with Stop_One_Sol -> ());
    print_endline "------------------------------";
    MyPrint.print_color_str "red"
      (Printf.sprintf "The number of fails is %d" !number_of_fails);
    MyPrint.print_color_str "green"
      (Printf.sprintf "The number of solutions is %d" !number_of_solutions);
    MyPrint.print_color_str "gray"
      (Printf.sprintf "Total Time: %f" (Sys.time () -. time));
    MyPrint.print_color_str "gray"
      (Printf.sprintf "Time of backtracks: %f" !time_of_backtracks);
    MyPrint.print_color_str "gray"
      (Printf.sprintf "Time of revise: %f" !time_of_revise);
    print_endline "------------------------------"

  (** Returns if a fail has occured, i.e. if there is an empty domain among those that have been modified and the list of removed values *)
  let remove_by_value ?(verbose = false) value domain_name =
    try
      let e =
        DLL.find_by_value value
          (Constraint.get_domain (get_graph ()) domain_name)
      in
      remove_by_node ~verbose e
    with _ ->
      invalid_arg
        (Printf.sprintf "The value %s does not exists in the domain %s" value
           domain_name)

  let propagation_remove_by_value ?(verbose = false) value domain_name =
    let domain = Constraint.get_domain (get_graph ()) domain_name in
    match DLL.find (fun e -> e.value = value) domain with
    | None ->
        invalid_arg
          ("Propagation remove: " ^ value ^ " is not in " ^ domain_name)
    | Some node -> propagation_remove_by_node ~verbose node

  let propagation_select_by_value ?(verbose = false) value domain_name =
    let domain = Constraint.get_domain (get_graph ()) domain_name in
    match DLL.find (fun e -> e.value = value) domain with
    | None ->
        invalid_arg
          ("Propagation select: " ^ value ^ " is not in " ^ domain_name)
    | Some node -> propagation_select_by_node ~verbose node
end
