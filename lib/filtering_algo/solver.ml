open Base

module type Solver = sig
  module DLL = DoublyLinkedList

  val initialization : ?verbose:bool -> string Graph.graph -> unit

  val find_solution :
    ?debug:bool ->
    ?only_stats:bool ->
    ?only_valid:bool ->
    ?verbose:bool ->
    ?one_sol:bool ->
    unit ->
    unit

  val remove_by_value : ?verbose:bool -> string -> string -> unit
  val propagation_remove_by_value : ?verbose:bool -> string -> string -> unit
  val propagation_select_by_value : ?verbose:bool -> string -> string -> unit
  val print_data_struct : unit -> unit
  val print_domains : unit -> unit
end

let debug ?(msg = "") () = Stdio.print_endline ("Debugging : " ^ msg)

module Make (AC : Arc_consistency.Arc_consistency) : Solver = struct
  module DLL = DoublyLinkedList

  exception Empty_domain

  let delta_domain = ref []
  let time_of_backtracks = ref 0.
  let time_of_revise = ref 0.
  let internal_data_struct = ref None
  let get_data_struct () = Option.value_exn !internal_data_struct
  let graph = ref None
  let get_graph () = Option.value_exn !graph

  type 'a stack_type =
    (string AC.stack_operation * string Graph.value) option Stack.t

  let stack_op : 'a stack_type =
    let stack = Stack.create () in
    Stack.push stack None;
    stack

  let backtrack_mem : 'a stack_type = Stack.create ()

  (* let counter_remove_prov = ref 0 *)
  let add_stack q = Stack.push stack_op (Some q)
  let get_op () = Option.value_exn (Stack.pop_exn stack_op)

  let to_str_node_list l =
    let to_str (e : 'a Graph.value) =
      Printf.sprintf "(%s,%s)" e.father.name e.value
    in
    let append a b = Printf.sprintf "%s, %s" (to_str b) a in
    let rec aux = function
      | [] -> ""
      | [ hd ] -> to_str hd
      | hd :: tl -> append (aux tl) hd
    in
    "[" ^ aux l ^ "]"

  let print_domains () = Graph.print_string_domains (get_graph ())
  let print_list_nodes l = Stdio.print_endline (to_str_node_list l)
  let print_data_struct () = AC.print_data_struct (get_data_struct ())

  let initialization ?(verbose = false) graph' =
    internal_data_struct := Some (AC.initialization ~verbose graph');
    graph := Some graph';
    if verbose then (
      Stdio.print_endline "The data structure is:";
      AC.print_data_struct (get_data_struct ());
      Stdio.print_endline "The domains are";
      print_domains ();
      Stdio.print_endline "-----------------------------")

  let remove_by_node ?(verbose = false) (node : 'a Graph.value) =
    if node.is_in then (
      if verbose then
        MyPrint.print_color_str "red"
          (Printf.sprintf " * Removing %s from %s" node.value node.father.name);
      DLL.remove node;
      let t = Unix.gettimeofday () in
      let stack_op, unsupported = AC.revise node (get_data_struct ()) in
      time_of_revise := Unix.gettimeofday () -. t +. !time_of_revise;
      add_stack (stack_op, node);
      delta_domain := unsupported;
      if verbose then (
        Stdio.print_string "List of values having no more support = ";
        print_list_nodes unsupported);
      if DLL.is_empty node.father then raise Empty_domain)

  let rec propagation_remove_by_node ?(verbose = false) (node : 'a Graph.value)
      =
    if node.is_in then (
      remove_by_node ~verbose node;
      List.iter ~f:(propagation_remove_by_node ~verbose) !delta_domain)

  let propagation_select_by_node ?(verbose = false) (v : 'a Graph.value) =
    if verbose then
      MyPrint.print_color_str "blue"
        (Printf.sprintf "--> Selecting %s from %s" v.value v.father.name);
    Stack.push backtrack_mem (Stack.top_exn stack_op);
    DLL.iter
      (fun v' ->
        if not (phys_equal v' v) then propagation_remove_by_node ~verbose v')
      v.father

  let back_track () =
    let top = Stack.pop_exn backtrack_mem in
    let t = Unix.gettimeofday () in
    while not (phys_equal top (Stack.top_exn stack_op)) do
      let internal_op, node = get_op () in
      AC.back_track internal_op;
      DLL.insert node
    done;
    time_of_backtracks := Unix.gettimeofday () -. t +. !time_of_backtracks

  let find_solution ?(debug = false) ?(only_stats = false) ?(only_valid = false)
      ?(verbose = false) ?(one_sol = false) () =
    let exception Stop_One_Sol in
    let domains =
      Graph.list_domains (get_graph ())
      |> List.sort ~compare:(fun (a : 'a Graph.domain) (b : 'a Graph.domain) ->
             compare_string a.name b.name)
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
    if debug then AC.print_data_struct (get_data_struct ());
    let time = Unix.gettimeofday () in
    let rec aux sol : string Graph.domain list -> unit = function
      | [] ->
          Int.incr number_of_solutions;
          if not only_stats then print_sol sol;
          if one_sol then raise Stop_One_Sol
      | hd :: tl ->
          DLL.iter
            (fun v ->
              let sol = v :: sol in
              if debug then print_domains ();
              (try
                 propagation_select_by_node ~verbose v;
                 aux sol tl
               with Empty_domain ->
                 Int.incr number_of_fails;
                 if (not only_stats) && not only_valid then print_fail sol);
              back_track ())
            hd
    in
    (try aux [] domains with Stop_One_Sol -> ());
    Stdio.print_endline "------------------------------";
    MyPrint.print_color_str "red"
      (Printf.sprintf "The number of fails is %d" !number_of_fails);
    MyPrint.print_color_str "green"
      (Printf.sprintf "The number of solutions is %d" !number_of_solutions);
    MyPrint.print_color_str "gray"
      (Printf.sprintf
         "Total Time: %f\nTime of backtracks: %f\nTime of revise: %f"
         (Unix.gettimeofday () -. time)
         !time_of_backtracks !time_of_revise);
    Stdio.print_endline "------------------------------"

  (** Returns if a fail has occured, i.e. if there is an empty domain among those that have been modified and the list of removed values *)
  let remove_by_value ?(verbose = false) value domain_name =
    try
      let e =
        DLL.find_by_value value (Graph.get_domain (get_graph ()) domain_name)
      in
      remove_by_node ~verbose e
    with _ ->
      invalid_arg
        (Printf.sprintf "The value %s does not exists in the domain %s" value
           domain_name)

  let propagation_remove_by_value ?(verbose = false) (value : string)
      (domain_name : string) =
    let domain = Graph.get_domain (get_graph ()) domain_name in
    match DLL.find (fun e -> String.( = ) e.value value) domain with
    | None ->
        invalid_arg
          ("Propagation remove: " ^ value ^ " is not in " ^ domain_name)
    | Some node -> propagation_remove_by_node ~verbose node

  let propagation_select_by_value ?(verbose = false) value domain_name =
    let domain = Graph.get_domain (get_graph ()) domain_name in
    match DLL.find (fun e -> String.( = ) e.value value) domain with
    | None ->
        invalid_arg
          ("Propagation select: " ^ value ^ " is not in " ^ domain_name)
    | Some node -> propagation_select_by_node ~verbose node
end
