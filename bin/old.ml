open Constraint_Solver_with_Backpropagation
module DLL = DoublyLinkedList

let build_domain name n = List.init n string_of_int |> DLL.of_list name

let print_list_nodes l =
  List.iter (fun (e : string DLL.dll_node) -> Printf.printf "%s " e.value) l;
  print_newline ()

let build_constraint () =
  let d1, d2 = (build_domain "d1" 5, build_domain "d2" 5) in
  let support = Constraint.build_constraint () in
  let add_constr a b =
    Constraint.add_constraint support d1 (string_of_int a) d2 (string_of_int b)
  in
  add_constr 0 0;
  add_constr 0 2;
  add_constr 1 0;
  add_constr 1 4;
  add_constr 2 1;
  add_constr 2 3;
  add_constr 3 2;
  add_constr 3 4;
  add_constr 4 4;
  (d1, d2, support)

let _ac_3 () =
  let _d1, d2, graph = build_constraint () in

  let supp = AC_3.initialization graph in
  AC_3.print_data_struct supp;

  let filtered = AC_3.revise (DLL.remove_by_value "4" d2 |> Option.get) supp in

  Constraint.print_string_domains graph;

  print_string "removed = ";
  print_list_nodes (AC_3.get_to_remove filtered);

  print_endline "Backtrack:";
  AC_3.back_track filtered;

  Constraint.print_string_domains graph;
  print_endline ""

let _ac_4 () =
  let d1, d2, support = build_constraint () in

  let supp = AC_4.initialization support in
  AC_4.print_data_struct supp;

  let stack = Queue.create () in

  List.iter
    (fun e ->
      let removed1 = Option.get (DLL.remove_by_value e d2) in

      let removed_list = AC_4.revise removed1 supp in
      print_string "removed: ";
      print_list_nodes (AC_4.get_to_remove removed_list);
      List.iter DLL.remove (AC_4.get_to_remove removed_list);
      Queue.push removed_list stack)
    [ "4" ];

  List.iter
    (fun e ->
      let removed1 = Option.get (DLL.remove_by_value e d1) in

      let removed_list = AC_4.revise removed1 supp in
      print_string "removed: ";
      print_list_nodes (AC_4.get_to_remove removed_list);
      List.iter DLL.remove (AC_4.get_to_remove removed_list);
      Queue.push removed_list stack)
    [];

  AC_4.print_data_struct supp;

  print_endline "";
  Constraint.print_string_domains support;

  while not (Queue.is_empty stack) do
    AC_4.back_track (Queue.take stack)
  done;

  print_endline "After roll back";

  AC_4.print_data_struct supp;

  print_endline "";
  Constraint.print_string_domains support

let _ac_6 () =
  let d1, d2, support = build_constraint () in

  Printf.printf "** length : %d\n" (Hashtbl.length support.domains);

  let supp = AC_6.initialization support in
  AC_6.print_data_struct supp;

  (* invalid_arg "STOP" |> ignore; *)
  List.iter
    (fun e ->
      let removed1 = Option.get (DLL.remove_by_value e d2) in

      let removed_list = AC_6.revise removed1 supp in
      print_string "removed: ";
      print_list_nodes (AC_6.get_to_remove removed_list);
      List.iter DLL.remove (AC_6.get_to_remove removed_list))
    [ "0"; "2" ];

  List.iter
    (fun e ->
      let removed1 = Option.get (DLL.remove_by_value e d1) in

      let removed_list = AC_6.revise removed1 supp in
      print_string "removed: ";
      print_list_nodes (AC_6.get_to_remove removed_list);
      List.iter DLL.remove (AC_6.get_to_remove removed_list))
    [ "2" ];

  AC_6.print_data_struct supp;

  print_endline "";
  Constraint.print_string_domains support

let () = _ac_3 ()
