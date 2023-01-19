open Constraint_Solver_with_Backpropagation
module DLL = DoublyLinkedList

let () = print_endline "Hello, World!"
let build_domain name n = List.init n Fun.id |> DLL.of_list name

let print_int_domain ?(is_rev = false) d1 =
  (if is_rev then DLL.iter_rev else DLL.iter)
    (fun { value; _ } -> Printf.printf "%d;" value)
    d1;
  print_newline ()

let print_list_nodes l =
  List.iter (fun (e : int DLL.dll_node) -> Printf.printf "%d " e.value) l;
  print_newline ()

let build_constraint () =
  let d1, d2 = (build_domain "d1" 5, build_domain "d2" 5) in
  let support = Constraint.build_constraint () in
  let add_constr a b =
    Constraint.add_constraint support
      (DLL.find (fun { value; _ } -> value = a) d1 |> Option.get)
      (DLL.find (fun { value; _ } -> value = b) d2 |> Option.get)
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

let _test_remove () =
  let d1 = build_domain "" 4 in
  print_int_domain d1;
  let node = DLL.find (fun { value; _ } -> value = 2) d1 |> Option.get in
  Printf.printf "%d\n" node.value;
  DLL.remove node;
  print_int_domain d1;
  print_int_domain ~is_rev:true d1;

  let node = DLL.find (fun { value; _ } -> value = 3) d1 |> Option.get in
  Printf.printf "%d\n" node.value;
  DLL.remove node;
  print_int_domain d1;
  print_int_domain ~is_rev:true d1;

  let node = DLL.find (fun { value; _ } -> value = 0) d1 |> Option.get in
  Printf.printf "%d\n" node.value;
  DLL.remove node;
  print_int_domain d1;
  print_int_domain ~is_rev:true d1;

  let node = DLL.find (fun { value; _ } -> value = 1) d1 |> Option.get in
  Printf.printf "%d\n" node.value;
  DLL.remove node;
  print_int_domain d1;
  print_int_domain ~is_rev:true d1

let _ac_3 () =
  let d1, d2, support = build_constraint () in

  let filtered = AC_3.revise (DLL.remove_by_value 4 d2 |> Option.get) support in

  print_int_domain d1;
  print_int_domain d2;

  print_string "removed = ";
  print_list_nodes filtered.to_remove_in_domain;

  print_endline "Backtrack:";
  AC_3.back_track filtered;

  print_int_domain d1;
  print_int_domain d2;
  print_endline ""

let _ac_4 () =
  let d1, d2, support = build_constraint () in

  let supp = AC_4.build_support support in
  AC_4.print_compteurs supp;

  let stack = Queue.create () in

  List.iter
    (fun e ->
      let removed1 = Option.get (DLL.remove_by_value e d2) in

      let removed_list = AC_4.revise removed1 supp in
      print_string "removed: ";
      print_list_nodes removed_list.to_remove_in_domain;
      List.iter DLL.remove removed_list.to_remove_in_domain;
      Queue.push removed_list stack)
    [ 4 ];

  List.iter
    (fun e ->
      let removed1 = Option.get (DLL.remove_by_value e d1) in

      let removed_list = AC_4.revise removed1 supp in
      print_string "removed: ";
      print_list_nodes removed_list.to_remove_in_domain;
      List.iter DLL.remove removed_list.to_remove_in_domain;
      Queue.push removed_list stack)
    [];

  AC_4.print_compteurs supp;

  print_endline "";
  print_int_domain d1;
  print_int_domain d2;

  while not (Queue.is_empty stack) do
    AC_4.back_track (Queue.take stack)
  done;

  print_endline "After roll back";

  AC_4.print_compteurs supp;

  print_endline "";
  print_int_domain d1;
  print_int_domain d2

let _ac_6 () =
  let d1, d2, support = build_constraint () in

  Printf.printf "** length : %d\n" (DLL.length support.domains);

  let supp = AC_6.build_support support in
  AC_6.print_compteurs supp;

  (* invalid_arg "STOP" |> ignore; *)
  List.iter
    (fun e ->
      let removed1 = Option.get (DLL.remove_by_value e d2) in

      let removed_list = AC_6.revise removed1 supp support in
      print_string "removed: ";
      print_list_nodes removed_list.to_remove_in_domain;
      List.iter DLL.remove removed_list.to_remove_in_domain)
    [ 0; 2 ];

  List.iter
    (fun e ->
      let removed1 = Option.get (DLL.remove_by_value e d1) in

      let removed_list = AC_6.revise removed1 supp support in
      print_string "removed: ";
      print_list_nodes removed_list.to_remove_in_domain;
      List.iter DLL.remove removed_list.to_remove_in_domain)
    [ 2 ];

  AC_6.print_compteurs supp;

  print_endline "";
  print_int_domain d1;
  print_int_domain d2

let () = _ac_3 ()
