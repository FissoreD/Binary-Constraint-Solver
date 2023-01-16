open Constraint_Solver_with_Backpropagation
module DLL = DoublyLinkedList

let () = print_endline "Hello, World!"
let build_domain n = List.init n Fun.id |> DLL.of_list

let print_int_domain ?(is_rev = false) d1 =
  (if is_rev then DLL.iter_rev else DLL.iter)
    (fun { value; _ } -> Printf.printf "%d;" value)
    d1;
  print_newline ()

let _test_remove () =
  let d1 = build_domain 4 in
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

let () =
  let d1, d2 = (build_domain 5, build_domain 5) in
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

  DLL.remove_by_list_of_values [ 0; 2; 4 ] d2;

  let removed_list = AC_3.revise d1 d2 support in
  print_string "removed: ";
  List.iter
    (fun ({ value; _ } : int DLL.dll_node) -> Printf.printf "%d" value)
    removed_list;
  print_endline "";
  print_int_domain d1;
  print_int_domain d2;

  print_endline ""
