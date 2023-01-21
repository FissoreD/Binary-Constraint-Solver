open Constraint_Solver_with_Backpropagation
module DLL = DoublyLinkedList

let () = print_endline "Hello, World!"
let build_domain name n = List.init n string_of_int |> DLL.of_list name

let _test_remove () =
  let d1 = build_domain "" 4 in
  Constraint.print_string_domain d1;
  let node = DLL.find (fun { value; _ } -> value = "2") d1 |> Option.get in
  Printf.printf "%s\n" node.value;
  DLL.remove node;
  Constraint.print_string_domain d1;
  Constraint.print_string_domain ~is_rev:true d1;

  let node = DLL.find (fun { value; _ } -> value = "3") d1 |> Option.get in
  Printf.printf "%s\n" node.value;
  DLL.remove node;
  Constraint.print_string_domain d1;
  Constraint.print_string_domain ~is_rev:true d1;

  let node = DLL.find (fun { value; _ } -> value = "0") d1 |> Option.get in
  Printf.printf "%s\n" node.value;
  DLL.remove node;
  Constraint.print_string_domain d1;
  Constraint.print_string_domain ~is_rev:true d1;

  let node = DLL.find (fun { value; _ } -> value = "1") d1 |> Option.get in
  Printf.printf "%s\n" node.value;
  DLL.remove node;
  Constraint.print_string_domain d1;
  Constraint.print_string_domain ~is_rev:true d1