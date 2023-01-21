open Constraint_Solver_with_Backpropagation
module DLL = DoublyLinkedList

let () = print_endline "Hello, World!"

let () =
  let module Filtr = Filtrage.Make (AC_6) in
  let support = Parser.parse_file "graphs/input_2.txt" in

  Printf.printf "There are %d domains\n" (Hashtbl.length support.domains);
  Hashtbl.iter
    (fun k (v : 'a DLL.t DLL.t) ->
      Printf.printf "%s is linked to %d domains. They are : %s\n" k
        (DLL.length v)
        (let l = DLL.to_list v in
         List.fold_left
           (fun acc (e : string DLL.t) -> Printf.sprintf "%s%s," acc e.name)
           "" l))
    support.constraint_binding;

  let print () =
    Filtr.print_domains ();
    Filtr.print_compteurs ()
  in
  Filtr.build_support support;
  print ();
  Filtr.propagation ~verbose:true "e" (Hashtbl.find support.domains "d3")
  |> ignore;
  print_endline "-- After Remove --";
  print ();

  Filtr.back_track ();
  print_endline "-- After Backtrack --";
  print ()
(* _ac_3 () *)
