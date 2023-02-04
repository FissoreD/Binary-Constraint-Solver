open Constraint_Solver_with_Backpropagation
module DLL = DoublyLinkedList

let () =
  let open Arg in
  let algo = ref 3 in
  let verbose = ref false in
  let path = ref "graphs/input_2.txt" in
  let print_inp = ref false in
  let one_sol = ref false in
  let only_valid = ref false in
  let queens = ref (-1) in
  let allInt = ref (-1) in
  let count_only = ref false in
  let debug = ref false in
  let speclist =
    align
      [
        ( "-ac",
          Set_int algo,
          " Set the filtering algo among 3, 4, 6, 2001 - default : 3" );
        ("-v", Set verbose, " Set the verbose mode");
        ("-f", Set_string path, " Set the input file");
        ("-first", Set one_sol, " Finds only the first solution if it exists");
        ("-queens", Set_int queens, " Set the size of the queen solver");
        ( "-all-int",
          Set_int allInt,
          " Set the size of the allIntervalSeries solver" );
        ( "-only-sol",
          Set count_only,
          " Only print the number of fails and solutions" );
        ("-only-valid", Set only_valid, " Print only the valid solutions");
        ("-d", Set debug, " Debug mode");
        ("-print-inp", Set print_inp, " Print the input graph");
      ]
  in

  Arg.parse speclist print_endline
    "A constraint solver using AC-[3,4,6,2001] filtering algos";

  let m : (module Arc_consistency.Arc_consistency) =
    match !algo with
    | 3 -> (module AC_3)
    | 4 -> (module AC_4)
    | 6 -> (module AC_6)
    | 2001 -> (module AC_2001)
    | _ -> invalid_arg "No valid -ac option"
  in

  let module M = (val m : Arc_consistency.Arc_consistency) in
  let module Filtr = Solver.Make (M) in
  let graph =
    if !allInt > 2 then
      AllIntervalSeries.build_graph ~print_inp:!print_inp !allInt
    else if !queens > 4 then Queens.build_graph ~print_inp:!print_inp !queens
    else Parser.parse_file ~print_inp:!print_inp !path
  in

  Filtr.initialization ~verbose:!verbose graph;

  Filtr.find_solution ~debug:!debug ~count_only:!count_only
    ~only_valid:!only_valid ~verbose:!verbose ~one_sol:!one_sol ()
