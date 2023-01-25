open Constraint_Solver_with_Backpropagation
module DLL = DoublyLinkedList

let _select (module Filtr : Filtrage.M) verbose =
  Filtr.propagation_select_by_value ~verbose "b" "d1";

  MyPrint.print_color_str "blue" "-- After Select --";
  Filtr.print_domains ();

  Filtr.back_track_select ();
  MyPrint.print_color_str "blue" "-- After Backtrack --";
  Filtr.print_domains ()

let () =
  let open Arg in
  let algo = ref 3 in
  let verbose = ref false in
  let path = ref "graphs/input_2.txt" in
  let one_sol = ref false in
  let speclist =
    align
      [
        ( "-ac",
          Set_int algo,
          " Set the filtering algo among 3, 4, 6, 2001 - default : 3" );
        ("-v", Set verbose, " Set the verbose mode");
        ("-f", Set_string path, " Set the input file");
        ("-first", Set one_sol, " Finds only the first solution if it exists");
      ]
  in

  Arg.parse speclist print_endline
    "A constraint solver using AC-[3,5,6,2001] filtering algos";

  let m : (module Filtrage.Arc_Consistency) =
    match !algo with
    | 3 -> (module AC_3)
    | 4 -> (module AC_4)
    | 6 -> (module AC_6)
    | 2001 -> (module AC_2001)
    | _ -> invalid_arg "No valid -ac option"
  in

  let module M = (val m : Filtrage.Arc_Consistency) in
  let module Filtr = Filtrage.Make (M) in
  let _print () =
    Filtr.print_domains ();
    Filtr.print_data_struct ()
  in

  let graph = Parser.parse_file !path in

  Filtr.initialization graph;
  Filtr.print_domains ();
  Filtr.find_solution ~verbose:!verbose ~one_sol:!one_sol ()
(*Filtr.propagation_remove_by_value "2" "d2";
  Filtr.back_track_remove ();
  _print () *)
