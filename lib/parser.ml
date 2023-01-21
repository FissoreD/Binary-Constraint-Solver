module DLL = DoublyLinkedList

let parse_file file_name =
  let f = open_in file_name in
  let graph = Constraint.build_constraint () in
  let domains : (string, string DLL.t) Hashtbl.t = Hashtbl.create 1024 in
  let add_domain s =
    let splitted =
      Str.split (Str.regexp "[ :,;.]") s |> List.filter (( <> ) "")
    in
    let name = List.hd splitted |> String.trim in
    let content = List.tl splitted in
    let domain = DLL.empty name in
    List.iter (fun e -> DLL.append e domain |> ignore) content;
    Hashtbl.add domains name domain
  in
  let add_constraint s =
    let splitted =
      Str.split (Str.regexp "[ :,;.]") s |> List.filter (( <> ) "")
    in
    match splitted with
    | [ d1; v1; d2; v2 ] ->
        Constraint.add_constraint graph (Hashtbl.find domains d1) v1
          (Hashtbl.find domains d2) v2
        |> ignore
    | _ -> invalid_arg "Error in input file when parsing constraints"
  in
  let stage = ref 0 in
  (try
     while true do
       let line = input_line f in
       (* print_endline line; *)
       if String.starts_with ~prefix:"#" line then ()
       else if line = "--" then incr stage
       else if !stage = 0 then add_domain line
       else add_constraint line
     done
   with End_of_file -> ());
  close_in f;
  graph
