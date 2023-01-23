module DLL = DoublyLinkedList

let parse_file file_name =
  let f = open_in file_name in
  let graph = Constraint.build_constraint () in
  let domains : (string, string DLL.t) Hashtbl.t = Hashtbl.create 1024 in
  let add_domain s =
    let name = List.hd s |> String.trim in
    let content = List.tl s in
    let domain = DLL.empty name in
    List.iter (fun e -> DLL.append e domain |> ignore) content;
    Hashtbl.add domains name domain
  in
  let add_constraint = function
    | [ d1; v1; d2; v2 ] ->
        Constraint.add_constraint graph (Hashtbl.find domains d1) v1
          (Hashtbl.find domains d2) v2
        |> ignore
    | _ -> invalid_arg "Error in input file when parsing constraints"
  in
  let stage = ref 0 in
  (try
     while true do
       let line =
         Str.split (Str.regexp "[ :,;.]") (input_line f)
         |> List.filter (( <> ) "")
       in
       (* print_endline line; *)
       match line with
       | [] | "#" :: _ -> ()
       | h :: _ when String.starts_with ~prefix:"#" h -> ()
       | "--" :: _ -> incr stage
       | l when !stage = 0 -> add_domain l
       | l -> add_constraint l
     done
   with End_of_file -> ());
  close_in f;
  graph