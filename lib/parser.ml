module DLL = DoublyLinkedList

let parse ?(print_inp = false) cnt =
  if print_inp then List.iter print_endline cnt;
  let graph = Graph.build_graph () in
  let domains : (string, string DLL.t) Hashtbl.t = Hashtbl.create 1024 in
  let add_domain s =
    let name = List.hd s |> String.trim in
    let content = List.tl s in
    let domain = DLL.empty name in
    List.iter ((Fun.flip DLL.append) domain) content;
    Hashtbl.add domains name domain
  in
  let add_constraint = function
    | [ d1; v1; d2; v2 ] ->
        let d1 = Hashtbl.find domains d1 in
        let d2 = Hashtbl.find domains d2 in
        Graph.add_constraint graph d1 v1 d2 v2
    | _ -> invalid_arg "Error in input file when parsing constraints"
  in
  let stage = ref 0 in
  List.iter
    (fun line ->
      let line =
        Str.split (Str.regexp "[ :,;.]") line |> List.filter (( <> ) "")
      in
      (* print_endline line; *)
      match line with
      | [] | "#" :: _ -> ()
      | h :: _ when String.starts_with ~prefix:"#" h -> ()
      | "--" :: _ -> incr stage
      | l when !stage = 0 -> add_domain l
      | l -> add_constraint l)
    cnt;
  graph

let read_whole_file file_name =
  let f = open_in file_name in
  let rec aux () =
    try
      let l = input_line f in
      l :: aux ()
    with End_of_file ->
      close_in f;
      []
  in
  aux ()

let parse_file ?(print_inp = false) file_name =
  let res = parse ~print_inp (read_whole_file file_name) in
  res
