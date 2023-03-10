module DLL = DoublyLinkedList

let make_domains n =
  let buf_head = Buffer.create 7000 in
  let buf_tail = Buffer.create 7000 in
  (* Adding domains *)
  Buffer.add_string buf_head "# Columns\n";
  Buffer.add_string buf_tail "# Constraints\n";
  for row = 1 to n do
    Buffer.add_string buf_head ("col" ^ string_of_int row ^ ": ");
    for col = 1 to n do
      Buffer.add_string buf_head (string_of_int col ^ " ");
      for row' = 1 to n do
        for col' = 1 to n do
          if abs (col' - col) <> abs (row' - row) && col <> col' && row <> row'
          then
            Buffer.add_string buf_tail
              (Printf.sprintf "col%d %d col%d %d;\n" col row col' row')
        done
      done
    done;
    Buffer.add_char buf_head '\n'
  done;
  Buffer.add_string buf_head "--\n";
  Buffer.add_buffer buf_head buf_tail;

  let res = Buffer.contents buf_head in
  res

let build_graph ?(print_inp = false) n =
  Parser.parse ~print_inp (String.split_on_char '\n' (make_domains n))
