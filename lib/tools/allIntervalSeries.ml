module DLL = DoublyLinkedList

let make_domains n =
  let buf_head = Buffer.create 7000 in
  let buf_tail = Buffer.create 7000 in

  (* Add domain v *)
  for dom_v1 = 1 to n - 1 do
    Buffer.add_string buf_head (Printf.sprintf "v%d : " dom_v1);
    for val_v1 = 1 to n - 1 do
      Buffer.add_string buf_head (Printf.sprintf "%d " val_v1);
      (* The all diff on v *)
      for dom_v2 = 1 to n - 1 do
        for val_v2 = 1 to n - 1 do
          if dom_v1 <> dom_v2 && val_v1 <> val_v2 then
            Buffer.add_string buf_tail
              (Printf.sprintf "v%d %d v%d %d\n" dom_v1 val_v1 dom_v2 val_v2)
        done
      done
    done;
    Buffer.add_char buf_head '\n'
  done;

  (* the interval vector v = (|s2−s1|,|s3−s2|,…|sn−sn−1|) is a permutation of {1,2,…,n−1}. Idea : use an auxiliary variable   
  *)
  for i = 1 to n - 1 do
    Buffer.add_string buf_head (Printf.sprintf "aux%d : " i);
    for aux = 1 to n * n do
      Buffer.add_string buf_head (Printf.sprintf "%d " aux)
    done;
    Buffer.add_char buf_head '\n'
  done;

  (* The absolute value constraints *)
  for aux_i = 1 to n - 1 do
    for value = 1 to n * n do
      if abs (((value - 1) / n) - ((value - 1) mod n)) > 0 then (
        Buffer.add_string buf_tail
          (Printf.sprintf "aux%d %d %d %d\n" aux_i value aux_i
             ((value - 1) mod n));
        Buffer.add_string buf_tail
          (Printf.sprintf "aux%d %d %d %d\n" aux_i value (aux_i + 1)
             ((value - 1) / n));
        Buffer.add_string buf_tail
          (Printf.sprintf "aux%d %d v%d %d\n" aux_i value aux_i
             (abs (((value - 1) / n) - ((value - 1) mod n)))))
    done
  done;

  (* Adding domains *)
  for dom = 1 to n do
    Buffer.add_string buf_head (string_of_int dom ^ ": ");
    for var = 0 to n - 1 do
      Buffer.add_string buf_head (string_of_int var ^ " ");
      for dom2 = 1 to n do
        for var2 = 0 to n - 1 do
          (* the all diff constraint between the values of dom and dom2 *)
          if dom <> dom2 && var <> var2 then
            Buffer.add_string buf_tail
              (Printf.sprintf "%d %d %d %d\n" dom var dom2 var2)
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
