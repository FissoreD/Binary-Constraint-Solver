module DLL = DoublyLinkedList

type 'a cell_type = 'a DLL.dll_node * 'a DLL.dll_node DLL.t

and 'a compteurs = 'a cell_type DLL.t
(**
  For each constraint and each value there is a linked list of supports  
*)

let build_support ({ tbl; _ } : int Constraint.supports) =
  let compteurs : int compteurs = DLL.empty () in
  DLL.iter
    (fun node ->
      let a, b = node.value in
      (match DLL.find_assoc (( == ) a) compteurs with
      | None -> ()
      | Some x -> DLL.append b x);
      ignore (a, b))
    tbl

(** 
  For each value v in d1 it should exist a support in d2, otherwise we remove v from d1,
  returns the list of filtered values.
  If the list is empty, then no modification has been performed on d1
*)
let revise (d1 : 'a DLL.t) (d2 : 'a DLL.t) (support : 'a Constraint.relation) =
  match !d1 with
  | None -> []
  | Some dom1 ->
      let rec aux (current : 'a DLL.dll_node) acc =
        let remove_current = not (DLL.exsist (fun e -> support current e) d2) in
        if remove_current then DLL.remove current;
        match current.next with
        | None -> acc
        | Some new_current ->
            aux new_current (if remove_current then current :: acc else acc)
      in
      aux dom1.first []
