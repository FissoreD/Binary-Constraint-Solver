module DDL = DoublyLinkedList

(** 
  For each value v in d1 it should exist a support in d2, otherwise we remove v from d1,
  returns the list of filtered values.
  If the list is empty, then no modification has been performed on d1
*)
let revise (d1 : 'a DDL.t) (d2 : 'a DDL.t) (support : 'a Constraint.supports) =
  match !d1 with
  | None -> []
  | Some dom1 ->
      let rec aux (current : 'a DDL.dll_node) acc =
        let remove_current =
          DDL.not_exsist (fun e -> support.relation current e) d2
        in
        if remove_current then DDL.remove current;
        let acc = if remove_current then current :: acc else acc in
        match current.next with
        | None -> acc
        | Some new_current -> aux new_current acc
      in
      aux dom1.first []
