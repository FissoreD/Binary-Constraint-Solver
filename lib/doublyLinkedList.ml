(* The type of the double linked list (ddl) *)
type 'e dll_node = {
  value : 'e;
  mutable prev : 'e dll_node option;
  mutable next : 'e dll_node option;
  dll_father : 'e t;
}

and 'e sentinel = { mutable first : 'e dll_node; mutable last : 'e dll_node }
and 'e t = 'e sentinel option ref

let empty () = ref None
let is_empty d = !d = None
let make_node value dll_father = { value; prev = None; next = None; dll_father }

let add_after current value =
  let value = make_node value current.dll_father in
  value.prev <- Some current;
  value.next <- current.next;
  current.next <- Some value;
  match value.next with
  | None -> (Option.get !(value.dll_father)).last <- value
  | Some e -> e.prev <- Some value

let add_before current value =
  let value = make_node value current.dll_father in
  value.next <- Some current;
  value.prev <- current.prev;
  current.prev <- Some value;
  match value.prev with
  | None -> (Option.get !(value.dll_father)).first <- value
  | Some e -> e.next <- Some value

let append e domain =
  match !domain with
  | None ->
      let node = make_node e domain in
      domain := Some { first = node; last = node }
  | Some { last; _ } -> add_after last e

let prepend e domain =
  match !domain with
  | None ->
      let node = make_node e domain in
      domain := Some { first = node; last = node }
  | Some { first; _ } -> add_before first e

let remove_after current =
  match current.next with
  | None -> None
  | Some removed ->
      current.next <- removed.next;
      (match removed.next with None -> () | Some e -> e.prev <- Some current);
      Some removed

let remove_before current =
  match current.prev with
  | None -> current.prev
  | Some removed ->
      current.prev <- removed.prev;
      (match removed.prev with None -> () | Some e -> e.next <- Some current);
      Some removed

let iter_gen is_rev f d =
  let get n = if is_rev then n.prev else n.next in
  if is_empty d then ()
  else
    let e = Option.get !d in
    let rec aux current =
      f current;
      match get current with None -> () | Some e -> aux e
    in
    aux (if is_rev then e.last else e.first)

let iter f d = iter_gen false f d
let iter_rev f d = iter_gen true f d

let find p (t : 'a t) =
  match !t with
  | None -> None
  | Some { first; _ } ->
      let rec aux e =
        if p e then Some e
        else match e.next with None -> None | Some e -> aux e
      in
      aux first

let find_assoc p (t : 'a t) =
  match find (fun e -> p (fst e.value)) t with
  | None -> None
  | Some e -> Some (snd e.value)

let find_by_value (value : 'a) = find (fun e -> e.value = value)
let exsist p (t : 'a t) = find p t <> None
let not_exsist p (t : 'a t) = find p t = None

let remove (node : 'a dll_node) =
  let dom1 = Option.get !(node.dll_father) in
  match (node.prev, node.next) with
  (* TODO: do not remember to push to stack this operation *)
  | None, None -> node.dll_father := None
  | Some prev, None ->
      remove_after prev |> ignore;
      dom1.last <- prev
  | None, Some succ ->
      remove_before succ |> ignore;
      dom1.first <- succ
  | Some prev, _ -> remove_after prev |> ignore

let remove_by_value (value : 'a) (domain : 'a t) =
  match find (fun e -> e.value = value) domain with
  | None -> ()
  | Some node -> remove node

let remove_by_list_of_values l domain =
  List.iter ((Fun.flip remove_by_value) domain) l

(** Takes a ('a list) L and returns a dll containing the elements of L *)
let of_list l : 'a t =
  let domain = ref None in
  let rec aux node = function
    | [] -> domain
    | hd :: tl ->
        add_after node hd;
        (Option.get !domain).last <- Option.get node.next;
        aux (Option.get node.next) tl
  in
  match l with
  | [] -> domain
  | hd :: tl ->
      let fst_node = make_node hd domain in
      domain := Some { first = fst_node; last = fst_node };
      aux fst_node tl

let to_list (d : 'a t) =
  match !d with
  | None -> []
  | Some e ->
      let rec aux { value; next; _ } =
        value :: (match next with None -> [] | Some e -> aux e)
      in
      aux e.first
