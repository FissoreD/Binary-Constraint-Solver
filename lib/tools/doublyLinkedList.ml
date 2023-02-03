exception AlreadyIn
exception AlreadyOut

let id = ref 0

(* The type of the double linked list (ddl) *)
type 'e dll_node = {
  value : 'e;
  id : int;
  dll_father : 'e t;
  mutable prev : 'e dll_node option;
  mutable next : 'e dll_node option;
  mutable is_in : bool;
}

and 'e sentinel = { mutable first : 'e dll_node; mutable last : 'e dll_node }
and 'e t = { name : string; mutable content : 'e sentinel option }

let empty name = { name; content = None }
let get d = Option.get d
let get_first d = (get d.content).first
let get_last d = (get d.content).last
let is_empty d = d.content = None

let make_node value dll_father =
  incr id;
  { is_in = true; value; prev = None; next = None; dll_father; id = !id }

let add_after_node current node =
  if current.dll_father != node.dll_father then
    invalid_arg "Can't add after, in nodes with different fathers";
  node.prev <- Some current;
  node.next <- current.next;
  current.next <- Some node;
  match node.next with
  | None -> (get node.dll_father.content).last <- node
  | Some e -> e.prev <- Some node

let add_after current value =
  let value = make_node value current.dll_father in
  add_after_node current value

let add_before_node current node =
  node.next <- Some current;
  node.prev <- current.prev;
  current.prev <- Some node;
  match node.prev with
  | None -> (get node.dll_father.content).first <- node
  | Some e -> e.next <- Some node

let add_before current value =
  let value = make_node value current.dll_father in
  add_before_node current value

let append e dll =
  match dll.content with
  | None ->
      let node = make_node e dll in
      dll.content <- Some { first = node; last = node }
  | Some { last; _ } -> add_after last e

let append_node node dll =
  match !dll with
  | None -> dll := Some { first = node; last = node }
  | Some { last; _ } -> add_after_node last node

let prepend e dll =
  match dll.content with
  | None ->
      let node = make_node e dll in
      dll.content <- Some { first = node; last = node }
  | Some { first; _ } -> add_before first e

let insert e =
  if e.is_in then raise AlreadyIn;
  e.is_in <- true;
  match e.dll_father.content with
  | None -> e.dll_father.content <- Some { first = e; last = e }
  | Some father -> (
      match (e.prev, e.next) with
      | None, None ->
          father.first <- e;
          father.last <- e
      | Some prev, None ->
          father.last <- e;
          prev.next <- Some e
      | None, Some succ ->
          father.first <- e;
          succ.prev <- Some e
      | Some prev, Some succ ->
          prev.next <- Some e;
          succ.prev <- Some e)

let singleton name s =
  let dom = empty name in
  append s dom;
  dom

let remove_after current =
  match current.next with
  | None -> ()
  | Some removed -> (
      current.next <- removed.next;
      match removed.next with None -> () | Some e -> e.prev <- Some current)

let remove_before current =
  match current.prev with
  | None -> ()
  | Some removed -> (
      current.prev <- removed.prev;
      match removed.prev with None -> () | Some e -> e.next <- Some current)

let iter_gen is_rev f d =
  let get n = if is_rev then n.prev else n.next in
  if is_empty d then ()
  else
    let e = Option.get d.content in
    let rec aux current =
      f current;
      match get current with None -> () | Some e -> aux e
    in
    aux (if is_rev then e.last else e.first)

let map f d =
  if is_empty d then []
  else
    let e = Option.get d.content in
    let rec aux current =
      f current :: (match current.next with None -> [] | Some e -> aux e)
    in
    aux e.first

let iter f d = iter_gen false f d
let iter_value f d = iter_gen false (fun e -> f e.value) d
let map_value f d = map (fun e -> f e.value) d
let iter_rev f d = iter_gen true f d

let rec find_from p (t : 'a dll_node) =
  if p t then Some t
  else match t.next with None -> None | Some e -> find_from p e

let find_from_next p (t : 'a dll_node) =
  match t.next with None -> None | Some t -> find_from p t

let find p (t : 'a t) =
  match t.content with None -> None | Some { first; _ } -> find_from p first

let find_assoc p (t : 'a t) = find (fun e -> p (fst e.value)) t

let find_all p (t : 'a t) =
  match t.content with
  | None -> []
  | Some { first; _ } ->
      let rec aux e acc =
        let acc = if p e then e :: acc else acc in
        match e.next with None -> acc | Some e -> aux e acc
      in
      aux first []

let find_by_value (value : 'a) e =
  let res = find (fun e -> e.value = value) e in
  Option.get res

let rec exists_from p (t : 'a dll_node) =
  p t || match t.next with None -> false | Some e -> exists_from p e

let rec exists_from_by_value (p : 'a -> bool) (t : 'a dll_node) =
  p t.value
  || match t.next with None -> false | Some e -> exists_from_by_value p e

let exist p (t : 'a t) =
  match t.content with
  | None -> false
  | Some { first; _ } -> exists_from p first

let exist_by_balue p (t : 'a t) =
  match t.content with
  | None -> false
  | Some { first; _ } -> exists_from_by_value p first

let not_exist p (t : 'a t) = not (exist p t)
let not_exist_by_value p (t : 'a t) = not (exist_by_balue p t)
let add_if_absent p e d = if not_exist p d then append e d

let add_assoc k value d =
  match find_assoc (( == ) k) d with
  | None ->
      let nd = empty "" in
      append (k, nd) d;
      append value nd
  | Some nd -> add_if_absent (fun e -> e.value == value) value (snd nd.value)

let forall p (t : 'a t) =
  match t.content with
  | None -> false
  | Some { first; _ } ->
      let rec aux t =
        p t && match t.next with None -> true | Some e -> aux e
      in
      aux first

let forall_value p (t : 'a t) = forall (fun e -> p e.value) t

let remove (node : 'a dll_node) =
  if not node.is_in then raise AlreadyOut;
  node.is_in <- false;
  let dom1 = get node.dll_father.content in
  match (node.prev, node.next) with
  | None, None -> node.dll_father.content <- None
  | Some prev, None ->
      remove_after prev;
      dom1.last <- prev
  | None, Some succ ->
      remove_before succ;
      dom1.first <- succ
  | Some prev, _ -> remove_after prev

let remove_by_value (value : 'a) (dll : 'a t) =
  match find (fun e -> e.value = value) dll with
  | None -> None
  | Some node ->
      remove node;
      Some node

let remove_by_list_of_values l dll = List.map ((Fun.flip remove_by_value) dll) l

(** Takes a ('a list) L and returns a dll containing the elements of L *)
let of_list name l : 'a t =
  let dll = empty name in
  let rec aux = function
    | [] -> dll
    | hd :: tl ->
        append hd dll;
        aux tl
  in
  aux l

let to_list (d : 'a t) =
  match d.content with
  | None -> []
  | Some e ->
      let rec aux { value; next; _ } =
        value :: (match next with None -> [] | Some e -> aux e)
      in
      aux e.first

let length dll =
  let l = ref 0 in
  iter (fun _ -> incr l) dll;
  !l
