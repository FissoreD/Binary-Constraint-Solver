open Base
module DLL = DoublyLinkedList

module type Arc_consistency = sig
  module DLL = DoublyLinkedList

  type 'a data_struct
  type 'a stack_operation

  val print_data_struct : string data_struct -> unit
  val initialization : ?verbose:bool -> string Graph.graph -> string data_struct

  val revise :
    string Graph.value ->
    string data_struct ->
    string stack_operation * string Graph.value list

  val back_track : string stack_operation -> unit
end

let init_remove print (v : 'a Graph.value) =
  if print then
    MyPrint.print_color_str "green"
      (Printf.sprintf "Initialization : removing %s from %s" v.value
         v.father.name);
  DLL.remove v

let make_name (node : 'a Graph.value) =
  Printf.sprintf "(%s,%s)" node.father.name node.value

let clean_domains ?(verbose = false) (g : 'a Graph.graph) =
  Graph.loop_domains
    (fun (d1 : 'a DLL.t) ->
      let neighs = Graph.get_constraint_binding g d1 in
      DLL.iter
        (fun v1 ->
          if not (DLL.forall_value (DLL.exist (Graph.relation g v1)) neighs)
          then init_remove verbose v1)
        d1)
    g;
  g
