open Base
module DLL = DoublyLinkedList

module type Arc_consistency = sig
  exception Not_in_support of string

  module DLL = DoublyLinkedList

  type 'a data_struct
  type 'a stack_operation
  type 'a remove_in_domain = string DLL.node list

  val name : string
  val print_data_struct : string data_struct -> unit

  val initialization :
    ?print:bool -> string Constraint.graph -> string data_struct

  val revise :
    string DLL.node ->
    string data_struct ->
    string stack_operation * string remove_in_domain

  val back_track : string stack_operation -> unit
end

let init_remove print (v : 'a DLL.node) =
  if print then
    MyPrint.print_color_str "green"
      (Printf.sprintf "Initialization : removing %s from %s" v.value
         v.dll_father.name);
  DLL.remove v

let make_name (node : 'a DLL.node) =
  Printf.sprintf "(%s,%s)" node.dll_father.name node.value

let clean_domains ?(print = false) (g : 'a Constraint.graph) =
  Constraint.loop_domains
    (fun (d1 : 'a DLL.t) ->
      let neighs = Constraint.get_constraint_binding g d1 in
      DLL.iter
        (fun v1 ->
          if
            not (DLL.forall_value (DLL.exist (Constraint.relation g v1)) neighs)
          then init_remove print v1)
        d1)
    g;
  g
