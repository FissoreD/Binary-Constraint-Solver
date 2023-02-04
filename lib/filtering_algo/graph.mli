module DLL = DoublyLinkedList

type 'a domain = 'a DLL.t
type 'a graph

val build_graph : unit -> string graph

val add_constraint :
  string graph -> string DLL.t -> string -> string DLL.t -> string -> unit

val get_domain : 'a graph -> string -> 'a DLL.t
val get_constraint_binding : 'b graph -> 'a DLL.t -> 'b domain DLL.t
val print_string_domain : ?is_rev:bool -> string domain -> unit
val print_domains : ('a DLL.t -> unit) -> 'a graph -> unit
val print_string_domains : ?is_rev:bool -> string graph -> unit
val loop_domains : ('a DLL.t -> unit) -> 'a graph -> unit
val list_domains : 'a graph -> 'a DLL.t list
val relation : 'a graph -> 'a DLL.node -> 'a DLL.node -> bool
