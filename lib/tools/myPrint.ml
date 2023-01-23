let colors =
  [ ("white", 0); ("red", 31); ("blue", 34); ("gray", 90); ("green", 92) ]

let set_color color = Printf.sprintf "\027[%dm" (List.assoc color colors)

let print_color_str color s =
  Printf.printf "%s%s%s\n" (set_color color) s (set_color "white")